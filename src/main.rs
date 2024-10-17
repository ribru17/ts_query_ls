use std::{borrow::Cow, cmp::Ordering, collections::HashSet, env::set_current_dir, path::PathBuf};
use streaming_iterator::StreamingIterator;

use dashmap::DashMap;
use regex::Regex;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::*,
    Client, LanguageServer, LspService, Server,
};
use tree_sitter::{InputEdit, Parser, Query, QueryCursor, Tree};
use util::{
    byte_offset_to_position, get_current_capture_node, get_references, lsp_position_to_ts_point,
    node_is_or_has_ancestor, position_to_byte_offset,
};

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<Url, Rope>,
    ast_map: DashMap<Url, Tree>,
}

mod util;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls initialize: {:?}", params),
            )
            .await;
        if let Some(root_uri) = params.root_uri {
            let root = PathBuf::from(root_uri.path());
            if set_current_dir(&root).is_err() {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to set root directory to {:?}", root),
                    )
                    .await;
            };
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["@".to_string()]),
                    ..CompletionOptions::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<()> {
        self.client
            .log_message(MessageType::LOG, "ts_query_ls shutdown".to_string())
            .await;
        Ok(())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls goto_definition: {:?}", params),
            )
            .await;
        let uri = params.text_document_position_params.text_document.uri;
        let pos = Position {
            line: params.text_document_position_params.position.line,
            character: 0,
        };
        let range = Range::new(pos, pos);
        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri,
            range,
        })))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls did_open: {:?}", params),
            )
            .await;
        let rope = Rope::from_str(&params.text_document.text);
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_query::language())
            .expect("Error loading Query grammar");
        self.document_map
            .insert(params.text_document.uri.clone(), rope.clone());
        self.ast_map.insert(
            params.text_document.uri,
            parser.parse(params.text_document.text, None).unwrap(),
        );
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls did_change: {:?}", params),
            )
            .await;
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_query::language())
            .expect("Error loading Query grammar");
        // FIXME: Completions sometimes become all garbled. Is this because the text documents are
        // not synced?
        let mut old_rope: Option<Rope> = None;
        self.document_map
            .alter(&params.text_document.uri, |_, mut rope| {
                old_rope = Some(rope.clone());
                for change in &params.content_changes {
                    if let Some(range) = change.range {
                        let rope_range = util::lsp_range_to_rope_range(range, &rope).unwrap();
                        rope.remove(rope_range.clone());
                        rope.insert(rope_range.start, &change.text);
                    } else {
                        rope = Rope::from_str(&change.text);
                    }
                }
                rope
            });
        let rope = &self.document_map.get(&params.text_document.uri).unwrap();
        self.ast_map
            .alter(&params.text_document.uri, |_, mut tree| {
                for change in &params.content_changes {
                    if let Some(range) = change.range {
                        let start = position_to_byte_offset(range.start, rope).unwrap();
                        let end =
                            position_to_byte_offset(range.end, &old_rope.clone().unwrap()).unwrap();
                        let start_position = lsp_position_to_ts_point(range.start);
                        let old_end_position = lsp_position_to_ts_point(range.end);
                        let len_new = change.text.len();
                        let new_end_offset = start + len_new;
                        let new_end_position =
                            byte_offset_to_position(new_end_offset, rope).unwrap();
                        let new_end_position = lsp_position_to_ts_point(new_end_position);
                        tree.edit(&InputEdit {
                            start_byte: start,
                            old_end_byte: end,
                            new_end_byte: new_end_offset,
                            start_position,
                            old_end_position,
                            new_end_position,
                        });
                        tree = parser
                            .parse(rope.slice(..).to_string(), Some(&tree))
                            .unwrap();
                    } else {
                        tree = parser.parse(&change.text, None).unwrap();
                    }
                }
                tree
            });
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;

        let Some(tree) = self.ast_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No AST built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let Some(rope) = self.document_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No document built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let cur_pos = lsp_position_to_ts_point(params.text_document_position.position);
        let current_node = match get_current_capture_node(tree.root_node(), cur_pos) {
            None => return Ok(None),
            Some(value) => value,
        };

        let language = tree_sitter_query::language();
        let query = Query::new(&language, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let contents = Cow::from(rope.clone());
        let contents = contents.as_bytes();

        let mut parser = Parser::new();
        parser
            .set_language(&language)
            .expect("Error setting language for Query parser");

        Ok(Some(
            get_references(
                uri,
                &tree.root_node(),
                &current_node,
                &query,
                &mut cursor,
                contents,
            )
            .collect(),
        ))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let Some(tree) = self.ast_map.get(&uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No AST built for URI: {:?}", uri),
                )
                .await;
            return Ok(None);
        };
        let Some(rope) = self.document_map.get(&uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No document built for URI: {:?}", uri),
                )
                .await;
            return Ok(None);
        };
        let contents = Cow::from(rope.clone());
        let contents = contents.as_bytes();
        let current_node = match get_current_capture_node(
            tree.root_node(),
            lsp_position_to_ts_point(params.text_document_position.position),
        ) {
            None => return Ok(None),
            Some(value) => value,
        };
        let language = tree_sitter_query::language();
        let query = Query::new(&language, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let new_name = params.new_name;
        let identifier_pattern = Regex::new("^[a-zA-Z0-9.\\-_\\$]+$").unwrap();
        if !identifier_pattern.is_match(new_name.as_str()) {
            return Err(jsonrpc::Error::invalid_params(
                "New name is not a valid identifier",
            ));
        }
        let mut text_document_edits: Vec<TextDocumentEdit> = vec![];
        get_references(
            &uri,
            &tree.root_node(),
            &current_node,
            &query,
            &mut cursor,
            contents,
        )
        .for_each(|mut elem| {
            // Don't include the preceding `@`
            elem.range.start.character += 1;
            text_document_edits.push(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier {
                    uri: elem.uri,
                    // TODO: Support versioned edits
                    version: None,
                },
                edits: vec![OneOf::Left(TextEdit {
                    range: elem.range,
                    new_text: new_name.clone(),
                })],
            });
        });
        // Apply edits from end to start, to prevent offset inaccuracies
        text_document_edits.sort_by(|a, b| {
            if let OneOf::Left(a) = &a.edits[0] {
                if let OneOf::Left(b) = &b.edits[0] {
                    let range_a = a.range;
                    let range_b = b.range;
                    range_b.start.cmp(&range_a.start)
                } else {
                    Ordering::Equal
                }
            } else {
                Ordering::Equal
            }
        });

        Ok(Some(WorkspaceEdit {
            document_changes: Some(DocumentChanges::Edits(text_document_edits)),
            changes: None,
            change_annotations: None,
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;

        let Some(tree) = self.ast_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No AST built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let Some(rope) = self.document_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No document built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let point = lsp_position_to_ts_point(params.text_document_position.position);
        let language = tree_sitter_query::language();
        let query = Query::new(&language, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let contents = Cow::from(rope.slice(..));
        let contents = contents.as_bytes();
        let current_node = tree
            .root_node()
            .named_descendant_for_point_range(point, point)
            .unwrap();
        if !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate") {
            return Ok(None);
        }
        let node = match tree.root_node().child_with_descendant(current_node) {
            None => return Ok(None),
            Some(value) => value,
        };

        let mut completion_items = vec![];

        let mut iter = cursor.matches(&query, node, contents);
        let mut seen = HashSet::new();
        while let Some(match_) = iter.next() {
            for capture in match_.captures {
                let node_text = capture.node.utf8_text(contents).unwrap();
                let parent_params = match capture.node.parent() {
                    None => true,
                    Some(value) => value.grammar_name() != "parameters",
                };
                if
                // !node_contains(capture.node, point) &&
                parent_params && !seen.contains(node_text) {
                    seen.insert(node_text);
                    completion_items.push(CompletionItem {
                        label: node_text.to_string(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        ..Default::default()
                    });
                }
            }
        }

        Ok(Some(CompletionResponse::Array(completion_items)))
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
        ast_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
