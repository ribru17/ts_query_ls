use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::HashSet,
    env::set_current_dir,
    path::PathBuf,
    sync::{Arc, RwLock},
};
use streaming_iterator::StreamingIterator;

use dashmap::DashMap;
use regex::Regex;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::*,
    Client, LanguageServer, LspService, Server,
};
use tree_sitter::{wasmtime::Engine, Parser, Query, QueryCursor, Tree};
use util::{
    get_current_capture_node, get_language, get_references, lsp_position_to_ts_point,
    lsp_textdocchange_to_ts_inputedit, node_is_or_has_ancestor,
};

lazy_static! {
    static ref ENGINE: Engine = Engine::default();
}

struct Backend {
    client: Client,
    document_map: DashMap<Url, Rope>,
    ast_map: DashMap<Url, Tree>,
    options: Arc<RwLock<Options>>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Options {
    parser_install_directories: Vec<String>,
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
                    trigger_characters: Some(['@', '"', '\\', '('].map(|c| c.to_string()).into()),
                    ..CompletionOptions::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let changed_options =
            if let Ok(options) = serde_json::from_value::<Options>(params.settings) {
                options
            } else {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        "Unable to parse configuration settings!",
                    )
                    .await;
                return;
            };
        let mut options = self.options.write().unwrap();
        options.parser_install_directories = changed_options.parser_install_directories;
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
        let uri = &params.text_document.uri;
        let mut rope = self.document_map.get_mut(uri).unwrap();
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_query::language())
            .expect("Error loading Query grammar");

        let mut edits = vec![];
        for change in &params.content_changes {
            let text = change.text.as_str();
            let text_bytes = text.as_bytes();
            let text_end_byte_idx = text_bytes.len();

            let range = if let Some(range) = change.range {
                range
            } else {
                let start_line_idx = rope.byte_to_line(0);
                let end_line_idx = rope.byte_to_line(text_end_byte_idx);

                let start = Position::new(start_line_idx as u32, 0);
                let end = Position::new(end_line_idx as u32, 0);
                Range { start, end }
            };

            edits.push(lsp_textdocchange_to_ts_inputedit(&rope, change).unwrap());

            let start_row_char_idx = rope.line_to_char(range.start.line as usize);
            let start_row_cu = rope.char_to_utf16_cu(start_row_char_idx);
            let start_col_char_idx = rope
                .utf16_cu_to_char(start_row_cu + range.start.character as usize)
                - start_row_char_idx;
            let end_row_char_idx = rope.line_to_char(range.end.line as usize);
            let end_row_cu = rope.char_to_utf16_cu(end_row_char_idx);
            let end_col_char_idx =
                rope.utf16_cu_to_char(end_row_cu + range.end.character as usize) - end_row_char_idx;

            let start_char_idx = start_row_char_idx + start_col_char_idx;
            let end_char_idx = end_row_char_idx + end_col_char_idx;
            rope.remove(start_char_idx..end_char_idx);

            if !change.text.is_empty() {
                rope.insert(start_char_idx, text);
            }
        }
        let result = {
            let mut old_tree = self.ast_map.get_mut(uri).unwrap();

            for edit in edits {
                old_tree.edit(&edit);
            }

            parser.parse(rope.to_string(), Some(&old_tree))
        };

        if let Some(tree) = result {
            *self.ast_map.get_mut(uri).unwrap() = tree.clone();
        }
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
        let cur_pos = lsp_position_to_ts_point(params.text_document_position.position, &rope);
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
                &rope,
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
            lsp_position_to_ts_point(params.text_document_position.position, &rope),
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
            &rope,
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

        let point = lsp_position_to_ts_point(params.text_document_position.position, &rope);
        let language = tree_sitter_query::language();
        let query = Query::new(&language, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let contents = Cow::from(rope.slice(..));
        let contents = contents.as_bytes();
        let current_node = tree
            .root_node()
            .named_descendant_for_point_range(point, point)
            .unwrap();

        let mut completion_items = vec![];

        // Node name completions
        let path_type1 = Regex::new(r#"queries/([^/]+)/.*\.scm$"#).unwrap();
        let path_type2 = Regex::new(r#"tree-sitter-([^/]+)/queries/.*\.scm$"#).unwrap();
        let captures = path_type1
            .captures(uri.as_str())
            .or(path_type2.captures(uri.as_str()));
        let options = self.options.read().unwrap();
        let lang = captures
            .and_then(|captures| captures.get(1))
            .map(|cap| get_language(cap.as_str(), &options.parser_install_directories, &ENGINE));
        let mut seen = HashSet::new();
        if let Some(lang) = lang.flatten() {
            if !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate") {
                for i in 0..lang.node_kind_count() as u16 {
                    let in_anon =
                        node_is_or_has_ancestor(tree.root_node(), current_node, "anonymous_node");
                    let label = lang
                        .node_kind_for_id(i)
                        .unwrap()
                        .replace('"', r#"\""#)
                        .replace("\n", r#"\n"#);
                    if seen.contains(&label) || !lang.node_kind_is_visible(i) {
                        continue;
                    }
                    if (in_anon && !lang.node_kind_is_named(i))
                        || (!in_anon && lang.node_kind_is_named(i))
                    {
                        completion_items.push(CompletionItem {
                            label: label.clone(),
                            kind: Some(CompletionItemKind::CLASS),
                            ..Default::default()
                        });
                        seen.insert(label);
                    }
                }
            }
        }

        // Capture completions
        if !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate")
            || node_is_or_has_ancestor(tree.root_node(), current_node, "string")
        {
            return Ok(Some(CompletionResponse::Array(completion_items)));
        }
        let node = match tree.root_node().child_with_descendant(current_node) {
            None => return Ok(Some(CompletionResponse::Array(completion_items))),
            Some(value) => value,
        };

        let mut iter = cursor.matches(&query, node, contents);
        let mut seen = HashSet::new();
        while let Some(match_) = iter.next() {
            for capture in match_.captures {
                let node_text = capture.node.utf8_text(contents).unwrap();
                let parent_params = match capture.node.parent() {
                    None => true,
                    Some(value) => value.grammar_name() != "parameters",
                };
                if parent_params && !seen.contains(node_text) {
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

    let options = Arc::new(RwLock::new(Options {
        parser_install_directories: vec![],
    }));
    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
        ast_map: DashMap::new(),
        options,
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
