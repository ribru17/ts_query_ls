use std::{env::set_current_dir, path::PathBuf};

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};
use tree_sitter::{InputEdit, Parser, Tree};
use util::{byte_offset_to_position, lsp_position_to_ts_point, position_to_byte_offset};

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
                // hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
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
            })
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
