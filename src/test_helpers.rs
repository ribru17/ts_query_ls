#[cfg(test)]
pub mod helpers {
    use ropey::Rope;
    use serde_json::to_value;

    use lazy_static::lazy_static;
    use std::{collections::HashSet, sync::Arc};
    use tree_sitter::Parser;

    use tower::{Service, ServiceExt};

    use dashmap::DashMap;
    use std::sync::RwLock;
    use tower_lsp::{
        jsonrpc::{Request, Response},
        lsp_types::{
            request::Initialize, ClientCapabilities, InitializeParams, Position, Range,
            TextDocumentContentChangeEvent, TextEdit, Url,
        },
        LspService,
    };

    use crate::{Backend, Options, SymbolInfo, QUERY_LANGUAGE};

    lazy_static! {
        pub static ref TEST_URI: Url = Url::parse("file:///tmp/test.scm").unwrap();
        pub static ref TEST_URI_2: Url = Url::parse("file:///tmp/injections.scm").unwrap();
        pub static ref SIMPLE_FILE: &'static str = r"((identifier) @constant
 (#match? @constant @constant))
 ; @constant here";
        pub static ref COMPLEX_FILE: &'static str = r#"((comment) @injection.content
  (#set! injection.language "comment"))

; html(`...`), html`...`, sql(`...`), etc.
(call_expression
  function: (identifier) @injection.language
  arguments: [
    (arguments
      (template_string) @injection.content)
    (template_string) @injection.content
  ]
  (#lua-match? @injection.language "^[a-zA-Z][a-zA-Z0-9]*$")
  (#offset! @injection.content 0 1 0 -1)
  (#set! injection.include-children)
  ; Languages excluded from auto-injection due to special rules
  ; - svg uses the html parser
  ; - css uses the styled parser
  (#not-any-of? @injection.language "svg" "css")
  (#not-any-of? @injection.content "test"))

; svg`...` or svg(`...`)
(call_expression
  function: (identifier) @_name
  (#eq? @_name "svg")
  arguments: [
    (arguments
      (template_string) @injection.content)
    (template_string) @injection.content
  ]
  (#offset! @injection.content 0 1 0 -1)
  (#set! injection.include-children)
  (#set! injection.language "html"))"#;
    }

    // Always test with id of 1 for simplicity
    const ID: i64 = 1;

    /// Initialize a test server, populating it with fake documents denoted by (uri, text, symbols, fields) tuples.
    pub async fn initialize_server(
        documents: &[(Url, &str, Vec<SymbolInfo>, Vec<&str>)],
    ) -> (LspService<Backend>, Response) {
        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error loading Query grammar");
        let options = Arc::new(RwLock::new(Options {
            parser_install_directories: None,
            parser_aliases: None,
            language_retrieval_patterns: None,
        }));
        let (mut service, _socket) =
            LspService::build(|client| Backend {
                client,
                document_map: DashMap::from_iter(
                    documents
                        .iter()
                        .map(|(uri, source, _, _)| (uri.clone(), Rope::from(*source))),
                ),
                cst_map: DashMap::from_iter(documents.iter().map(|(uri, source, _, _)| {
                    (uri.clone(), parser.parse(*source, None).unwrap())
                })),
                symbols_set_map: DashMap::from_iter(documents.iter().map(
                    |(uri, _, symbols, _)| (uri.clone(), HashSet::from_iter(symbols.clone())),
                )),
                symbols_vec_map: DashMap::from_iter(
                    documents
                        .iter()
                        .map(|(uri, _, symbols, _)| (uri.clone(), symbols.clone())),
                ),
                fields_set_map: DashMap::from_iter(documents.iter().map(|(uri, _, _, fields)| {
                    (
                        uri.clone(),
                        HashSet::from_iter(fields.iter().map(ToString::to_string)),
                    )
                })),
                fields_vec_map: DashMap::from_iter(documents.iter().map(|(uri, _, _, fields)| {
                    (
                        uri.clone(),
                        fields.clone().iter().map(ToString::to_string).collect(),
                    )
                })),
                options,
            })
            .finish();

        let init_result = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Initialize>(
                InitializeParams {
                    capabilities: ClientCapabilities::default(),
                    root_uri: Some(Url::parse("file:///tmp/").unwrap()),
                    ..Default::default()
                },
            ))
            .await
            .unwrap()
            .unwrap();

        (service, init_result)
    }

    // An equivalent function is provided but it is private
    pub fn lsp_request_to_jsonrpc_request<R>(params: R::Params) -> Request
    where
        R: tower_lsp::lsp_types::request::Request,
    {
        Request::build(R::METHOD)
            .id(ID)
            .params(to_value(params).unwrap())
            .finish()
    }

    pub fn lsp_notification_to_jsonrpc_request<R>(params: R::Params) -> Request
    where
        R: tower_lsp::lsp_types::notification::Notification,
    {
        Request::build(R::METHOD)
            .params(to_value(params).unwrap())
            .finish()
    }

    pub fn lsp_response_to_jsonrpc_response<R>(params: R::Result) -> Response
    where
        R: tower_lsp::lsp_types::request::Request,
    {
        Response::from_ok(ID.into(), to_value(params).unwrap())
    }

    pub fn jsonrpc_response_to_lsp_value<R>(response: Response) -> R::Result
    where
        R: tower_lsp::lsp_types::request::Request,
    {
        serde_json::from_value::<R::Result>(response.result().unwrap().clone()).unwrap()
    }

    #[derive(Debug, Clone)]
    pub struct TestEdit {
        pub text: String,
        pub range: Range,
    }

    impl TestEdit {
        pub fn new(text: &str, start: (u32, u32), end: (u32, u32)) -> Self {
            Self {
                text: text.to_string(),
                range: Range {
                    start: Position {
                        line: start.0,
                        character: start.1,
                    },
                    end: Position {
                        line: end.0,
                        character: end.1,
                    },
                },
            }
        }
    }

    impl From<&TestEdit> for TextDocumentContentChangeEvent {
        fn from(val: &TestEdit) -> Self {
            Self {
                range: Some(val.range),
                range_length: None,
                text: val.text.clone(),
            }
        }
    }

    impl From<&TestEdit> for TextEdit {
        fn from(val: &TestEdit) -> Self {
            Self {
                range: val.range,
                new_text: val.text.clone(),
            }
        }
    }
}