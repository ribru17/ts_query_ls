#[cfg(test)]
pub mod helpers {
    use ropey::Rope;
    use serde_json::to_value;

    use std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        sync::{Arc, LazyLock},
    };
    use tower::{Service, ServiceExt};
    use tree_sitter::Parser;

    use dashmap::DashMap;
    use tower_lsp::{
        LspService,
        jsonrpc::{Request, Response},
        lsp_types::{
            ClientCapabilities, InitializeParams, Position, Range, TextDocumentContentChangeEvent,
            TextEdit, Url, request::Initialize,
        },
    };

    use crate::{Backend, Options, QUERY_LANGUAGE, SymbolInfo};

    pub static TEST_URI: LazyLock<Url> =
        LazyLock::new(|| Url::parse("file:///tmp/test.scm").unwrap());
    pub static TEST_URI_2: LazyLock<Url> =
        LazyLock::new(|| Url::parse("file:///tmp/injections/test.scm").unwrap());
    pub static SIMPLE_FILE: LazyLock<&str> = LazyLock::new(|| {
        r"((identifier) @constant
 (#match? @constant @constant))
 ; @constant here"
    });
    pub static COMPLEX_FILE: LazyLock<&str> = LazyLock::new(|| {
        r#"((comment) @injection.content
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
  (#set! injection.language "html"))"#
    });

    /// Always test with id of 1 for simplicity
    const ID: i64 = 1;

    /// A tuple holding the document's URI, source text, symbols, fields, supertypes, and allowable
    /// captures
    pub type Document<'a> = (Url, &'a str, Vec<SymbolInfo>, Vec<&'a str>, Vec<&'a str>);

    /// Initialize a test server, populating it with fake documents denoted by (uri, text, symbols, fields) tuples.
    pub async fn initialize_server(
        documents: &[Document<'_>],
        valid_captures: Option<BTreeMap<String, String>>,
    ) -> LspService<Backend> {
        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error loading Query grammar");
        let valid_captures = if let Some(caps) = valid_captures {
            HashMap::from([(
                String::from("test"),
                BTreeMap::from_iter(caps.iter().map(|(name, desc)| (name.clone(), desc.clone()))),
            )])
        } else {
            HashMap::new()
        };
        let options = Arc::new(tokio::sync::RwLock::new(Options {
            parser_install_directories: None,
            parser_aliases: None,
            language_retrieval_patterns: None,
            valid_captures,
        }));
        let (mut service, _socket) = LspService::build(|client| Backend {
            client,
            document_map: DashMap::from_iter(
                documents
                    .iter()
                    .map(|(uri, source, _, _, _)| (uri.clone(), Rope::from(*source))),
            ),
            cst_map: DashMap::from_iter(
                documents.iter().map(|(uri, source, _, _, _)| {
                    (uri.clone(), parser.parse(*source, None).unwrap())
                }),
            ),
            symbols_set_map: DashMap::from_iter(
                documents.iter().map(|(uri, _, symbols, _, _)| {
                    (uri.clone(), HashSet::from_iter(symbols.clone()))
                }),
            ),
            symbols_vec_map: DashMap::from_iter(
                documents
                    .iter()
                    .map(|(uri, _, symbols, _, _)| (uri.clone(), symbols.clone())),
            ),
            fields_set_map: DashMap::from_iter(documents.iter().map(|(uri, _, _, fields, _)| {
                (
                    uri.clone(),
                    HashSet::from_iter(fields.iter().map(ToString::to_string)),
                )
            })),
            fields_vec_map: DashMap::from_iter(documents.iter().map(|(uri, _, _, fields, _)| {
                (
                    uri.clone(),
                    fields.clone().iter().map(ToString::to_string).collect(),
                )
            })),
            supertype_map_map: DashMap::from_iter(documents.iter().map(
                |(uri, _, _, _, supertypes)| {
                    (
                        uri.clone(),
                        HashMap::from_iter(supertypes.iter().map(|supertype| {
                            (
                                SymbolInfo {
                                    named: true,
                                    label: String::from(*supertype),
                                },
                                BTreeSet::from([
                                    SymbolInfo {
                                        named: true,
                                        label: String::from("test"),
                                    },
                                    SymbolInfo {
                                        named: true,
                                        label: String::from("test2"),
                                    },
                                ]),
                            )
                        })),
                    )
                },
            )),
            workspace_uris: Default::default(),
            options,
        })
        .finish();

        service
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

        service
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

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::{
        SymbolInfo,
        test_helpers::helpers::{
            COMPLEX_FILE, SIMPLE_FILE, TEST_URI, TEST_URI_2, initialize_server,
        },
    };

    use super::helpers::Document;

    #[rstest]
    #[case(&[], None)]
    #[case(&[(
        TEST_URI.clone(),
        SIMPLE_FILE.clone(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    ), (
        TEST_URI_2.clone(),
        COMPLEX_FILE.clone(),
        vec![
            SymbolInfo { named: true, label: String::from("identifier") },
            SymbolInfo { named: false, label: String::from(";") }
        ],
        vec![
            "operator",
            "content",
        ],
        vec!["type"],
    )],
        Some(BTreeMap::from([(String::from("variable"), String::from("A common variable"))])),
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn initialize_server_helper(
        #[case] documents: &[Document<'_>],
        #[case] valid_captures: Option<BTreeMap<String, String>>,
    ) {
        // Act
        let service = initialize_server(documents, valid_captures.clone()).await;

        // Assert
        let backend = service.inner();
        assert_eq!(backend.document_map.len(), documents.len());
        assert_eq!(backend.cst_map.len(), documents.len());
        assert_eq!(backend.symbols_vec_map.len(), documents.len());
        assert_eq!(backend.symbols_set_map.len(), documents.len());
        assert_eq!(backend.fields_vec_map.len(), documents.len());
        assert_eq!(backend.fields_set_map.len(), documents.len());
        for (uri, doc, symbols, fields, supertypes) in documents {
            assert_eq!(
                backend.document_map.get(uri).unwrap().to_string(),
                (*doc).to_string()
            );
            assert_eq!(
                backend
                    .cst_map
                    .get(uri)
                    .unwrap()
                    .root_node()
                    .utf8_text((*doc).to_string().as_bytes())
                    .unwrap(),
                (*doc).to_string()
            );
            assert!(
                backend
                    .symbols_vec_map
                    .get(uri)
                    .is_some_and(|v| v.len() == symbols.len())
            );
            assert!(
                backend
                    .symbols_set_map
                    .get(uri)
                    .is_some_and(|v| v.len() == symbols.len())
            );
            for symbol in symbols {
                assert!(backend.symbols_vec_map.get(uri).unwrap().contains(symbol));
                assert!(backend.symbols_set_map.get(uri).unwrap().contains(symbol));
            }
            assert!(
                backend
                    .fields_vec_map
                    .get(uri)
                    .is_some_and(|v| v.len() == fields.len())
            );
            assert!(
                backend
                    .fields_set_map
                    .get(uri)
                    .is_some_and(|v| v.len() == fields.len())
            );
            for field in fields {
                assert!(
                    backend
                        .fields_vec_map
                        .get(uri)
                        .unwrap()
                        .contains(&field.to_string())
                );
                assert!(
                    backend
                        .fields_set_map
                        .get(uri)
                        .unwrap()
                        .contains(&field.to_string())
                );
            }
            assert!(backend.supertype_map_map.get(uri).is_some());
            for supertype in supertypes {
                assert!(
                    backend
                        .supertype_map_map
                        .get(uri)
                        .unwrap()
                        .contains_key(&SymbolInfo {
                            named: true,
                            label: String::from(*supertype)
                        })
                )
            }
            let options = backend.options.read().await;
            if let Some(ref valid_captures) = valid_captures {
                assert_eq!(
                    options.valid_captures.get("test"),
                    Some(BTreeMap::from_iter(
                        valid_captures
                            .iter()
                            .map(|(name, desc)| (name.clone(), desc.clone()))
                    ))
                    .as_ref()
                );
            } else {
                assert_eq!(options.valid_captures, Default::default());
            }
        }
    }
}
