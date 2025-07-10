#[cfg(test)]
pub mod helpers {
    use serde_json::to_value;

    use std::{
        collections::{BTreeSet, HashMap, HashSet},
        path::PathBuf,
        str::FromStr,
        sync::{Arc, LazyLock},
    };
    use tower::{Service, ServiceExt};
    use tree_sitter::Parser;

    use dashmap::DashMap;
    use tower_lsp::{
        LspService,
        jsonrpc::{Request, Response},
        lsp_types::{
            ClientCapabilities, DidOpenTextDocumentParams, InitializeParams, Position, Range,
            TextDocumentContentChangeEvent, TextDocumentItem, TextEdit, Url,
            notification::DidOpenTextDocument, request::Initialize,
        },
    };

    use crate::{Backend, LanguageData, Options, QUERY_LANGUAGE, SymbolInfo};

    pub static TEST_URI: LazyLock<Url> =
        LazyLock::new(|| Url::parse("file:///tmp/queries/js/test.scm").unwrap());
    pub static TEST_URI_2: LazyLock<Url> =
        LazyLock::new(|| Url::parse("file:///tmp/queries/css/test.scm").unwrap());
    pub const SIMPLE_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/queries/example_test_files/simple.scm"
    ));
    pub const COMPLEX_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/queries/example_test_files/complex.scm"
    ));

    /// Always test with id of 1 for simplicity
    const ID: i64 = 1;

    /// A tuple holding the document's URI and source text.
    pub type Document<'a> = (Url, &'a str);

    /// A tuple holding the language's name, symbols, fields, and supertype names.
    pub type TestLanguage<'a> = (String, Vec<SymbolInfo>, Vec<&'a str>, Vec<&'a str>);

    /// Initialize a test server, populating it with fake documents denoted by (uri, text, symbols, fields) tuples.
    pub async fn initialize_server(
        documents: &[Document<'_>],
        languages: &[TestLanguage<'_>],
        options: &Options,
    ) -> LspService<Backend> {
        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error loading Query grammar");
        let options_value = serde_json::to_value(options).unwrap();
        let options = &serde_json::from_value::<Options>(options_value.clone()).unwrap();
        let arced_options = Arc::new(tokio::sync::RwLock::new(options.clone()));
        let workspace_dirs = vec![
            PathBuf::from_str(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/queries/test_workspace/"
            ))
            .unwrap(),
        ];
        let (mut service, _socket) = LspService::build(|client| Backend {
            _client: client,
            document_map: Default::default(),
            language_map: DashMap::from_iter(languages.iter().cloned().map(
                |(name, symbols, fields, supertypes)| {
                    (
                        name.clone(),
                        Arc::new(LanguageData {
                            name,
                            symbols_set: HashSet::from_iter(symbols.iter().cloned()),
                            symbols_vec: symbols.to_vec(),
                            fields_set: HashSet::from_iter(fields.iter().map(ToString::to_string)),
                            fields_vec: fields.iter().map(ToString::to_string).collect(),
                            supertype_map: HashMap::from_iter(supertypes.iter().map(|st| {
                                (
                                    SymbolInfo {
                                        label: st.to_string(),
                                        named: true,
                                    },
                                    BTreeSet::from([
                                        SymbolInfo {
                                            label: "test".to_string(),
                                            named: true,
                                        },
                                        SymbolInfo {
                                            label: "test2".to_string(),
                                            named: true,
                                        },
                                    ]),
                                )
                            })),
                            language: None,
                        }),
                    )
                },
            )),
            workspace_uris: Arc::new(workspace_dirs.into()),
            options: arced_options,
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
                    initialization_options: Some(options_value),
                    ..Default::default()
                },
            ))
            .await
            .unwrap()
            .unwrap();

        // Open the documents
        for (uri, src) in documents.iter().cloned() {
            service
                .ready()
                .await
                .unwrap()
                .call(lsp_notification_to_jsonrpc_request::<DidOpenTextDocument>(
                    DidOpenTextDocumentParams {
                        text_document: TextDocumentItem {
                            version: 0,
                            language_id: String::from("query"),
                            text: src.to_string(),
                            uri,
                        },
                    },
                ))
                .await
                .unwrap();
        }

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
    use std::{
        collections::{BTreeMap, HashMap},
        ops::Deref,
    };
    use ts_query_ls::Options;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::{
        SymbolInfo,
        test_helpers::helpers::{
            COMPLEX_FILE, SIMPLE_FILE, TEST_URI, TEST_URI_2, initialize_server,
        },
    };

    use super::helpers::{Document, TestLanguage};

    #[rstest]
    #[case(&[], &[], &Default::default())]
    #[case(&[(
            TEST_URI.clone(),
            SIMPLE_FILE,
        ),
        (
            TEST_URI_2.clone(),
            COMPLEX_FILE,
        )],
        &[
            (
                String::from("css"),
                vec![
                    SymbolInfo { named: true, label: String::from("identifier") },
                    SymbolInfo { named: false, label: String::from(";") },
                ],
                vec!["operator", "content"],
                vec!["type"]
            )
        ],
        &Options {
            valid_captures: HashMap::from([(String::from("test"), BTreeMap::from([(String::from("variable"), String::from("A common variable"))]))]),
            ..Default::default()
        }
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn initialize_server_helper(
        #[case] documents: &[Document<'_>],
        #[case] languages: &[TestLanguage<'_>],
        #[case] options: &Options,
    ) {
        // Act
        let service = initialize_server(documents, languages, options).await;

        // Assert
        let backend = service.inner();
        // Serialize and re-serialize to populate the required fields that are added at
        // deserialization time (default language retrieval regexes, `not-` predicates)
        let options =
            &serde_json::from_value::<Options>(serde_json::to_value(options).unwrap()).unwrap();

        let actual_options = backend.options.read().await;
        assert_eq!(actual_options.deref(), options);
        assert_eq!(backend.document_map.len(), documents.len());
        for (uri, source) in documents {
            let doc = backend.document_map.get(uri).unwrap();
            assert_eq!(doc.rope.to_string(), (*source).to_string());
            assert_eq!(
                doc.tree
                    .root_node()
                    .utf8_text((*source).to_string().as_bytes())
                    .unwrap(),
                (*source).to_string()
            );
        }
        for (language_name, symbols, fields, supertypes) in languages {
            let language_data = backend.language_map.get(language_name).unwrap();
            assert!(language_data.symbols_vec.len() == symbols.len());
            assert!(language_data.symbols_set.len() == symbols.len());
            for symbol in symbols {
                assert!(language_data.symbols_vec.contains(symbol));
                assert!(language_data.symbols_set.contains(symbol));
            }
            assert!(language_data.fields_vec.len() == fields.len());
            assert!(language_data.fields_set.len() == fields.len());
            for field in fields {
                assert!(language_data.fields_vec.contains(&field.to_string()));
                assert!(language_data.fields_set.contains(*field));
            }
            for supertype in supertypes {
                assert!(language_data.supertype_map.contains_key(&SymbolInfo {
                    named: true,
                    label: String::from(*supertype)
                }))
            }
        }
    }
}
