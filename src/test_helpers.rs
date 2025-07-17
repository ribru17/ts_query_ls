#[cfg(test)]
pub mod helpers {
    use serde_json::to_value;

    use std::sync::LazyLock;
    use tower::{Service, ServiceExt};

    use tower_lsp::{
        LspService,
        jsonrpc::{Request, Response},
        lsp_types::{
            ClientCapabilities, DiagnosticClientCapabilities, DidOpenTextDocumentParams,
            InitializeParams, Position, Range, TextDocumentClientCapabilities,
            TextDocumentContentChangeEvent, TextDocumentItem, TextEdit, Url, WorkspaceFolder,
            notification::DidOpenTextDocument, request::Initialize,
        },
    };

    use crate::{Backend, Options};

    pub static TEST_URI: LazyLock<Url> =
        LazyLock::new(|| Url::parse("file:///tmp/queries/js/test.scm").unwrap());
    pub static QUERY_TEST_URI: LazyLock<Url> =
        LazyLock::new(|| Url::parse("file:///tmp/queries/query/test.scm").unwrap());
    pub const SIMPLE_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/queries/example_test_files/simple.scm"
    ));
    pub const COMPLEX_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/queries/example_test_files/complex.scm"
    ));

    pub static TEST_CLIENT_CAPABILITIES: LazyLock<ClientCapabilities> =
        LazyLock::new(|| ClientCapabilities {
            text_document: Some(TextDocumentClientCapabilities {
                diagnostic: Some(DiagnosticClientCapabilities {
                    dynamic_registration: Some(false),
                    related_document_support: Some(true),
                }),
                ..Default::default()
            }),
            ..Default::default()
        });

    /// Always test with id of 1 for simplicity
    const ID: i64 = 1;

    /// A tuple holding the document's URI and source text.
    pub type Document<'a> = (Url, &'a str);

    /// Initialize a test server, populating it with fake documents denoted by (uri, text, symbols, fields) tuples.
    pub async fn initialize_server(
        documents: &[Document<'_>],
        options: &Options,
    ) -> LspService<Backend> {
        let options_value = serde_json::to_value(options).unwrap();
        let (mut service, _socket) = LspService::build(|client| Backend {
            client,
            client_capabilities: Default::default(),
            document_map: Default::default(),
            language_map: Default::default(),
            workspace_uris: Default::default(),
            options: Default::default(),
        })
        .finish();

        // Initialize the server
        service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Initialize>(
                InitializeParams {
                    capabilities: TEST_CLIENT_CAPABILITIES.clone(),
                    workspace_folders: Some(vec![WorkspaceFolder {
                        name: String::from("test_workspace"),
                        uri: Url::from_file_path(concat!(
                            env!("CARGO_MANIFEST_DIR"),
                            "/queries/test_workspace/"
                        ))
                        .unwrap(),
                    }]),
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
    use tower_lsp::lsp_types::Url;
    use ts_query_ls::Options;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::test_helpers::helpers::{
        COMPLEX_FILE, SIMPLE_FILE, TEST_CLIENT_CAPABILITIES, TEST_URI, initialize_server,
    };

    use super::helpers::Document;

    #[rstest]
    #[case(&[], &Default::default())]
    #[case(&[(
            TEST_URI.clone(),
            SIMPLE_FILE,
        ),
        (
            Url::parse("file:///tmp/queries/css/test.scm").unwrap(),
            COMPLEX_FILE,
        )],
        &Options {
            valid_captures: HashMap::from([(String::from("test"), BTreeMap::from([(String::from("variable"), String::from("A common variable"))]))]),
            ..Default::default()
        }
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn initialize_server_helper(
        #[case] documents: &[Document<'_>],
        #[case] options: &Options,
    ) {
        // Act
        let service = initialize_server(documents, options).await;

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
        assert_eq!(
            backend.client_capabilities.deref().read().await.deref(),
            TEST_CLIENT_CAPABILITIES.deref()
        );
    }
}
