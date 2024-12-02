#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use ropey::Rope;
    use rstest::rstest;
    use serde_json::to_value;

    use lazy_static::lazy_static;
    use std::{
        collections::{BTreeMap, HashSet},
        sync::Arc,
    };
    use tree_sitter::Parser;

    use tower::{Service, ServiceExt};

    use dashmap::DashMap;
    use std::sync::RwLock;
    use tower_lsp::{
        jsonrpc::{Request, Response},
        lsp_types::{
            notification::{DidChangeConfiguration, DidChangeTextDocument, DidOpenTextDocument},
            request::{Completion, Formatting, Initialize, References, Rename},
            ClientCapabilities, CompletionItemKind, CompletionParams, CompletionResponse,
            DidChangeConfigurationParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
            DocumentChanges, DocumentFormattingParams, FormattingOptions, InitializeParams,
            InitializeResult, Location, OneOf, OptionalVersionedTextDocumentIdentifier,
            PartialResultParams, Position, Range, ReferenceContext, ReferenceParams, RenameParams,
            TextDocumentContentChangeEvent, TextDocumentEdit, TextDocumentIdentifier,
            TextDocumentItem, TextDocumentPositionParams, TextEdit, Url,
            VersionedTextDocumentIdentifier, WorkDoneProgressParams, WorkspaceEdit,
        },
        LspService,
    };

    use crate::{Backend, Options, SymbolInfo, QUERY_LANGUAGE, SERVER_CAPABILITIES};

    lazy_static! {
        static ref TEST_URI: Url = Url::parse("file:///tmp/test.scm").unwrap();
        static ref TEST_URI_2: Url = Url::parse("file:///tmp/injections.scm").unwrap();
        static ref SIMPLE_FILE: &'static str = r"((identifier) @constant
 (#match? @constant @constant))
 ; @constant here";
        static ref COMPLEX_FILE: &'static str = r#"((comment) @injection.content
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

    type Coordinate = ((u32, u32), (u32, u32));

    // Always test with id of 1 for simplicity
    const ID: i64 = 1;

    // An equivalent function is provided but it is private
    fn lsp_request_to_jsonrpc_request<R>(params: R::Params) -> Request
    where
        R: tower_lsp::lsp_types::request::Request,
    {
        Request::build(R::METHOD)
            .id(ID)
            .params(to_value(params).unwrap())
            .finish()
    }

    fn lsp_notification_to_jsonrpc_request<R>(params: R::Params) -> Request
    where
        R: tower_lsp::lsp_types::notification::Notification,
    {
        Request::build(R::METHOD)
            .params(to_value(params).unwrap())
            .finish()
    }

    fn lsp_response_to_jsonrpc_response<R>(params: R::Result) -> Response
    where
        R: tower_lsp::lsp_types::request::Request,
    {
        Response::from_ok(ID.into(), to_value(params).unwrap())
    }

    #[derive(Debug, Clone)]
    struct TestEdit {
        pub text: String,
        pub range: Range,
    }

    impl TestEdit {
        fn new(text: &str, start: (u32, u32), end: (u32, u32)) -> Self {
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

    /// Initialize a test server, populating it with fake documents denoted by (uri, text, symbols, fields) tuples.
    async fn initialize_server(
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

    #[rstest]
    #[case(&[])]
    #[case(&[(
        TEST_URI.clone(),
        SIMPLE_FILE.clone(),
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
    )])]
    #[tokio::test(flavor = "current_thread")]
    async fn test_server_initialize(#[case] documents: &[(Url, &str, Vec<SymbolInfo>, Vec<&str>)]) {
        // Act
        let (service, response) = initialize_server(documents).await;

        // Assert
        let backend = service.inner();
        assert_eq!(
            response,
            lsp_response_to_jsonrpc_response::<Initialize>(InitializeResult {
                capabilities: SERVER_CAPABILITIES.clone(),
                ..Default::default()
            })
        );
        assert_eq!(backend.document_map.len(), documents.len());
        assert_eq!(backend.cst_map.len(), documents.len());
        assert_eq!(backend.symbols_vec_map.len(), documents.len());
        assert_eq!(backend.symbols_set_map.len(), documents.len());
        assert_eq!(backend.fields_vec_map.len(), documents.len());
        assert_eq!(backend.fields_set_map.len(), documents.len());
        for (uri, doc, symbols, fields) in documents {
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
            assert!(backend
                .symbols_vec_map
                .get(uri)
                .is_some_and(|v| v.len() == symbols.len()));
            assert!(backend
                .symbols_set_map
                .get(uri)
                .is_some_and(|v| v.len() == symbols.len()));
            for symbol in symbols {
                assert!(backend.symbols_vec_map.get(uri).unwrap().contains(symbol));
                assert!(backend.symbols_set_map.get(uri).unwrap().contains(symbol));
            }
            assert!(backend
                .fields_vec_map
                .get(uri)
                .is_some_and(|v| v.len() == fields.len()));
            assert!(backend
                .fields_set_map
                .get(uri)
                .is_some_and(|v| v.len() == fields.len()));
            for field in fields {
                assert!(backend
                    .fields_vec_map
                    .get(uri)
                    .unwrap()
                    .contains(&field.to_string()));
                assert!(backend
                    .fields_set_map
                    .get(uri)
                    .unwrap()
                    .contains(&field.to_string()));
            }
        }
    }

    #[tokio::test(flavor = "current_thread")]
    async fn test_server_did_open_document() {
        // Arrange
        let mut service = initialize_server(&[]).await.0;
        let source = r#""[" @cap"#;

        // Act
        service
            .ready()
            .await
            .unwrap()
            .call(lsp_notification_to_jsonrpc_request::<DidOpenTextDocument>(
                DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: TEST_URI.clone(),
                        language_id: String::from("query"),
                        version: 0,
                        text: String::from(source),
                    },
                },
            ))
            .await
            .unwrap();

        // Assert
        let doc_rope = service.inner().document_map.get(&TEST_URI);
        assert!(doc_rope.is_some());
        let doc_rope = doc_rope.unwrap();
        assert_eq!(doc_rope.to_string(), source);
        let tree = service.inner().cst_map.get(&TEST_URI);
        assert!(tree.is_some());
        let tree = tree.unwrap();
        assert_eq!(
            tree.root_node().utf8_text(source.as_bytes()).unwrap(),
            doc_rope.to_string()
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn test_server_did_change_configuration() {
        // Arrange
        let mut service = initialize_server(&[]).await.0;

        // Act
        service
            .ready()
            .await
            .unwrap()
            .call(
                lsp_notification_to_jsonrpc_request::<DidChangeConfiguration>(
                    DidChangeConfigurationParams {
                        settings: serde_json::from_str(
                            r#"
                    {
                      "parser_aliases": {
                        "ecma": "javascript",
                        "jsx": "javascript",
                        "foolang": "barlang"
                      }
                    }
                    "#,
                        )
                        .unwrap(),
                    },
                ),
            )
            .await
            .unwrap();

        // Assert
        let options = service.inner().options.read();
        assert!(options.is_ok());
        let options = options.unwrap();
        assert_eq!(
            *options,
            Options {
                parser_aliases: Some(BTreeMap::from([
                    ("ecma".to_string(), "javascript".to_string()),
                    ("jsx".to_string(), "javascript".to_string()),
                    ("foolang".to_string(), "barlang".to_string())
                ])),
                parser_install_directories: None,
                language_retrieval_patterns: None
            }
        );
    }

    #[rstest]
    #[case(
        r#"(node_name) @hello
";" @semicolon"#,
        r#"(identifier) @goodbye
";" @punctuation.delimiter"#,
        &[
            TestEdit::new("goodbye", (0, 13), (0, 18)),
            TestEdit::new("identifier", (0, 1), (0, 10)),
            TestEdit::new("punctuation.delimiter", (1, 5), (1, 14)),
        ]
    )]
    #[case(
        r#"; Some comment with emojis 🚀🛳️🫡
(node_name) @hello
";" @semicolon"#,
        r#"; Some comment with emojis 🚀🛳️🫡
(identifier) @goodbye
";" @punctuation.delimiter"#,
        &[
            TestEdit::new("goodbye", (1, 13), (1, 18)),
            TestEdit::new("identifier", (1, 1), (1, 10)),
            TestEdit::new("punctuation.delimiter", (2, 5), (2, 14)),
        ]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn test_server_did_change(
        #[case] original: &str,
        #[case] expected: &str,
        #[case] edits: &[TestEdit],
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), original, Vec::new(), Vec::new())])
                .await
                .0;

        // Act
        service
            .ready()
            .await
            .unwrap()
            .call(
                lsp_notification_to_jsonrpc_request::<DidChangeTextDocument>(
                    DidChangeTextDocumentParams {
                        text_document: VersionedTextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                            version: 1,
                        },
                        content_changes: edits
                            .iter()
                            .map(Into::<TextDocumentContentChangeEvent>::into)
                            .collect(),
                    },
                ),
            )
            .await
            .unwrap();

        // Assert
        let doc = service.inner().document_map.get(&TEST_URI);
        let tree = service.inner().cst_map.get(&TEST_URI);
        assert!(doc.is_some());
        let doc = doc.unwrap();
        assert_eq!(doc.to_string(), expected);
        assert!(tree.is_some());
        let tree = tree.unwrap();
        assert_eq!(
            tree.root_node().utf8_text(expected.as_bytes()).unwrap(),
            expected
        );
    }

    #[rstest]
    #[case(
        "(identifier) @variable",
        Position { line: 0, character: 17 },
        &[((0, 13), (0, 22))]
    )]
    #[case(
        r#"((identifier) @constant
(#match? @constant "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 0, character: 17 },
        &[((0, 14), (0, 23)), ((1, 9), (1, 18))]
    )]
    #[case(
        r"(type_definition declarator: (type_identifier) @name) @definition.type",
        Position { line: 0, character: 61 },
        &[((0, 54), (0, 70))]
    )]
    #[case(
        r"(call_expression
function: (identifier) @function)",
        Position { line: 0, character: 1 },
        &[]
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 5, character: 25 },
        &[((5, 25), (5, 44)), ((11, 15), (11, 34)), ((17, 16), (17, 35))]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn test_capture_references(
        #[case] input: &str,
        #[case] position: Position,
        #[case] ranges: &[Coordinate],
    ) {
        // Arrange
        let mut service = initialize_server(&[(TEST_URI.clone(), input, Vec::new(), Vec::new())])
            .await
            .0;

        // Act
        let refs = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<References>(
                ReferenceParams {
                    context: ReferenceContext {
                        include_declaration: true,
                    },
                    partial_result_params: PartialResultParams {
                        partial_result_token: None,
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    text_document_position: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                        },
                        position,
                    },
                },
            ))
            .await
            .unwrap();

        // Assert
        let actual = if ranges.is_empty() {
            None
        } else {
            Some(
                ranges
                    .iter()
                    .map(|r| Location {
                        uri: TEST_URI.clone(),
                        range: Range {
                            start: Position {
                                line: r.0 .0,
                                character: r.0 .1,
                            },
                            end: Position {
                                line: r.1 .0,
                                character: r.1 .1,
                            },
                        },
                    })
                    .collect(),
            )
        };
        assert_eq!(
            refs,
            Some(lsp_response_to_jsonrpc_response::<References>(actual))
        );
    }

    #[rstest]
    #[case(
        &SIMPLE_FILE,
        Position { line: 1, character: 12, },
        &[
            TestEdit::new("superlongnamehere", (1, 21), (1, 29)),
            TestEdit::new("superlongnamehere", (1, 11), (1, 19)),
            TestEdit::new("superlongnamehere", (0, 15), (0, 23)),
        ],
        "superlongnamehere",
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 8, character: 24 },
        &[
            TestEdit::new("invariant", (18, 17), (18, 34)),
            TestEdit::new("invariant", (12, 13), (12, 30)),
            TestEdit::new("invariant", (9, 23), (9, 40)),
            TestEdit::new("invariant", (8, 25), (8, 42)),
        ],
        "invariant"
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 8, character: 23 },
        // Doesn't rename when cursor is not in capture
        &[],
        "invariant"
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn test_server_rename(
        #[case] original: &str,
        #[case] cursor_position: Position,
        #[case] edits: &[TestEdit],
        #[case] new_name: &str,
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), original, Vec::new(), Vec::new())])
                .await
                .0;

        // Act
        let rename_edits = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Rename>(RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                    },
                    position: cursor_position,
                },
                new_name: new_name.to_string(),
                work_done_progress_params: WorkDoneProgressParams {
                    work_done_token: None,
                },
            }))
            .await
            .map_err(|e| format!("textDocument/rename call returned error: {e}"))
            .unwrap();

        // Assert
        let ws_edit = if edits.is_empty() {
            None
        } else {
            Some(WorkspaceEdit {
                document_changes: Some(DocumentChanges::Edits(
                    edits
                        .iter()
                        .map(|e| TextDocumentEdit {
                            text_document: OptionalVersionedTextDocumentIdentifier {
                                uri: TEST_URI.clone(),
                                version: None,
                            },
                            edits: vec![OneOf::Left(e.into())],
                        })
                        .collect(),
                )),
                ..Default::default()
            })
        };
        assert_eq!(
            rename_edits,
            Some(lsp_response_to_jsonrpc_response::<Rename>(ws_edit))
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn test_server_formatting() {
        // Arrange
        let mut service = initialize_server(&[(
            TEST_URI.clone(),
            r"(    node   
            )         @cap                 
;;;; comment     ",
            Vec::new(),
            Vec::new(),
        )])
        .await
        .0;

        // Act
        let delta = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Formatting>(
                DocumentFormattingParams {
                    text_document: TextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    options: FormattingOptions::default(),
                },
            ))
            .await
            .unwrap();
        let mut edits =
            serde_json::from_value::<Option<Vec<TextEdit>>>(delta.unwrap().into_parts().1.unwrap());
        edits.as_mut().unwrap().as_mut().unwrap().sort_by(|a, b| {
            let range_a = a.range;
            let range_b = b.range;
            range_b.start.cmp(&range_a.start)
        });
        service
            .ready()
            .await
            .unwrap()
            .call(
                lsp_notification_to_jsonrpc_request::<DidChangeTextDocument>(
                    DidChangeTextDocumentParams {
                        text_document: VersionedTextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                            version: 1,
                        },
                        content_changes: edits
                            .unwrap()
                            .unwrap()
                            .iter()
                            .map(|e| TextDocumentContentChangeEvent {
                                range: Some(e.range),
                                text: e.new_text.clone(),
                                range_length: None,
                            })
                            .collect(),
                    },
                ),
            )
            .await
            .unwrap();

        // Assert
        let doc = service.inner().document_map.get(&TEST_URI).unwrap();
        assert_eq!(
            doc.to_string(),
            String::from(
                r"(node) @cap

; comment"
            )
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn it_provides_capture_completions() {
        let source = r#"((identifier) @constant
 (#match? @cons<CURSOR> "^[A-Z][A-Z\\d_]*$"))"#;
        let expected_comps = vec![expected_capture_completion("@constant")];
        test_server_completions(source, &expected_comps).await;
    }

    //    TODO: Requires server's `symbols_vec_map` to be populated
    //    #[tokio::test(flavor = "current_thread")]
    //    async fn it_provides_symbol_completions() {
    //        let source = r#"((ident<CURSOR>) @constant
    // (#match? @constant "^[A-Z][A-Z\\d_]*$"))"#;
    //        let expected_comps = vec![expected_named_symbol_completion("identifier")];
    //        test_server_completions(source, &expected_comps).await;
    //    }

    // TODO: Probably want to replicate this test to make sure other completions
    // aren't offered inside of comments, not just captures
    #[tokio::test(flavor = "current_thread")]
    async fn it_doesnt_provide_completions_inside_comments() {
        let source = r"((identifier) @constant
            ; @co<CURSOR>
            )
            ";
        let expected_comps = Vec::new();
        test_server_completions(source, &expected_comps).await;
    }

    // TODO: Other completion tests...

    fn expected_capture_completion(text: &str) -> (&str, Option<CompletionItemKind>) {
        (text, Some(CompletionItemKind::VARIABLE))
    }

    // fn expected_field_completion(text: &str) -> (&str, Option<CompletionItemKind>) {
    //     (text, Some(CompletionItemKind::FIELD))
    // }
    //
    // fn expected_named_symbol_completion(text: &str) -> (&str, Option<CompletionItemKind>) {
    //     (text, Some(CompletionItemKind::CLASS))
    // }
    //
    // fn expected_unnamed_symbol_completion(text: &str) -> (&str, Option<CompletionItemKind>) {
    //     (text, Some(CompletionItemKind::CONSTANT))
    // }

    async fn test_server_completions(
        source: &str,
        expected_completions: &[(&str, Option<CompletionItemKind>)],
    ) {
        // Arrange
        let mut cursor_position: Option<Position> = None;
        for (line_num, line) in source.lines().enumerate() {
            if let Some((cursor_idx, _)) = line.match_indices("<CURSOR>").next() {
                assert!(
                    cursor_position.is_none(),
                    "Only one <CURSOR> marker supported for a test input"
                );
                cursor_position = Some(Position {
                    line: line_num as u32,
                    character: cursor_idx as u32,
                });
            }
        }
        let cursor_position =
            cursor_position.expect("Expected one <CURSOR> marker in test input, found none");
        let cleaned_input = source.replace("<CURSOR>", "");

        let mut service =
            initialize_server(&[(TEST_URI.clone(), &cleaned_input, Vec::new(), Vec::new())])
                .await
                .0;
        let data = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Completion>(
                CompletionParams {
                    text_document_position: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                        },
                        position: cursor_position,
                    },
                    work_done_progress_params: WorkDoneProgressParams {
                        work_done_token: None,
                    },
                    partial_result_params: PartialResultParams {
                        partial_result_token: None,
                    },
                    context: None,
                },
            ))
            .await
            .map_err(|e| format!("textDocument/completion call returned error: {e}"))
            .unwrap();
        let completions = match serde_json::from_value::<Option<CompletionResponse>>(
            data.unwrap().into_parts().1.unwrap(),
        ) {
            Ok(Some(CompletionResponse::List(list))) => list.items,
            Ok(Some(CompletionResponse::Array(comps))) => comps,
            Ok(None) => Vec::new(),
            other => panic!("textDocument/completion call returned unexpected response: {other:?}"),
        };

        let mut expected_comps = expected_completions.to_vec();
        'finder_loop: while !expected_comps.is_empty() {
            for comp in &completions {
                if let Some(idx) =
                    expected_comps
                        .iter()
                        .enumerate()
                        .find_map(|(i, (c_text, c_kind))| {
                            if comp.label.eq(c_text) && comp.kind.eq(c_kind) {
                                Some(i)
                            } else {
                                None
                            }
                        })
                {
                    expected_comps.remove(idx);
                    continue 'finder_loop;
                }
            }
            break;
        }

        // Assert
        assert!(expected_comps.is_empty(), "Failed to provide the following completions:\n{expected_comps:#?}\nProvided completions:\n{completions:#?}");
    }
}
