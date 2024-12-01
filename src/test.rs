#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use serde_json::to_value;

    use lazy_static::lazy_static;
    use ropey::Rope;
    use std::{collections::BTreeMap, sync::Arc};
    use tree_sitter::Parser;

    use tower::{Service, ServiceExt};

    use dashmap::DashMap;
    use std::sync::RwLock;
    use tower_lsp::{
        jsonrpc::{Request, Response},
        lsp_types::{
            notification::{DidChangeConfiguration, DidChangeTextDocument, DidOpenTextDocument},
            request::{Initialize, Rename},
            ClientCapabilities, DidChangeConfigurationParams, DidChangeTextDocumentParams,
            DidOpenTextDocumentParams, DocumentChanges, InitializeParams, InitializeResult, OneOf,
            OptionalVersionedTextDocumentIdentifier, PartialResultParams, Position, Range,
            ReferenceContext, ReferenceParams, RenameParams, TextDocumentContentChangeEvent,
            TextDocumentEdit, TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
            TextEdit, Url, VersionedTextDocumentIdentifier, WorkDoneProgressParams, WorkspaceEdit,
        },
        LanguageServer, LspService,
    };

    use crate::{Backend, Options, QUERY_LANGUAGE, SERVER_CAPABILITIES};

    lazy_static! {
        static ref TEST_URI: Url = Url::parse("file:///tmp/test.scm").unwrap();
        static ref SIMPLE_FILE: &'static str = r#"((identifier) @constant
 (#match? @constant @constant))
 ; @constant here"#;
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

    // An equivalent function is provided but it is private
    fn lsp_request_to_jsonrpc_request<R>(params: R::Params) -> Request
    where
        R: tower_lsp::lsp_types::request::Request,
    {
        Request::build(R::METHOD)
            // Always test with id of 1 for simplicity
            .id(1)
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
        Response::from_ok(1.into(), to_value(params).unwrap())
    }

    /// Initialize a test server, populating it with fake documents denoted by (uri, text) pairs.
    async fn initialize_server(documents: &[(Url, &str)]) -> LspService<Backend> {
        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error loading Query grammar");
        let options = Arc::new(RwLock::new(Options {
            parser_install_directories: None,
            parser_aliases: None,
            language_retrieval_patterns: None,
        }));
        let (mut service, _socket) = LspService::build(|client| Backend {
            client,
            document_map: DashMap::from_iter(
                documents
                    .iter()
                    .map(|(uri, source)| (uri.clone(), Rope::from(*source))),
            ),
            cst_map: DashMap::from_iter(
                documents
                    .iter()
                    .map(|(uri, source)| (uri.clone(), parser.parse(*source, None).unwrap())),
            ),
            symbols_set_map: DashMap::new(),
            symbols_vec_map: DashMap::new(),
            fields_set_map: DashMap::new(),
            fields_vec_map: DashMap::new(),
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
            .unwrap();

        service
    }

    async fn test_capture_references(input: &str) {
        let handle_cursor_marker =
            |input: &mut String,
             line_num: usize,
             idx: usize,
             cursor_position: &mut Option<Position>| {
                assert!(
                    cursor_position.is_none(),
                    "Only one cursor is supported inside text inputs"
                );
                *cursor_position = Some(Position {
                    line: line_num as u32,
                    character: idx as u32,
                });
                *input = input.replacen("<CURSOR>", "", 1);
            };
        let handle_ref_marker =
            |input: &mut String, line_num: usize, idx: usize, ref_positions: &mut Vec<Range>| {
                let line = input.lines().nth(line_num).unwrap();
                assert!(
                    line.chars().nth(idx + "<REF>".len()) == Some('@'),
                    "capture must immediately follow <REF> marker"
                );
                // We temporarily remove any cursor markers to cover the case in
                // which they share a capture with the current ref. This way, we get
                // an accurate end index for the word
                let counting_line = line.replace("<CURSOR>", "");
                let end = idx
                    + counting_line
                        .chars()
                        .enumerate()
                        .skip(idx + "<REF>".len() + 1) // skip past "<REF>@"
                        .take_while(|(_, c)| {
                            // NOTE: What is the actual legal character set?
                            c.is_alphanumeric() || c.eq(&'_') || c.eq(&'.')
                        })
                        .count()
                    + 1; // account for skipping past initial '@'

                ref_positions.push(Range::new(
                    Position {
                        line: line_num as u32,
                        character: idx as u32,
                    },
                    Position {
                        line: line_num as u32,
                        character: end as u32,
                    },
                ));
                *input = input.replacen("<REF>", "", 1);
            };

        let mut cursor_position: Option<Position> = None;
        let mut expected_refs: Vec<Range> = Vec::new();
        let mut cleaned_input = input.to_string();
        // Go through line by line, just pick out the earlier marker instance
        'finder_loop: loop {
            for (line_num, line) in cleaned_input.lines().enumerate() {
                let cursor_idx = line.match_indices("<CURSOR>").next();
                let ref_idx = line.match_indices("<REF>").next();
                match (cursor_idx, ref_idx) {
                    (Some((c_idx, _)), None) => {
                        handle_cursor_marker(
                            &mut cleaned_input,
                            line_num,
                            c_idx,
                            &mut cursor_position,
                        );
                        continue 'finder_loop;
                    }
                    (None, Some((r_idx, _))) => {
                        handle_ref_marker(&mut cleaned_input, line_num, r_idx, &mut expected_refs);
                        continue 'finder_loop;
                    }
                    (Some((c_idx, _)), Some((r_idx, _))) => {
                        if c_idx < r_idx {
                            handle_cursor_marker(
                                &mut cleaned_input,
                                line_num,
                                c_idx,
                                &mut cursor_position,
                            );
                        } else {
                            handle_ref_marker(
                                &mut cleaned_input,
                                line_num,
                                r_idx,
                                &mut expected_refs,
                            );
                        }
                        continue 'finder_loop;
                    }
                    (None, None) => {}
                }
            }
            break 'finder_loop;
        }

        let cursor_position =
            cursor_position.expect("textDocument/references test must contain one <CURSOR> marker");

        let server = initialize_server(&[(TEST_URI.clone(), cleaned_input.as_str())]).await;
        let found_refs = server
            .inner()
            .references(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                    },
                    position: cursor_position,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .await
            .map_err(|e| format!("textDocument/references call returned error: {e}"))
            .unwrap()
            .unwrap_or_default(); // Prefer an empty `Vec` over `None` so the missing
                                  // refs get printed in `panic_msg` below

        let mut panic_msg = String::new();
        for reference in found_refs {
            let Some(idx) = expected_refs
                .iter()
                .enumerate()
                .find(|(_, &r)| r == reference.range)
                .map(|(i, _)| i)
            else {
                let line = cleaned_input
                    .lines()
                    .nth(reference.range.start.line as usize)
                    .unwrap();
                panic_msg += &format!(
                    "Found unexpected reference at position ({}, {}), ({}, {}):\n{line}\n{}{}\n",
                    reference.range.start.line,
                    reference.range.start.character,
                    reference.range.end.line,
                    reference.range.end.character,
                    " ".repeat(reference.range.start.character as usize),
                    "^".repeat(
                        (reference.range.end.character - reference.range.start.character) as usize
                    ),
                );
                continue;
            };
            expected_refs.remove(idx);
        }

        for reference in expected_refs {
            let line = cleaned_input
                .lines()
                .nth(reference.start.line as usize)
                .unwrap();
            panic_msg += &format!(
                "Failed to find expected reference at position ({}, {}), ({}, {}):\n{line}\n{}{}\n",
                reference.start.line,
                reference.start.character,
                reference.end.line,
                reference.end.character,
                " ".repeat(reference.start.character as usize),
                "^".repeat((reference.end.character - reference.start.character) as usize),
            );
        }

        assert!(panic_msg.is_empty(), "{panic_msg}");
    }

    /*
     *  tree-sitter-c queries
     */
    #[tokio::test(flavor = "current_thread")]
    async fn it_gives_references_for_captures_0() {
        let src = "(identifier) <REF>@variab<CURSOR>le";
        test_capture_references(src).await;
    }
    #[tokio::test(flavor = "current_thread")]
    async fn it_gives_references_for_captures_1() {
        let src = r#"((identifier) <REF>@constant
        (#match? <REF>@c<CURSOR>onstant "^[A-Z][A-Z\\d_]*$"))"#;
        test_capture_references(src).await;
    }
    #[tokio::test(flavor = "current_thread")]
    async fn it_gives_references_for_captures_2() {
        let src =
            r"(type_definition declarator: (type_identifier) @name) <REF>@defin<CURSOR>ition.type";
        test_capture_references(src).await;
    }
    #[tokio::test(flavor = "current_thread")]
    async fn it_gives_references_for_captures_3() {
        let src = r"(call_expression
        function: (identifier) <REF>@<CURSOR>function)";
        test_capture_references(src).await;
    }

    /*
     * Other
     */
    #[tokio::test(flavor = "current_thread")]
    async fn it_gives_references_for_captures_4() {
        let src = r#"; html(`...`), html`...`, sql(`...`), etc.
    (call_expression
      function: (identifier) @injection.language
      arguments: [
        (arguments
          (template_string) <REF>@injection.content)
        (template_string) <REF>@injection<CURSOR>.content
      ]
      (#lua-match? @injection.language "^[a-zA-Z][a-zA-Z0-9]*$")
      (#offset! <REF>@injection.content 0 1 0 -1)
      (#set! injection.include-children)
      ; Languages excluded from auto-injection due to special rules
      ; - svg uses the html parser
      ; - css uses the styled parser
      (#not-any-of? @injection.language "svg" "css"))"#;
        test_capture_references(src).await;
    }
    // TODO: Pull out more tests from other tree-sitter repos

    #[tokio::test(flavor = "current_thread")]
    async fn test_server_initialize() {
        use tower::ServiceExt;
        // Arrange
        let options = Arc::new(RwLock::new(Options {
            parser_install_directories: None,
            parser_aliases: None,
            language_retrieval_patterns: None,
        }));
        let (mut service, _socket) = LspService::new(|client| Backend {
            client,
            document_map: DashMap::new(),
            cst_map: DashMap::new(),
            symbols_set_map: DashMap::new(),
            symbols_vec_map: DashMap::new(),
            fields_set_map: DashMap::new(),
            fields_vec_map: DashMap::new(),
            options,
        });

        // Act
        let resp = service
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
            .unwrap();

        // Assert
        assert_eq!(
            resp,
            Some(lsp_response_to_jsonrpc_response::<Initialize>(
                InitializeResult {
                    capabilities: SERVER_CAPABILITIES.clone(),
                    ..Default::default()
                }
            ))
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn test_server_did_open_document() {
        // Arrange
        let mut service = initialize_server(&[]).await;
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
        let mut service = initialize_server(&[]).await;

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
        let mut service = initialize_server(&[(TEST_URI.clone(), original)]).await;

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
        let mut service = initialize_server(&[(TEST_URI.clone(), original)]).await;

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
}
