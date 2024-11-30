#[cfg(test)]
mod tests {
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
            request::Initialize,
            ClientCapabilities, CompletionOptions, DidChangeConfigurationParams,
            DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentChanges,
            InitializeParams, InitializeResult, OneOf, OptionalVersionedTextDocumentIdentifier,
            PartialResultParams, Position, Range, ReferenceContext, ReferenceParams, RenameParams,
            ServerCapabilities, TextDocumentContentChangeEvent, TextDocumentEdit,
            TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
            TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
            VersionedTextDocumentIdentifier, WorkDoneProgressParams, WorkspaceEdit,
        },
        LanguageServer, LspService,
    };

    use crate::{Backend, Options, QUERY_LANGUAGE, SERVER_CAPABILITIES};

    lazy_static! {
        static ref TEST_URI: Url = Url::parse("file:///tmp/test.scm").unwrap();
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
        let source = r#"
        "[" @cap
        "#;

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

    #[tokio::test(flavor = "current_thread")]
    async fn it_handles_server_did_change_0() {
        let source = r#"(node_name) @hello
";" @semicolon"#;
        let edits = vec![
            TestEdit::new("goodbye", (0, 13), (0, 18)),
            TestEdit::new("identifier", (0, 1), (0, 10)),
            TestEdit::new("punctuation.delimiter", (1, 5), (1, 14)),
        ];
        let expected = r#"(identifier) @goodbye
";" @punctuation.delimiter"#;
        test_server_did_change(source, expected, &edits).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn it_handles_server_did_change_1() {
        let source = r#"; Some comment with emojis 🚀🛳️🫡
(node_name) @hello
";" @semicolon"#;

        let expected = r#"; Some comment with emojis 🚀🛳️🫡
(identifier) @goodbye
";" @punctuation.delimiter"#;
        let edits = vec![
            TestEdit::new("goodbye", (1, 13), (1, 18)),
            TestEdit::new("identifier", (1, 1), (1, 10)),
            TestEdit::new("punctuation.delimiter", (2, 5), (2, 14)),
        ];
        test_server_did_change(source, expected, &edits).await;
    }

    #[derive(Debug, Clone)]
    struct TestEdit {
        pub text: String,
        pub start: Position,
        pub end: Position,
    }

    impl TestEdit {
        fn new(text: &str, start: (u32, u32), end: (u32, u32)) -> Self {
            Self {
                text: text.to_string(),
                start: Position {
                    line: start.0,
                    character: start.1,
                },
                end: Position {
                    line: end.0,
                    character: end.1,
                },
            }
        }
    }

    impl From<&TestEdit> for TextDocumentContentChangeEvent {
        fn from(val: &TestEdit) -> Self {
            Self {
                range: Some(Range {
                    start: Position {
                        line: val.start.line,
                        character: val.start.character,
                    },
                    end: Position {
                        line: val.end.line,
                        character: val.end.character,
                    },
                }),
                range_length: None,
                text: val.text.clone(),
            }
        }
    }

    async fn test_server_did_change(original: &str, expected: &str, edits: &[TestEdit]) {
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

    #[tokio::test(flavor = "current_thread")]
    async fn it_renames_captures_0() {
        let source = r#"((identifier) @constant
 (#match? @cons<CURSOR>tant "^[A-Z][A-Z\\d_]*$"))"#;
        let expected = r#"((identifier) @invariant
 (#match? @invariant "^[A-Z][A-Z\\d_]*$"))"#;
        test_server_rename(source, expected, "invariant").await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn it_renames_captures_1() {
        let source = r#"((identifier) @constant
 (#match? @cons<CURSOR>tant "^[A-Z][A-Z\\d_]*$"))"#;
        let expected = r#"((identifier) @const
 (#match? @const "^[A-Z][A-Z\\d_]*$"))"#;
        test_server_rename(source, expected, "const").await;
    }

    async fn test_server_rename(original: &str, expected: &str, new_name: &str) {
        // Arrange
        let mut cursor_position: Option<Position> = None;
        for (line_num, line) in original.lines().enumerate() {
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
        let mut cleaned_input = original.replace("<CURSOR>", "");

        let service = initialize_server(&[(TEST_URI.clone(), &cleaned_input)]).await;

        // Act
        let rename_edits = service
            .inner()
            .rename(RenameParams {
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
            })
            .await
            .map_err(|e| format!("textDocument/rename call returned error: {e}"))
            .unwrap();

        // apply the edits returned by the server
        {
            let translate_range = |range: &Range, source: &str| -> (usize, usize) {
                let n_lines = source.lines().count();

                assert!(range.start.line as usize <= n_lines);
                assert!(range.end.line as usize <= n_lines);
                let start_line_idx: usize = source
                    .lines()
                    .take(range.start.line as usize)
                    .map(|l| l.len() + 1) // + 1 to account for '\n'
                    .sum();
                let start = start_line_idx + range.start.character as usize;
                let end_line_idx: usize = source
                    .lines()
                    .take(range.end.line as usize)
                    .map(|l| l.len() + 1) // + 1 to account for '\n'
                    .sum();
                let end = end_line_idx + range.end.character as usize;
                (start, end)
            };
            match rename_edits {
                None => {} // No edits
                Some(WorkspaceEdit {
                    document_changes: Some(DocumentChanges::Edits(edits)),
                    changes: None,
                    change_annotations: None,
                }) => {
                    for edit in edits {
                        match edit {
                            TextDocumentEdit {
                                text_document:
                                    OptionalVersionedTextDocumentIdentifier { uri, version: _ },
                                edits: inner_edits,
                            } if inner_edits.len() == 1 => {
                                assert!(
                                    uri == *TEST_URI,
                                    "Recieved edit for {uri}, expected {}",
                                    TEST_URI.clone()
                                );
                                if let OneOf::Left(TextEdit { range, new_text }) = &inner_edits[0] {
                                    let (start, end) = translate_range(range, &cleaned_input);
                                    cleaned_input.replace_range(start..end, new_text);
                                } else {
                                    panic!(
                                        "Untested rename edit format returned: {:#?}",
                                        inner_edits[0]
                                    );
                                }
                            }
                            other => panic!("Untested rename edit format returned: {other:#?}"),
                        }
                    }
                }
                other => panic!("Untested rename edit format returned: {other:#?}"),
            }
        }

        // Assert
        assert_eq!(&cleaned_input, expected);
    }
}
