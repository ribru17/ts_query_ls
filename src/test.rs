#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use std::sync::Arc;

    use dashmap::DashMap;
    use std::sync::RwLock;
    use tower_lsp::{
        lsp_types::{
            ClientCapabilities, DidOpenTextDocumentParams, InitializeParams, PartialResultParams,
            Position, Range, ReferenceContext, ReferenceParams, TextDocumentIdentifier,
            TextDocumentItem, TextDocumentPositionParams, Url, WorkDoneProgressParams,
        },
        LanguageServer, LspService,
    };

    use tokio::sync::Mutex;

    use crate::{Backend, Options};

    lazy_static! {
        static ref TEST_URI: Url = Url::parse("file:///tmp/test.scm").unwrap();
    }

    /// Initializes a mock instance of `ts_query_ls`
    /// The result is wrapped in a `tokio::sync::Mutex` for safe sharing
    /// between threads
    async fn initialize_test_server() -> Mutex<LspService<Backend>> {
        let options = Arc::new(RwLock::new(Options {
            parser_install_directories: None,
            parser_aliases: None,
            language_retrieval_patterns: None,
        }));
        let (service, _socket) = LspService::build(|client| Backend {
            client,
            document_map: DashMap::new(),
            cst_map: DashMap::new(),
            symbols_set_map: DashMap::new(),
            symbols_vec_map: DashMap::new(),
            fields_set_map: DashMap::new(),
            fields_vec_map: DashMap::new(),
            options,
        })
        .finish();

        _ = service
            .inner()
            .initialize(InitializeParams {
                capabilities: ClientCapabilities::default(),
                root_uri: Some(Url::parse("file:///tmp/").unwrap()),
                ..Default::default()
            })
            .await
            .expect("Failed to initialize server");
        service.into()
    }

    /// Mocks the test server opening a text document with `contents`
    async fn open_document(server: &mut Mutex<LspService<Backend>>, contents: &str) {
        server
            .get_mut()
            .inner()
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: TEST_URI.clone(),
                    language_id: String::from("query"),
                    version: 0,
                    text: contents.to_string(),
                },
            })
            .await;
        assert!(
            server
                .get_mut()
                .inner()
                .document_map
                .get(&TEST_URI)
                .is_some(),
            "Failed to insert contents of mock document"
        );
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

        let mut server = initialize_test_server().await;
        open_document(&mut server, &cleaned_input).await;
        let found_refs = server
            .get_mut()
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
            .expect("textDocument/references call returned None");

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
    #[tokio::test]
    async fn it_gives_references_for_captures_0() {
        let src = "(identifier) <REF>@variab<CURSOR>le";
        test_capture_references(src).await;
    }
    #[tokio::test]
    async fn it_gives_references_for_captures_1() {
        let src = r#"((identifier) <REF>@constant
        (#match? <REF>@c<CURSOR>onstant "^[A-Z][A-Z\\d_]*$"))"#;
        test_capture_references(src).await;
    }
    #[tokio::test]
    async fn it_gives_references_for_captures_2() {
        let src =
            r"(type_definition declarator: (type_identifier) @name) <REF>@defin<CURSOR>ition.type";
        test_capture_references(src).await;
    }
    #[tokio::test]
    async fn it_gives_references_for_captures_3() {
        let src = r"(call_expression
        function: (identifier) <REF>@<CURSOR>function)";
        test_capture_references(src).await;
    }

    /*
     * Other
     */
    #[tokio::test]
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
}
