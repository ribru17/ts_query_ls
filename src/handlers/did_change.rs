use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Position, Range};
use tree_sitter::Parser;

use crate::{
    Backend, QUERY_LANGUAGE,
    util::{TextProviderRope, get_diagnostics, lsp_textdocchange_to_ts_inputedit, uri_to_basename},
};

pub async fn did_change(backend: &Backend, params: DidChangeTextDocumentParams) {
    let uri = &params.text_document.uri;
    let mut rope = backend.document_map.get_mut(uri).unwrap();
    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
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
    let contents = rope.to_string();
    let result = {
        let mut old_tree = backend.cst_map.get_mut(uri).unwrap();

        for edit in edits {
            old_tree.edit(&edit);
        }

        parser.parse(&contents, Some(&old_tree))
    };

    if let Some(tree) = result {
        *backend.cst_map.get_mut(uri).unwrap() = tree.clone();
        // Update diagnostics
        if let (Some(symbols), Some(fields), Some(supertypes), options) = (
            backend.symbols_set_map.get(uri),
            backend.fields_set_map.get(uri),
            backend.supertype_map_map.get(uri),
            backend.options.read().await,
        ) {
            let provider = TextProviderRope(&rope);
            backend
                .client
                .publish_diagnostics(
                    uri.clone(),
                    get_diagnostics(
                        &tree,
                        &rope,
                        &provider,
                        &symbols,
                        &fields,
                        &supertypes,
                        options
                            .allowable_captures
                            .get(&uri_to_basename(uri).unwrap_or_default()),
                    ),
                    None,
                )
                .await;
        }
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        DidChangeTextDocumentParams, TextDocumentContentChangeEvent,
        VersionedTextDocumentIdentifier, notification::DidChangeTextDocument,
    };

    use crate::test_helpers::helpers::{
        TEST_URI, TestEdit, initialize_server, lsp_notification_to_jsonrpc_request,
    };

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
    async fn server_did_change(
        #[case] original: &str,
        #[case] expected: &str,
        #[case] edits: &[TestEdit],
    ) {
        // Arrange
        let mut service = initialize_server(&[(
            TEST_URI.clone(),
            original,
            Vec::new(),
            Vec::new(),
            Vec::new(),
        )])
        .await;

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
}
