use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Position, Range};
use tracing::warn;
use tree_sitter::Parser;

use crate::{
    Backend, QUERY_LANGUAGE,
    util::{edit_rope, get_imported_uris, lsp_textdocchange_to_ts_inputedit},
};

use super::did_open::populate_import_documents;

pub async fn did_change(backend: &Backend, params: DidChangeTextDocumentParams) {
    let uri = &params.text_document.uri;
    let Some(mut document) = backend.document_map.get_mut(uri) else {
        warn!("No document found for URI: {uri} when handling did_change");
        return;
    };
    let version = params.text_document.version;
    document.version = version;
    let mut parser = Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");

    let mut edits = vec![];
    let mut recalculate_imports = false;
    for change in &params.content_changes {
        let rope = &mut document.rope;
        let new_text = change.text.as_str();

        let range = if let Some(range) = change.range {
            range
        } else {
            let start_line_idx = rope.byte_to_line(0);
            let text_end_byte_idx = new_text.len();
            let end_line_idx = rope.byte_to_line(text_end_byte_idx);

            let start = Position::new(start_line_idx as u32, 0);
            let end = Position::new(end_line_idx as u32, 0);
            Range { start, end }
        };

        if range.start.line == 0 {
            recalculate_imports = true;
        }

        edits.push(lsp_textdocchange_to_ts_inputedit(rope, change).unwrap());
        edit_rope(rope, range, new_text);
    }
    let contents = document.rope.to_string();
    let mut old_tree = document.tree.clone();

    for edit in edits {
        old_tree.edit(&edit);
    }

    let Some(tree) = parser.parse(&contents, Some(&old_tree)) else {
        warn!("Failure during tree parse");
        return;
    };

    document.tree = tree;

    if recalculate_imports {
        let rope = document.rope.clone();
        let tree = document.tree.clone();

        // We must not hold a mutable reference to something in the `document_map` while populating
        // the import documents
        drop(document);

        let workspace_uris = backend.workspace_uris.read().unwrap().clone();
        let options = backend.options.read().await;
        let uris = get_imported_uris(&workspace_uris, &options, uri, &rope, &tree);
        populate_import_documents(&backend.document_map, &workspace_uris, &options, &uris);

        if let Some(mut document) = backend.document_map.get_mut(uri) {
            document.imported_uris = uris;
        };
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
        let mut service = initialize_server(
            &[(
                TEST_URI.clone(),
                original,
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            )],
            &Default::default(),
        )
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
        let doc = service.inner().document_map.get(&TEST_URI).unwrap();
        let rope = &doc.rope;
        assert_eq!(rope.to_string(), expected);
        let tree = &doc.tree;
        assert_eq!(
            tree.root_node().utf8_text(expected.as_bytes()).unwrap(),
            expected
        );
    }
}
