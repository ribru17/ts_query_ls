use tower_lsp::lsp_types::{DidChangeTextDocumentParams, Position, Range};
use tracing::warn;

use crate::{
    Backend,
    util::{ByteUtil, TextDocChangeUtil, edit_rope, get_imported_uris, parse, push_diagnostics},
};

use super::did_open::populate_import_documents;

pub async fn did_change(backend: &Backend, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri;
    let Some(mut document) = backend.document_map.get_mut(&uri) else {
        warn!("No document found for URI: {uri} when handling did_change");
        return;
    };
    let version = params.text_document.version;
    document.version = version;

    let mut edits = vec![];
    let mut recalculate_imports = false;
    for change in &params.content_changes {
        let rope = &mut document.rope;
        let new_text = change.text.as_str();

        let range = change.range.unwrap_or_else(|| {
            let start = Position::new(0, 0);
            let end = (rope.len_bytes() - 1).to_lsp_pos(rope);
            Range { start, end }
        });

        if range.start.line == 0 {
            recalculate_imports = true;
        }

        edits.push(change.to_tsedit(rope));
        edit_rope(rope, range, new_text);
    }

    for edit in edits {
        document.tree.edit(&edit);
    }

    let rope = document.rope.clone();
    let tree = parse(&rope, (&document.tree).into());

    document.tree = tree.clone();

    // We must not hold a reference to something in the `document_map` while populating the import
    // documents.
    drop(document);

    if recalculate_imports {
        let workspace_uris = backend.workspace_paths.read().unwrap().clone();
        let options = backend.options.read().await;
        let uris = get_imported_uris(&workspace_uris, &options, &uri, &rope, &tree);
        populate_import_documents(&backend.document_map, &workspace_uris, &options, &uris);

        if let Some(mut document) = backend.document_map.get_mut(&uri) {
            // Remove previous import URIs.
            for import_uri in document
                .imported_uris
                .iter()
                .filter_map(|import| import.uri.clone().filter(|url| url != &uri))
            {
                backend
                    .dependents
                    .entry(import_uri)
                    .or_default()
                    .remove(&uri);
            }
            // Add new import URIs.
            for import_uri in uris
                .iter()
                .filter_map(|import| import.uri.clone().filter(|url| url != &uri))
            {
                backend
                    .dependents
                    .entry(import_uri)
                    .or_default()
                    .insert(uri.clone());
            }
            document.imported_uris = uris;
        };
    }

    push_diagnostics(backend, uri).await;
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{
        DidChangeTextDocumentParams, TextDocumentContentChangeEvent,
        VersionedTextDocumentIdentifier, notification::DidChangeTextDocument,
    };

    use crate::test_helpers::helpers::{TEST_URI, TestEdit, TestService, initialize_server};

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
    #[case(
        r#"; Some comment with emojis 🚀🛳️🫡
(node_name) @hello
";" @semicolon"#,
        r#"; Some comment with emojis 🚀🛳️🫡(node_name) @hello
";" @semicolon"#,
        &[
            TestEdit::new("", (0, 34), (0, 35)),
        ]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_did_change(
        #[case] original: &str,
        #[case] expected: &str,
        #[case] edits: &[TestEdit],
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), original)], &Default::default()).await;

        // Act
        service
            .notify::<DidChangeTextDocument>(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: TEST_URI.clone(),
                    version: 1,
                },
                content_changes: edits
                    .iter()
                    .map(Into::<TextDocumentContentChangeEvent>::into)
                    .collect(),
            })
            .await;

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
