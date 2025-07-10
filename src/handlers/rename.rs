use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, RenameParams,
        TextDocumentEdit, TextEdit, WorkspaceEdit,
    },
};
use tracing::warn;
use tree_sitter::QueryCursor;

use crate::{
    Backend,
    util::{
        CAPTURES_QUERY, NodeUtil, TextProviderRope, ToTsPoint, get_current_capture_node,
        get_references,
    },
};

use super::diagnostic::IDENTIFIER_REGEX;

pub async fn rename(backend: &Backend, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position.text_document.uri;
    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri} when handling rename");
        return Ok(None);
    };
    let rope = &doc.rope;
    let tree = &doc.tree;
    let current_node = match get_current_capture_node(
        tree.root_node(),
        params.text_document_position.position.to_ts_point(rope),
    ) {
        None => return Ok(None),
        Some(value) => value,
    };
    let query = &CAPTURES_QUERY;
    let mut cursor = QueryCursor::new();
    // Allow the new name to begin with "@"
    let new_name = params
        .new_name
        .strip_prefix('@')
        .unwrap_or(params.new_name.as_str());
    if !IDENTIFIER_REGEX.is_match(new_name) {
        return Err(jsonrpc::Error::invalid_params(
            "New name is not a valid identifier",
        ));
    }
    let provider = TextProviderRope(rope);
    let edits = get_references(
        &tree.root_node(),
        &current_node,
        query,
        &mut cursor,
        &provider,
        rope,
    )
    .map(|node| {
        let mut range = node.lsp_range(rope);
        // Don't include the preceding `@`
        range.start.character += 1;
        OneOf::Left(TextEdit {
            range,
            new_text: new_name.to_owned(),
        })
    })
    .collect();

    Ok(Some(WorkspaceEdit {
        document_changes: Some(DocumentChanges::Edits(vec![TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: Some(doc.version),
            },
            edits,
        }])),
        changes: None,
        change_annotations: None,
    }))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, Position, RenameParams,
        TextDocumentEdit, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams, WorkspaceEdit, request::Rename,
    };

    use crate::test_helpers::helpers::{
        COMPLEX_FILE, SIMPLE_FILE, TEST_URI, TestEdit, initialize_server,
        lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
    };

    #[rstest]
    #[case(
        &SIMPLE_FILE,
        Position { line: 1, character: 12, },
        &[
            TestEdit::new("superlongnamehere", (0, 15), (0, 23)),
            TestEdit::new("superlongnamehere", (1, 11), (1, 19)),
            TestEdit::new("superlongnamehere", (1, 21), (1, 29)),
        ],
        "superlongnamehere",
    )]
    #[case(
        &COMPLEX_FILE,
        Position { line: 8, character: 24 },
        &[
            TestEdit::new("invariant", (8, 25), (8, 42)),
            TestEdit::new("invariant", (9, 23), (9, 40)),
            TestEdit::new("invariant", (12, 13), (12, 30)),
            TestEdit::new("invariant", (18, 17), (18, 34)),
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
    async fn server_rename(
        #[case] original: &str,
        #[case] cursor_position: Position,
        #[case] edits: &[TestEdit],
        #[case] new_name: &str,
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), original)], &[], &Default::default()).await;

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
                document_changes: Some(DocumentChanges::Edits(vec![TextDocumentEdit {
                    text_document: OptionalVersionedTextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                        version: Some(0),
                    },
                    edits: edits.iter().map(|e| OneOf::Left(e.into())).collect(),
                }])),
                ..Default::default()
            })
        };
        assert_eq!(
            rename_edits,
            Some(lsp_response_to_jsonrpc_response::<Rename>(ws_edit))
        );
    }
}
