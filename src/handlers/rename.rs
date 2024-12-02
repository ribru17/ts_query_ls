use std::cmp::Ordering;

use log::warn;
use regex::Regex;
use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, RenameParams,
        TextDocumentEdit, TextEdit, WorkspaceEdit,
    },
};
use tree_sitter::{Query, QueryCursor};

use crate::{
    util::{
        get_current_capture_node, get_references, lsp_position_to_ts_point,
        ts_node_to_lsp_location, TextProviderRope,
    },
    Backend, QUERY_LANGUAGE,
};

pub async fn rename(backend: &Backend, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let uri = params.text_document_position.text_document.uri;
    let Some(tree) = backend.cst_map.get(&uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = backend.document_map.get(&uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };
    let current_node = match get_current_capture_node(
        tree.root_node(),
        lsp_position_to_ts_point(params.text_document_position.position, &rope),
    ) {
        None => return Ok(None),
        Some(value) => value,
    };
    let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let mut cursor = QueryCursor::new();
    // Allow the new name to begin with "@"
    let new_name = params
        .new_name
        .strip_prefix('@')
        .unwrap_or(params.new_name.as_str());
    let identifier_pattern = Regex::new(r"^[a-zA-Z0-9.\-_\$]+$").unwrap();
    if !identifier_pattern.is_match(new_name) {
        return Err(jsonrpc::Error::invalid_params(
            "New name is not a valid identifier",
        ));
    }
    let mut text_document_edits: Vec<TextDocumentEdit> = vec![];
    let provider = TextProviderRope(&rope);
    get_references(
        &tree.root_node(),
        &current_node,
        &query,
        &mut cursor,
        &provider,
        &rope,
    )
    .map(|node| ts_node_to_lsp_location(&uri, &node, &rope))
    .for_each(|mut elem| {
        // Don't include the preceding `@`
        elem.range.start.character += 1;
        text_document_edits.push(TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: elem.uri,
                // TODO: Support versioned edits
                version: None,
            },
            edits: vec![OneOf::Left(TextEdit {
                range: elem.range,
                new_text: new_name.to_owned(),
            })],
        });
    });
    // Apply edits from end to start, to prevent offset inaccuracies
    text_document_edits.sort_by(|a, b| {
        if let OneOf::Left(a) = &a.edits[0] {
            if let OneOf::Left(b) = &b.edits[0] {
                let range_a = a.range;
                let range_b = b.range;
                range_b.start.cmp(&range_a.start)
            } else {
                Ordering::Equal
            }
        } else {
            Ordering::Equal
        }
    });

    Ok(Some(WorkspaceEdit {
        document_changes: Some(DocumentChanges::Edits(text_document_edits)),
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
        request::Rename, DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, Position,
        RenameParams, TextDocumentEdit, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams, WorkspaceEdit,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
        TestEdit, COMPLEX_FILE, SIMPLE_FILE, TEST_URI,
    };

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
}
