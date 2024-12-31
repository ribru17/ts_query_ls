use std::collections::HashSet;

use log::warn;
use streaming_iterator::StreamingIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
};
use tree_sitter::{Query, QueryCursor};

use crate::util::{
    get_node_text, lsp_position_to_byte_offset, lsp_position_to_ts_point, node_is_or_has_ancestor,
    TextProviderRope,
};
use crate::{Backend, QUERY_LANGUAGE};

pub async fn completion(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = &params.text_document_position.text_document.uri;

    let Some(tree) = backend.cst_map.get(uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = backend.document_map.get(uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };

    let mut position = params.text_document_position.position;
    if position.character > 0 {
        position.character -= 1;
    }
    let point = lsp_position_to_ts_point(position, &rope);
    let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
    let mut cursor = QueryCursor::new();
    let current_node = tree
        .root_node()
        .named_descendant_for_point_range(point, point)
        .unwrap();

    // Don't offer completions when in a comment
    if current_node.kind() == "comment" {
        return Ok(None);
    }

    let mut completion_items = vec![];

    // Node and field name completions
    let cursor_after_at_sign = lsp_position_to_byte_offset(position, &rope)
        .and_then(|b| rope.try_byte_to_char(b))
        .is_ok_and(|c| rope.char(c) == '@');
    let in_capture =
        cursor_after_at_sign || node_is_or_has_ancestor(tree.root_node(), current_node, "capture");
    if !in_capture && !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate") {
        let in_anon = node_is_or_has_ancestor(tree.root_node(), current_node, "anonymous_node");
        if let Some(symbols) = backend.symbols_vec_map.get(uri) {
            for symbol in symbols.iter() {
                if (in_anon && !symbol.named) || (!in_anon && symbol.named) {
                    completion_items.push(CompletionItem {
                        label: symbol.label.clone(),
                        kind: if symbol.named {
                            Some(CompletionItemKind::CLASS)
                        } else {
                            Some(CompletionItemKind::CONSTANT)
                        },
                        ..Default::default()
                    });
                }
            }
        }
        if !in_anon {
            if let Some(fields) = backend.fields_vec_map.get(uri) {
                for field in fields.iter() {
                    completion_items.push(CompletionItem {
                        label: format!("{field}: "),
                        kind: Some(CompletionItemKind::FIELD),
                        ..Default::default()
                    });
                }
            }
        }
    }

    // Capture completions
    if !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate")
        || node_is_or_has_ancestor(tree.root_node(), current_node, "string")
    {
        return Ok(Some(CompletionResponse::Array(completion_items)));
    }
    let node = match tree.root_node().child_with_descendant(current_node) {
        None => return Ok(Some(CompletionResponse::Array(completion_items))),
        Some(value) => value,
    };
    let provider = TextProviderRope(&rope);
    let mut iter = cursor.matches(&query, node, &provider);
    let mut seen = HashSet::new();
    while let Some(match_) = iter.next() {
        for capture in match_.captures {
            let node_text = get_node_text(&capture.node, &rope);
            let parent_params = capture
                .node
                .parent()
                .is_none_or(|p| p.kind() != "parameters");
            if parent_params && !seen.contains(&node_text) {
                seen.insert(node_text.clone());
                completion_items.push(CompletionItem {
                    label: node_text.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                });
            }
        }
    }

    Ok(Some(CompletionResponse::Array(completion_items)))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        request::Completion, CompletionItem, CompletionItemKind, CompletionParams,
        CompletionResponse, PartialResultParams, Position, TextDocumentIdentifier,
        TextDocumentPositionParams, WorkDoneProgressParams,
    };

    use crate::{
        test_helpers::helpers::{
            initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
            TEST_URI,
        },
        SymbolInfo,
    };

    #[rstest]
    #[case(
        r#"((identifier) @constant
(#match? @cons "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 1, character: 14 },
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &[("@constant", CompletionItemKind::VARIABLE)]
    )]
    #[case(
        r#"((ident) @constant
(#match? @constant "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 0, character: 6 },
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &[("identifier", CompletionItemKind::CLASS), ("operator: ", CompletionItemKind::FIELD)]
    )]
    #[case(
        r"((constant) @constant
; @co
)
",
        Position { line: 1, character: 4 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &[]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_completions(
        #[case] source: &str,
        #[case] position: Position,
        #[case] symbols: &[SymbolInfo],
        #[case] fields: &[&str],
        #[case] expected_completions: &[(&str, CompletionItemKind)],
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), source, symbols.to_vec(), fields.to_vec())])
                .await;

        // Act
        let completions = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Completion>(
                CompletionParams {
                    context: None,
                    text_document_position: TextDocumentPositionParams {
                        position,
                        text_document: TextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                        },
                    },
                    partial_result_params: PartialResultParams::default(),
                    work_done_progress_params: WorkDoneProgressParams::default(),
                },
            ))
            .await
            .map_err(|e| format!("textDocument/completion call returned error: {e}"))
            .unwrap();

        // Assert
        let actual_completions = if expected_completions.is_empty() {
            None
        } else {
            Some(CompletionResponse::Array(
                expected_completions
                    .iter()
                    .map(|c| CompletionItem {
                        label: c.0.to_string(),
                        kind: Some(c.1),
                        ..Default::default()
                    })
                    .collect(),
            ))
        };
        assert_eq!(
            completions,
            Some(lsp_response_to_jsonrpc_response::<Completion>(
                actual_completions
            ))
        );
    }
}
