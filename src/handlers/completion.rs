use std::collections::HashSet;

use streaming_iterator::StreamingIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, CompletionTextEdit,
    Documentation, MarkupContent, MarkupKind, Range, TextEdit,
};
use tracing::warn;
use tree_sitter::QueryCursor;

use crate::util::{
    CAPTURES_QUERY, NodeUtil, TextProviderRope, ToTsPoint, get_current_capture_node,
    lsp_position_to_byte_offset, node_is_or_has_ancestor, uri_to_basename,
};
use crate::{Backend, SymbolInfo};

pub async fn completion(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = &params.text_document_position.text_document.uri;

    let Some(tree) = backend.cst_map.get(uri) else {
        warn!("No CST built for URI: {uri:?}");
        return Ok(None);
    };
    let Some(rope) = &backend.document_map.get(uri) else {
        warn!("No document built for URI: {uri:?}");
        return Ok(None);
    };

    let mut position = params.text_document_position.position;
    if position.character > 0 {
        position.character -= 1;
    }
    let point = position.to_ts_point(rope);
    let query = &CAPTURES_QUERY;
    let mut cursor = QueryCursor::new();
    let current_node = tree
        .root_node()
        .named_descendant_for_point_range(point, point)
        .unwrap();

    // Don't offer completions when in a comment
    if current_node.kind() == "comment" {
        return Ok(None);
    }

    // Subtype completions
    if params
        .context
        .is_some_and(|ctx| ctx.trigger_character == Some("/".to_string()))
        || current_node
            .prev_sibling()
            .is_some_and(|sib| sib.kind() == "/")
    {
        let response = || {
            let supertype = current_node.prev_named_sibling()?;
            let supertype_map_map = backend.supertype_map_map.get(uri)?;
            let subtypes = supertype_map_map.get(&SymbolInfo {
                label: supertype.text(rope),
                named: true,
            })?;
            Some(CompletionResponse::Array(
                subtypes
                    .iter()
                    .map(|sub| CompletionItem {
                        label: sub.label.clone(),
                        kind: Some(CompletionItemKind::CLASS),
                        ..Default::default()
                    })
                    .collect(),
            ))
        };
        return Ok(response());
    }

    let mut completion_items = vec![];

    // Node and field name completions
    let cursor_after_at_sign = lsp_position_to_byte_offset(position, rope)
        .and_then(|b| rope.try_byte_to_char(b))
        .is_ok_and(|c| rope.char(c) == '@');
    let root = tree.root_node();
    let in_capture = cursor_after_at_sign || node_is_or_has_ancestor(root, current_node, "capture");
    let in_predicate = node_is_or_has_ancestor(root, current_node, "predicate");
    let in_missing = node_is_or_has_ancestor(root, current_node, "missing_node");
    if !in_capture && !in_predicate {
        let in_anon = node_is_or_has_ancestor(root, current_node, "string") && !in_predicate;
        let top_level = current_node.kind() == "program";
        if !top_level {
            if let (Some(symbols), Some(supertypes)) = (
                backend.symbols_vec_map.get(uri),
                backend.supertype_map_map.get(uri),
            ) {
                for symbol in symbols.iter() {
                    if (in_anon && !symbol.named) || (!in_anon && symbol.named) {
                        completion_items.push(CompletionItem {
                            label: symbol.label.clone(),
                            kind: if symbol.named {
                                if !supertypes.contains_key(symbol) {
                                    Some(CompletionItemKind::CLASS)
                                } else {
                                    Some(CompletionItemKind::INTERFACE)
                                }
                            } else {
                                Some(CompletionItemKind::CONSTANT)
                            },
                            ..Default::default()
                        });
                    }
                }
            }
        }
        if !in_missing && !in_anon {
            if !top_level {
                completion_items.push(CompletionItem {
                    label: String::from("MISSING"),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }
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
    if node_is_or_has_ancestor(tree.root_node(), current_node, "string") {
        return Ok(Some(CompletionResponse::Array(completion_items)));
    }
    let mut text_edit = get_current_capture_node(root, point).map_or_else(
        || {
            Some(CompletionTextEdit::Edit(TextEdit {
                new_text: Default::default(),
                range: Range {
                    start: position,
                    end: params.text_document_position.position,
                },
            }))
        },
        |cap_node| {
            Some(CompletionTextEdit::Edit(TextEdit {
                new_text: Default::default(),
                range: cap_node.lsp_range(rope),
            }))
        },
    );
    if in_predicate {
        let pattern_node = match root.child_with_descendant(current_node) {
            None => return Ok(Some(CompletionResponse::Array(completion_items))),
            Some(value) => value,
        };
        let provider = TextProviderRope(rope);
        let mut iter = cursor.matches(query, pattern_node, &provider);
        let mut seen = HashSet::new();
        let mut text_edit = if in_capture { text_edit } else { None };
        while let Some(match_) = iter.next() {
            for capture in match_.captures {
                let node_text = capture.node.text(rope);
                if let Some(CompletionTextEdit::Edit(edit)) = text_edit.as_mut() {
                    edit.new_text = node_text.clone();
                };
                let parent_params = capture
                    .node
                    .parent()
                    .is_none_or(|p| p.kind() != "parameters");
                if parent_params && !seen.contains(&node_text) {
                    seen.insert(node_text.clone());
                    completion_items.push(CompletionItem {
                        label: node_text.clone(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        text_edit: text_edit.clone(),
                        ..Default::default()
                    });
                }
            }
        }
    } else if in_capture {
        let options = backend.options.read().await;
        if let Some(valid_captures) =
            uri_to_basename(uri).and_then(|base| options.valid_captures.get(&base))
        {
            completion_items.extend(valid_captures.iter().map(|cap| {
                let label = "@".to_string() + cap.0;
                if let Some(CompletionTextEdit::Edit(edit)) = text_edit.as_mut() {
                    edit.new_text = label.clone();
                };
                CompletionItem {
                    label,
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: cap.1.clone(),
                    })),
                    kind: Some(CompletionItemKind::VARIABLE),
                    text_edit: text_edit.clone(),
                    ..Default::default()
                }
            }));
        }
    }

    Ok(Some(CompletionResponse::Array(completion_items)))
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
        CompletionTextEdit, MarkupContent, MarkupKind, PartialResultParams, Position, Range,
        TextDocumentIdentifier, TextDocumentPositionParams, TextEdit, WorkDoneProgressParams,
        request::Completion,
    };

    use crate::{
        SymbolInfo,
        test_helpers::helpers::{
            TEST_URI, initialize_server, lsp_request_to_jsonrpc_request,
            lsp_response_to_jsonrpc_response,
        },
    };

    #[rstest]
    #[case(
        r#"((identifier) @constant
(#match? @cons "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 1, character: 14 },
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        None,
        &[CompletionItem {
            label: String::from("@constant"),
            kind: Some(CompletionItemKind::VARIABLE),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                new_text: String::from("@constant"),
                range: Range {
                    start: Position { line: 1, character: 9 },
                    end: Position { line: 1, character: 14 },
                }
            })),
            ..Default::default()
        }],
    )]
    #[case(
        r#"((ident) @constant
(#match? @constant "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 0, character: 6 },
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        None,
        &[
            CompletionItem {
                label: String::from("identifier"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("MISSING"),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("operator: "),
                kind: Some(CompletionItemKind::FIELD),
                ..Default::default()
            },
        ]
    )]
    #[case(
        r"((constant) @constant
; @co
)
",
        Position { line: 1, character: 4 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        None,
        &[]
    )]
    #[case(
        r"(supertype/t)",
        Position { line: 0, character: 12 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        None,
        &[
            CompletionItem {
                label: String::from("test"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("test2"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
        ]
    )]
    #[case(
        r"(constant) @cons ",
        Position { line: 0, character: 13 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        Some(BTreeMap::from([(String::from("constant"), String::from("a constant"))])),
        &[
            CompletionItem {
                label: String::from("@constant"),
                kind: Some(CompletionItemKind::VARIABLE),
                documentation: Some(tower_lsp::lsp_types::Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from("a constant"),
                })),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit { range: Range { start: Position
                    { line: 0, character: 11 }, end: Position { line: 0, character: 16 } },
                    new_text: String::from("@constant") })),
                ..Default::default()
            },
        ]
    )]
    #[case(
        r"(constant) @ ",
        Position { line: 0, character: 12 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        Some(BTreeMap::from([(String::from("constant"), String::from("a constant"))])),
        &[
            CompletionItem {
                label: String::from("@constant"),
                kind: Some(CompletionItemKind::VARIABLE),
                documentation: Some(tower_lsp::lsp_types::Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from("a constant"),
                })),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit { range: Range { start: Position
                    { line: 0, character: 11 }, end: Position { line: 0, character: 12 } },
                    new_text: String::from("@constant") })),
                ..Default::default()
            },
        ]
    )]
    #[case(
        r"( (constant) @constant (#eq? @) ) ",
        Position { line: 0, character: 30 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        Some(BTreeMap::from([(String::from("constant"), String::from("a constant"))])),
        &[
            CompletionItem {
                label: String::from("@constant"),
                kind: Some(CompletionItemKind::VARIABLE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range { start: Position { line: 0, character: 29 }, end: Position { line: 0, character: 30 } },
                    new_text: String::from("@constant") })),
                ..Default::default()
            },
        ]
    )]
    #[case(
        r"( (constant) @constant (#eq? @cons) ) ",
        Position { line: 0, character: 34 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        Some(BTreeMap::from([(String::from("constant"), String::from("a constant"))])),
        &[
            CompletionItem {
                label: String::from("@constant"),
                kind: Some(CompletionItemKind::VARIABLE),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range { start: Position { line: 0, character: 29 }, end: Position { line: 0, character: 34 } },
                    new_text: String::from("@constant") })),
                ..Default::default()
            },
        ]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_completions(
        #[case] source: &str,
        #[case] position: Position,
        #[case] symbols: &[SymbolInfo],
        #[case] fields: &[&str],
        #[case] supertypes: &[&str],
        #[case] valid_captures: Option<BTreeMap<String, String>>,
        #[case] expected_completions: &[CompletionItem],
    ) {
        // Arrange
        let mut service = initialize_server(
            &[(
                TEST_URI.clone(),
                source,
                symbols.to_vec(),
                fields.to_vec(),
                supertypes.to_vec(),
            )],
            valid_captures,
        )
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
            Some(CompletionResponse::Array(expected_completions.to_vec()))
        };
        assert_eq!(
            completions,
            Some(lsp_response_to_jsonrpc_response::<Completion>(
                actual_completions
            ))
        );
    }
}
