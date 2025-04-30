use std::collections::HashSet;

use streaming_iterator::StreamingIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, CompletionTextEdit,
    Documentation, InsertTextFormat, MarkupContent, MarkupKind, Range, TextEdit,
};
use tracing::warn;
use tree_sitter::QueryCursor;
use ts_query_ls::{PredicateParameterArity, PredicateParameterType};

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

    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document for URI: {uri:?}");
        return Ok(None);
    };
    let rope = &doc.rope;
    let tree = &doc.tree;

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
        .as_ref()
        .is_some_and(|ctx| ctx.trigger_character == Some("/".to_string()))
        || current_node
            .prev_sibling()
            .is_some_and(|sib| sib.kind() == "/")
    {
        let response = || {
            let supertype = current_node.prev_named_sibling()?;
            let supertypes = &doc.supertype_map;
            let subtypes = supertypes.get(&SymbolInfo {
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

    // Predicates and directives completions
    let cursor_after_hashtag = lsp_position_to_byte_offset(position, rope)
        .and_then(|b| rope.try_byte_to_char(b))
        .is_ok_and(|c| rope.char(c) == '#');
    if cursor_after_hashtag
        || current_node
            .prev_sibling()
            .is_some_and(|sib| sib.kind() == "#")
    {
        let options = backend.options.read().await;
        let predicates = &options.valid_predicates;
        let directives = &options.valid_directives;
        let range = if cursor_after_hashtag {
            Range {
                start: position,
                end: params.text_document_position.position,
            }
        } else {
            current_node.parent().unwrap().lsp_range(rope)
        };
        let completions = predicates
            .iter()
            .map(|(name, pred)| (true, name, pred))
            .chain(directives.iter().map(|(name, pred)| (false, name, pred)))
            .map(|(is_pred, name, predicate)| {
                let label = if is_pred {
                    format!("#{name}?")
                } else {
                    format!("#{name}!")
                };
                CompletionItem {
                    label: label.clone(),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: predicate.description.clone(),
                    })),
                    kind: Some(CompletionItemKind::FUNCTION),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range,
                        new_text: format!(
                            "{label} {}",
                            predicate
                                .parameters
                                .iter()
                                .enumerate()
                                .filter_map(|(i, param)| {
                                    let i = i + 1;
                                    if param.arity == PredicateParameterArity::Required {
                                        Some(if param.type_ == PredicateParameterType::Capture {
                                            format!("${{{i}:@capture}}")
                                        } else {
                                            format!("${{{i}:text}}")
                                        })
                                    } else {
                                        None
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(" ")
                        ),
                    })),
                    ..Default::default()
                }
            });
        return Ok(Some(CompletionResponse::Array(completions.collect())));
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
            let symbols = &doc.symbols_vec;
            let supertypes = &doc.supertype_map;
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
        if !in_missing && !in_anon {
            if !top_level {
                completion_items.push(CompletionItem {
                    label: String::from("MISSING"),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }
            for field in doc.fields_vec.iter() {
                completion_items.push(CompletionItem {
                    label: format!("{field}: "),
                    kind: Some(CompletionItemKind::FIELD),
                    ..Default::default()
                });
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
    use std::collections::{BTreeMap, HashMap};

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
        CompletionTextEdit, InsertTextFormat, MarkupContent, MarkupKind, PartialResultParams,
        Position, Range, TextDocumentIdentifier, TextDocumentPositionParams, TextEdit,
        WorkDoneProgressParams, request::Completion,
    };
    use ts_query_ls::{
        Options, Predicate, PredicateParameter, PredicateParameterArity, PredicateParameterType,
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
        &Default::default(),
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
        &Default::default(),
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
        &Default::default(),
        &[]
    )]
    #[case(
        r"(supertype/t)",
        Position { line: 0, character: 12 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        &Default::default(),
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
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
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
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
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
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
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
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
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
    #[case(
        r"( (constant) @constant (#) ) ",
        Position { line: 0, character: 25 },
        &[SymbolInfo { label: String::from("constant"), named: true }],
        &["operator"],
        &["supertype"],
        &Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            valid_predicates: BTreeMap::from([
                (String::from("eq"), Predicate {
                    description: String::from("Equality check"),
                    parameters: vec![PredicateParameter {
                        type_: PredicateParameterType::Capture,
                        arity: PredicateParameterArity::Required,
                        description: None,
                    }, PredicateParameter {
                        type_: PredicateParameterType::Any,
                        arity: PredicateParameterArity::Required,
                        description: None,
                    }]
                })
            ]),
            valid_directives: BTreeMap::from([
                (String::from("set"), Predicate {
                    description: String::from("Set metadata"),
                    parameters: vec![PredicateParameter {
                        type_: PredicateParameterType::Any,
                        arity: PredicateParameterArity::Required,
                        description: None,
                    }, PredicateParameter {
                        type_: PredicateParameterType::String,
                        arity: PredicateParameterArity::Required,
                        description: None,
                    }, PredicateParameter {
                        type_: PredicateParameterType::String,
                        arity: PredicateParameterArity::Optional,
                        description: None,
                    }]
                })
            ]),
            ..Default::default()
        },
        &[
            CompletionItem {
                label: String::from("#eq?"),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation:
                    Some(tower_lsp::lsp_types::Documentation::MarkupContent(MarkupContent { kind:
                        MarkupKind::Markdown, value: String::from("Equality check") })),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range { start: Position { line: 0, character: 24 }, end: Position { line: 0, character: 25 } },
                    new_text: String::from("#eq? ${1:@capture} ${2:text}") })),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("#set!"),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation:
                    Some(tower_lsp::lsp_types::Documentation::MarkupContent(MarkupContent { kind:
                        MarkupKind::Markdown, value: String::from("Set metadata") })),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range { start: Position { line: 0, character: 24 }, end: Position { line: 0, character: 25 } },
                    new_text: String::from("#set! ${1:text} ${2:text}") })),
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
        #[case] options: &Options,
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
            options,
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
