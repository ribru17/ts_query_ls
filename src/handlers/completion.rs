use std::collections::HashSet;

use streaming_iterator::StreamingIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, CompletionTextEdit,
    Documentation, InsertTextFormat, MarkupContent, MarkupKind, Position, Range, TextEdit,
};
use tracing::warn;
use tree_sitter::QueryCursor;
use ts_query_ls::{PredicateParameterArity, PredicateParameterType};

use crate::util::{
    CAPTURES_QUERY, NodeUtil, TextProviderRope, ToTsPoint, get_current_capture_node,
    get_language_name_raw, get_scm_files, lsp_position_to_byte_offset, node_is_or_has_ancestor,
    uri_to_basename,
};
use crate::{Backend, SymbolInfo};

pub async fn completion(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = &params.text_document_position.text_document.uri;

    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri} when handling completion");
        return Ok(None);
    };
    let rope = &doc.rope;
    let tree = &doc.tree;
    let language_data = doc
        .language_name
        .as_ref()
        .and_then(|name| backend.language_map.get(name));

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

    // Import completions
    if current_node.kind() == "comment" {
        if position.line != 0 {
            return Ok(None);
        }
        let Some(inherits) = current_node.text(rope).find("inherits: ") else {
            let line_len = rope.line(0).len_utf16_cu() as u32;
            return Ok(Some(CompletionResponse::Array(vec![CompletionItem {
                label: String::from("inherits: "),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from(include_str!(concat!(
                        env!("CARGO_MANIFEST_DIR"),
                        "/docs/inherits.md"
                    ))),
                })),
                kind: Some(CompletionItemKind::KEYWORD),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(0, line_len)),
                    new_text: String::from("; inherits: ${1:foo}"),
                })),
                ..Default::default()
            }])));
        };
        if (position.character as usize) < current_node.start_position().column + inherits + 9 {
            return Ok(None);
        }
        let Ok(path) = uri.to_file_path() else {
            return Ok(None);
        };
        let Some(query_type) = path.file_stem() else {
            return Ok(None);
        };
        let options = &backend.options.read().await;
        return Ok(Some(CompletionResponse::Array(
            get_scm_files(&backend.workspace_uris.read().unwrap())
                .filter_map(|file| {
                    if file.file_stem().is_none_or(|stem| stem != query_type) {
                        return None;
                    }
                    get_language_name_raw(&file, options)
                })
                .map(|file| CompletionItem {
                    label: file,
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                })
                .collect(),
        )));
    }

    // Subtype completions
    if params
        .context
        .as_ref()
        .is_some_and(|ctx| ctx.trigger_character == Some("/".to_string()))
        || current_node
            .prev_sibling()
            .is_some_and(|sib| sib.kind() == "/")
        || (current_node.is_error() && current_node.child(0).is_some_and(|c| c.kind() == "/"))
    {
        let response = || {
            let supertype = current_node.prev_named_sibling()?;
            let supertypes = &language_data?.supertype_map;
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
    let char_idx =
        lsp_position_to_byte_offset(position, rope).and_then(|b| rope.try_byte_to_char(b));
    let cursor_after_at_sign = char_idx.is_ok_and(|c| rope.char(c) == '@');
    let cursor_after_exclamation_point = char_idx.is_ok_and(|c| rope.char(c) == '!');
    let root = tree.root_node();
    let in_capture = cursor_after_at_sign || node_is_or_has_ancestor(root, current_node, "capture");
    let in_predicate = node_is_or_has_ancestor(root, current_node, "predicate");
    let in_missing = node_is_or_has_ancestor(root, current_node, "missing_node");
    if !in_capture
        && !in_predicate
        && let Some(language_data) = language_data
    {
        let symbols = &language_data.symbols_vec;
        let supertypes = &language_data.supertype_map;
        let fields = &language_data.fields_vec;
        let in_anon = node_is_or_has_ancestor(root, current_node, "string") && !in_predicate;
        let top_level = current_node.kind() == "program";
        let in_negated_field = current_node.kind() == "negated_field"
            || cursor_after_exclamation_point
            || (current_node.kind() == "identifier"
                && current_node
                    .parent()
                    .is_some_and(|p| p.kind() == "negated_field"));

        if in_negated_field {
            for field in fields.clone() {
                completion_items.push(CompletionItem {
                    label: field,
                    kind: Some(CompletionItemKind::FIELD),
                    ..Default::default()
                });
            }
            return Ok(Some(CompletionResponse::Array(completion_items)));
        }
        if !top_level {
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
            for field in fields {
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
    use std::{
        collections::{BTreeMap, HashMap},
        sync::LazyLock,
    };

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
        CompletionTextEdit, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
        PartialResultParams, Position, Range, TextDocumentIdentifier, TextDocumentPositionParams,
        TextEdit, WorkDoneProgressParams, request::Completion,
    };
    use ts_query_ls::{
        Options, Predicate, PredicateParameter, PredicateParameterArity, PredicateParameterType,
    };

    use crate::test_helpers::helpers::{
        QUERY_TEST_URI, initialize_server, lsp_request_to_jsonrpc_request,
        lsp_response_to_jsonrpc_response,
    };

    static NODE_COMPLETIONS: LazyLock<Vec<CompletionItem>> = LazyLock::new(|| {
        vec![
            CompletionItem {
                label: String::from("ERROR"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("escape_sequence"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("identifier"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("comment"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("predicate_type"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("program"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("definition"),
                kind: Some(CompletionItemKind::INTERFACE),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("quantifier"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("capture"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("string"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("string_content"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("parameters"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("list"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("grouping"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("missing_node"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("anonymous_node"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("named_node"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("field_definition"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("negated_field"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("predicate"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("MISSING"),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
        ]
    });

    static FIELD_COMPLETIONS: LazyLock<Vec<CompletionItem>> = LazyLock::new(|| {
        vec![
            CompletionItem {
                label: String::from("name"),
                kind: Some(CompletionItemKind::FIELD),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("parameters"),
                kind: Some(CompletionItemKind::FIELD),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("quantifier"),
                kind: Some(CompletionItemKind::FIELD),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("supertype"),
                kind: Some(CompletionItemKind::FIELD),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("type"),
                kind: Some(CompletionItemKind::FIELD),
                ..Default::default()
            },
        ]
    });

    static SUBTYPE_COMPLETIONS: LazyLock<Vec<CompletionItem>> = LazyLock::new(|| {
        vec![
            CompletionItem {
                label: String::from("anonymous_node"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("field_definition"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("grouping"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("list"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("missing_node"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("named_node"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("predicate"),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            },
        ]
    });

    #[rstest]
    #[case(
        r#"((identifier) @constant
(#match? @cons "^[A-Z][A-Z\\d_]*$"))"#,
        Position { line: 1, character: 14 },
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
        &Default::default(),
        &{
            let mut compls = NODE_COMPLETIONS.clone();
            compls.extend(FIELD_COMPLETIONS.clone().iter_mut().map(|fc| {
                fc.label += ": ";
                fc.clone()
            }));
            compls
        },
    )]
    #[case(
        r"((constant) @constant
; @co
)
",
        Position { line: 1, character: 4 },
        &Default::default(),
        &[]
    )]
    #[case(
        r"(definition/)",
        Position { line: 0, character: 12 },
        &Default::default(),
        &SUBTYPE_COMPLETIONS
    )]
    #[case(
        r"(definition/a)",
        Position { line: 0, character: 13 },
        &Default::default(),
        &SUBTYPE_COMPLETIONS
    )]
    #[case(
        r"(constant) @cons ",
        Position { line: 0, character: 13 },
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
                label: String::from("#not-eq?"),
                kind: Some(CompletionItemKind::FUNCTION),
                documentation: Some(tower_lsp::lsp_types::Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from("The inverse of `#eq?`, which is defined as follows:\n\nEquality check")
                })),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range { start: Position { line: 0, character: 24 }, end: Position { line: 0, character: 25 } },
                    new_text: String::from("#not-eq? ${1:@capture} ${2:text}") })),
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
    #[case(
        r"((constant ! ) @constant)",
        Position { line: 0, character: 12 },
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
        &FIELD_COMPLETIONS
    )]
    #[case(
        r"((constant !oper ) @constant)",
        Position { line: 0, character: 16 },
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
        &FIELD_COMPLETIONS
    )]
    #[case(
        r"; inherits: ",
        Position { line: 0, character: 12 },
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
        &[
            CompletionItem {
                label: String::from("cpp"),
                kind: Some(CompletionItemKind::MODULE),
                ..Default::default()
            },
            CompletionItem {
                label: String::from("other"),
                kind: Some(CompletionItemKind::MODULE),
                ..Default::default()
            },
        ]
    )]
    #[case(
        r"; inherits: ",
        Position { line: 0, character: 4 },
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
        &[]
    )]
    #[case(
        r"; inhe",
        Position { line: 0, character: 6 },
        &Options { valid_captures: HashMap::from([(String::from("test"),
            BTreeMap::from([(String::from("constant"), String::from("a constant"))]))]),
            ..Default::default() },
        &[
            CompletionItem {
                label: String::from("inherits: "),
                kind: Some(CompletionItemKind::KEYWORD),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from("## Inheriting queries

```query
; inherits: foo,bar
```

Queries can inherit other queries if they have an `; inherits:` comment as the
first line of the query file. The language server will then act as though the
text of the inherited query files was placed at the top of the document, and
will provide diagnostics for the text in those queries as well (calculated with
the language information of the parent query). Queries will always inherit
others of the same type (e.g. a `highlights.scm` will only import other
`highlights.scm`, never an `injections.scm`).

Note that the syntax is very sensitive; there must be _exactly one_ space after
the `inherits:` keyword, and there must be no spaces in-between module names.
"),
                })),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range::new(Position::new(0, 0), Position::new(0, 6)),
                    new_text: String::from("; inherits: ${1:foo}"),
                })),
                ..Default::default()
            },
        ]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_completions(
        #[case] source: &str,
        #[case] position: Position,
        #[case] options: &Options,
        #[case] expected_completions: &[CompletionItem],
    ) {
        // Arrange
        let mut service = initialize_server(&[(QUERY_TEST_URI.clone(), source)], options).await;

        // Act
        let actual_completions = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Completion>(
                CompletionParams {
                    context: None,
                    text_document_position: TextDocumentPositionParams {
                        position,
                        text_document: TextDocumentIdentifier {
                            uri: QUERY_TEST_URI.clone(),
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
        let expected_completions = if expected_completions.is_empty() {
            None
        } else {
            Some(CompletionResponse::Array(expected_completions.to_vec()))
        };
        assert_eq!(
            Some(lsp_response_to_jsonrpc_response::<Completion>(
                expected_completions
            )),
            actual_completions,
        );
    }
}
