use std::{
    ops::Deref,
    sync::{Arc, LazyLock},
};

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{Error, ErrorCode, Result},
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DocumentDiagnosticParams, DocumentDiagnosticReport,
        DocumentDiagnosticReportResult, FullDocumentDiagnosticReport,
        RelatedFullDocumentDiagnosticReport, Url,
    },
};
use tree_sitter::{
    Language, Node, Query, QueryCursor, QueryError, QueryErrorKind, StreamingIterator as _,
    TreeCursor,
};
use ts_query_ls::{Options, PredicateParameter, PredicateParameterArity, PredicateParameterType};

use crate::{
    Backend, DocumentData, LanguageData, QUERY_LANGUAGE, SymbolInfo,
    util::{CAPTURES_QUERY, NodeUtil as _, TextProviderRope, uri_to_basename},
};

static DIAGNOSTICS_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/queries/query/diagnostics.scm"
        )),
    )
    .unwrap()
});
static DEFINITIONS_QUERY: LazyLock<Query> =
    LazyLock::new(|| Query::new(&QUERY_LANGUAGE, "(program (definition) @def)").unwrap());

pub async fn diagnostic(
    backend: &Backend,
    params: DocumentDiagnosticParams,
) -> Result<DocumentDiagnosticReportResult> {
    let uri = &params.text_document.uri;
    let Some(document) = backend.document_map.get(uri).as_deref().cloned() else {
        return Err(Error {
            code: ErrorCode::InternalError,
            message: format!("Document not found for URI '{uri}'").into(),
            data: None,
        });
    };
    let language_data = document
        .language_name
        .as_ref()
        .and_then(|name| backend.language_map.get(name))
        .as_deref()
        .cloned();
    Ok(DocumentDiagnosticReportResult::Report(
        DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                result_id: None,
                items: get_diagnostics(uri, document, language_data, backend.options.clone(), true)
                    .await,
            },
        }),
    ))
}

static QUERY_SCAN_CACHE: LazyLock<DashMap<(String, String), Option<usize>>> =
    LazyLock::new(DashMap::new);

fn get_pattern_diagnostic_cached(
    pattern_node: Node,
    rope: &Rope,
    language_name: String,
    language: Language,
) -> Option<usize> {
    let pattern_text = pattern_node.text(rope);
    let pattern_key = (language_name, pattern_text);
    if let Some(cached_diag) = QUERY_SCAN_CACHE.get(&pattern_key) {
        return *cached_diag.deref();
    }
    let byte_offset = get_pattern_diagnostic(&pattern_key.1, language);
    QUERY_SCAN_CACHE.insert(pattern_key, byte_offset);
    byte_offset
}

fn get_pattern_diagnostic(pattern_text: &str, language: Language) -> Option<usize> {
    match Query::new(&language, pattern_text) {
        Err(QueryError {
            kind: QueryErrorKind::Structure,
            offset,
            ..
        }) => Some(offset),
        _ => None,
    }
}

pub async fn get_diagnostics(
    uri: &Url,
    document: DocumentData,
    language_data: Option<Arc<LanguageData>>,
    options_arc: Arc<tokio::sync::RwLock<Options>>,
    cache: bool,
) -> Vec<Diagnostic> {
    let tree = document.tree.clone();
    let rope = document.rope.clone();
    let error_severity = Some(DiagnosticSeverity::ERROR);
    let ld = language_data.clone();

    // Separately iterate over pattern definitions since this step can be costly and we want to
    // wrap in `spawn_blocking`. We can't merge this with the main iteration loop because it would
    // cause a race condition, due to holding the `options` lock while `await`ing.
    let handle = tokio::task::spawn_blocking(move || {
        let provider = TextProviderRope(&rope);
        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(&DEFINITIONS_QUERY, tree.root_node(), &provider);
        let mut diagnostics = Vec::new();
        let Some(language_name) = document.language_name else {
            return diagnostics;
        };
        let Some(LanguageData {
            language: Some(language),
            ..
        }) = ld.as_deref()
        else {
            return diagnostics;
        };
        while let Some(match_) = matches.next() {
            for capture in match_.captures {
                if let Some(offset) = if cache {
                    get_pattern_diagnostic_cached(
                        capture.node,
                        &rope,
                        language_name.clone(),
                        language.clone(),
                    )
                } else {
                    get_pattern_diagnostic(&capture.node.text(&rope), language.clone())
                } {
                    let true_offset = offset + capture.node.start_byte();
                    diagnostics.push(Diagnostic {
                        message: String::from("Invalid pattern structure"),
                        severity: error_severity,
                        range: tree
                            .root_node()
                            .named_descendant_for_byte_range(true_offset, true_offset)
                            .map(|node| node.lsp_range(&rope))
                            .unwrap_or_default(),
                        ..Default::default()
                    });
                }
            }
        }
        diagnostics
    })
    .await;

    let mut diagnostics = handle.unwrap_or_default();

    let options = options_arc.read().await;
    let valid_captures = options
        .valid_captures
        .get(&uri_to_basename(uri).unwrap_or_default());
    let rope = &document.rope;
    let tree = &document.tree;
    let valid_predicates = &options.valid_predicates;
    let valid_directives = &options.valid_directives;
    let symbols = language_data.as_deref().map(|ld| &ld.symbols_set);
    let fields = language_data.as_deref().map(|ld| &ld.fields_set);
    let supertypes = language_data.as_deref().map(|ld| &ld.supertype_map);
    let mut cursor = QueryCursor::new();
    let mut tree_cursor = tree.root_node().walk();
    let provider = &TextProviderRope(rope);
    let mut matches = cursor.matches(&DIAGNOSTICS_QUERY, tree.root_node(), provider);
    while let Some(match_) = matches.next() {
        for capture in match_.captures {
            let capture_name = DIAGNOSTICS_QUERY.capture_names()[capture.index as usize];
            let capture_text = capture.node.text(rope);
            let range = capture.node.lsp_range(rope);
            match capture_name {
                capture_name if capture_name.starts_with("node.") => {
                    let symbols = match symbols {
                        Some(symbols) => symbols,
                        None => continue,
                    };
                    let sym = SymbolInfo {
                        label: capture_text.clone(),
                        named: capture_name == "node.named",
                    };
                    if !symbols.contains(&sym) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid node type: \"{capture_text}\""),
                            severity: error_severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "supertype" => {
                    let supertypes = match supertypes {
                        Some(supertypes) => supertypes,
                        None => continue,
                    };
                    let symbols = match symbols {
                        Some(symbols) => symbols,
                        None => continue,
                    };
                    let supertype_text = capture_text;
                    let sym = SymbolInfo {
                        label: supertype_text.clone(),
                        named: true,
                    };
                    if let Some(subtypes) = supertypes.get(&sym) {
                        let subtype = capture.node.next_named_sibling().unwrap();
                        let subtype_text = subtype.text(rope);
                        let subtype_sym = SymbolInfo {
                            label: subtype_text.clone(),
                            named: true,
                        };
                        let range = subtype.lsp_range(rope);
                        // Only run this check when subtypes is not empty, to account for parsers
                        // generated with ABI < 15
                        if !subtypes.is_empty() && !subtypes.contains(&subtype_sym) {
                            diagnostics.push(Diagnostic {
                                message: format!("Node \"{subtype_text}\" is not a subtype of \"{supertype_text}\""),
                                severity: error_severity,
                                range,
                                ..Default::default()
                            });
                        } else if subtypes.is_empty() && !symbols.contains(&subtype_sym) {
                            diagnostics.push(Diagnostic {
                                message: format!("Invalid node type: \"{subtype_text}\""),
                                severity: error_severity,
                                range,
                                ..Default::default()
                            });
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            message: format!("Node \"{supertype_text}\" is not a supertype"),
                            severity: error_severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "field" => {
                    let fields = match fields {
                        Some(fields) => fields,
                        None => continue,
                    };
                    let field = capture_text;
                    if !fields.contains(&field) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid field name: \"{field}\""),
                            severity: error_severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "error" => diagnostics.push(Diagnostic {
                    message: "Invalid syntax".to_owned(),
                    severity: error_severity,
                    range,
                    ..Default::default()
                }),
                "missing" => diagnostics.push(Diagnostic {
                    message: format!("Missing \"{}\"", capture.node.kind()),
                    severity: error_severity,
                    range,
                    ..Default::default()
                }),
                "capture" => {
                    if capture
                        .node
                        .parent()
                        .is_some_and(|p| p.kind() == "parameters")
                    {
                        let mut cursor = QueryCursor::new();
                        let query = &CAPTURES_QUERY;
                        let mut matches = cursor.matches(
                            query,
                            tree.root_node()
                                .child_with_descendant(capture.node)
                                .unwrap(),
                            provider,
                        );
                        let mut valid = false;
                        // NOTE: Find a simpler way to do this?
                        'outer: while let Some(m) = matches.next() {
                            for cap in m.captures {
                                if let Some(parent) = cap.node.parent() {
                                    if parent.kind() != "parameters"
                                        && cap.node.text(rope) == capture_text
                                    {
                                        valid = true;
                                        break 'outer;
                                    }
                                }
                            }
                        }
                        if !valid {
                            diagnostics.push(Diagnostic {
                                message: format!("Undeclared capture: \"{capture_text}\""),
                                severity: error_severity,
                                range,
                                ..Default::default()
                            });
                        }
                    } else if let Some(suffix) = capture_text.strip_prefix("@") {
                        if !suffix.starts_with('_')
                            && valid_captures
                                .is_some_and(|c| !c.contains_key(&String::from(suffix)))
                        {
                            diagnostics.push(Diagnostic {
                                message: format!("Unsupported capture name \"{capture_text}\", consider prefixing with '_'"),
                                severity: Some(DiagnosticSeverity::WARNING),
                                range,
                                ..Default::default()
                            });
                        }
                    }
                }
                "predicate" | "directive" => {
                    let validator = if capture_name == "predicate" {
                        valid_predicates
                    } else {
                        valid_directives
                    };
                    if validator.is_empty() {
                        continue;
                    }
                    if let Some(predicate) = validator.get(&capture_text) {
                        validate_predicate(
                            &mut diagnostics,
                            &mut tree_cursor,
                            rope,
                            &predicate.parameters,
                            capture.node,
                        );
                    } else {
                        diagnostics.push(Diagnostic {
                            message: format!("Unrecognized {capture_name} \"{capture_text}\""),
                            severity: Some(DiagnosticSeverity::WARNING),
                            range,
                            ..Default::default()
                        });
                    }
                }
                _ => {}
            }
        }
    }
    diagnostics
}

fn validate_predicate<'a>(
    diagnostics: &mut Vec<Diagnostic>,
    tree_cursor: &mut TreeCursor<'a>,
    rope: &Rope,
    predicate_params: &[PredicateParameter],
    predicate_node: Node<'a>,
) {
    let params_node = predicate_node.parent().unwrap().named_child(2).unwrap();
    let mut param_spec_iter = predicate_params.iter().peekable();
    let mut prev_param_spec = match param_spec_iter.peek() {
        Some(p) => *p,
        None => {
            diagnostics.push(Diagnostic {
                message: String::from("Parameter specification must not be empty"),
                severity: Some(DiagnosticSeverity::WARNING),
                range: params_node.lsp_range(rope),
                ..Default::default()
            });
            return;
        }
    };

    let param_type_mismatch = |is_capture: bool, param_spec: &PredicateParameter| {
        is_capture && param_spec.type_ == PredicateParameterType::String
            || !is_capture && param_spec.type_ == PredicateParameterType::Capture
    };

    let type_mismatch_diag =
        |is_capture: bool, param: Node<'a>, param_spec: &PredicateParameter| Diagnostic {
            message: format!(
                "Parameter type mismatch: expected \"{}\", got \"{}\"",
                param_spec.type_,
                if is_capture { "capture" } else { "string" }
            ),
            severity: Some(DiagnosticSeverity::WARNING),
            range: param.lsp_range(rope),
            ..Default::default()
        };

    for param in params_node.children(tree_cursor) {
        if param.is_missing() {
            // At least one parameter must be passed; this will be caught by the MISSING syntax
            // error diagnostic.
            break;
        }
        let is_capture = param.kind() == "capture";
        if let Some(param_spec) = param_spec_iter.next() {
            if param_type_mismatch(is_capture, param_spec) {
                diagnostics.push(type_mismatch_diag(is_capture, param, param_spec));
            }
            prev_param_spec = param_spec;
        } else if prev_param_spec.arity != PredicateParameterArity::Variadic {
            diagnostics.push(Diagnostic {
                message: format!("Unexpected parameter: \"{}\"", param.text(rope),),
                severity: Some(DiagnosticSeverity::WARNING),
                range: param.lsp_range(rope),
                ..Default::default()
            });
        } else if param_type_mismatch(is_capture, prev_param_spec) {
            diagnostics.push(type_mismatch_diag(is_capture, param, prev_param_spec));
        }
    }
    if let Some(PredicateParameter {
        type_,
        description: _,
        arity: PredicateParameterArity::Required,
    }) = param_spec_iter.next()
    {
        diagnostics.push(Diagnostic {
            message: format!("Missing parameter of type \"{}\"", type_),
            severity: Some(DiagnosticSeverity::WARNING),
            range: predicate_node.parent().unwrap().lsp_range(rope),
            ..Default::default()
        });
    }
}

#[cfg(test)]
mod test {
    use std::collections::{BTreeMap, HashMap};
    use std::sync::Arc;

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
    use ts_query_ls::{
        Options, Predicate, PredicateParameter, PredicateParameterArity, PredicateParameterType,
    };

    use crate::{
        SymbolInfo,
        handlers::diagnostic::get_diagnostics,
        test_helpers::helpers::{TEST_URI, initialize_server},
    };

    #[rstest]
    #[case(
        r#"((identifier) @constant
(#match? @cons "^[A-Z][A-Z\\d_]*$"))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([
                    (String::from("variable"), String::default()),
                    (String::from("variable.parameter"), String::default()),
                ]))]),
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 14,
                },
                end: Position {
                    line: 0,
                    character: 23,
                },
            },
            severity: Some(DiagnosticSeverity::WARNING),
            message: String::from("Unsupported capture name \"@constant\", consider prefixing with '_'"),
            ..Default::default()
        }, Diagnostic {
            range: Range {
                start: Position {
                    line: 1,
                    character: 9,
                },
                end: Position {
                    line: 1,
                    character: 14,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: String::from("Undeclared capture: \"@cons\""),
            ..Default::default()
        }],
    )]
    #[case(
        r#"((identifierr) @_constant
(#match? @_constant "^[A-Z][A-Z\\d_]*$"))

(identifier) @variable"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([
                    (String::from("variable"), String::default()),
                    (String::from("variable.parameter"), String::default()),
                ]))]),
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 2,
                },
                end: Position {
                    line: 0,
                    character: 13,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: String::from("Invalid node type: \"identifierr\""),
            ..Default::default()
        }],
    )]
    #[case(
        r#"((identifier) @variable
(#match? @variable "^[A-Z][A-Z\\d_]*$"))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([
                    (String::from("variable"), String::default()),
                    (String::from("variable.parameter"), String::default()),
                ]))]),
            ..Default::default()
        },
        &[],
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#eq? @variable.builtin "self"))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: BTreeMap::from([(String::from("eq"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[],
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#eq? @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: BTreeMap::from([(String::from("eq"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position { line: 1, character: 0 },
                end: Position { line: 1, character: 24 },
            },
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: None,
            message: String::from("Missing parameter of type \"any\""),
            related_information: None,
            tags: None,
            data: None,
        }],
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#eq? @variable.builtin "self" @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: BTreeMap::from([(String::from("eq"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position { line: 1, character: 31 },
                end: Position { line: 1, character: 48 },
            },
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: None,
            message: String::from("Unexpected parameter: \"@variable.builtin\""),
            related_information: None,
            tags: None,
            data: None,
        }],
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin "self" "asdf" "bar"))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::String,
                    arity: PredicateParameterArity::Variadic,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[],
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin "self" "asdf" "bar" @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::String,
                    arity: PredicateParameterArity::Variadic,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[
            Diagnostic {
                range: Range {
                    start: Position { line: 1, character: 45, },
                    end: Position { line: 1, character: 62, },
                },
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: None,
                message: String::from("Parameter type mismatch: expected \"string\", got \"capture\""),
                related_information: None,
                tags: None,
                data: None,
            },
        ]
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin "self" "asdf" "bar" @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Variadic,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[]
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Variadic,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[]
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Optional,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[]
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin "self"))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Optional,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[]
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#set! @variable.builtin "self" "asdf"))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Optional,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[
            Diagnostic {
                range: Range {
                    start: Position { line: 1, character: 32, },
                    end: Position { line: 1, character: 38, },
                },
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: None,
                message: String::from("Unexpected parameter: \"\"asdf\"\""),
                related_information: None,
                tags: None,
                data: None,
            },
        ]
    )]
    #[case(
        r#"((identifier) @variable.builtin
(#sett! @variable.builtin "self" "asdf" "bar" @variable.builtin))"#,
        &[SymbolInfo { label: String::from("identifier"), named: true }],
        &["operator"],
        &["supertype"],
        Options {
            valid_predicates: Default::default(),
            valid_directives: BTreeMap::from([(String::from("set"), Predicate {
                description: String::from("Checks for equality"),
                parameters: vec![PredicateParameter {
                    type_: PredicateParameterType::Capture,
                    arity: PredicateParameterArity::Required,
                    description: None,
                }, PredicateParameter {
                    type_: PredicateParameterType::Any,
                    arity: PredicateParameterArity::Variadic,
                    description: None,
                }],
            })]),
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("variable.builtin"), String::default())]))]),
            ..Default::default()
        },
        &[
            Diagnostic {
                range: Range {
                    start: Position { line: 1, character: 2, },
                    end: Position { line: 1, character: 6, },
                },
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: None,
                message: String::from("Unrecognized directive \"sett\""),
                related_information: None,
                tags: None,
                data: None,
            },
        ]
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_diagnostics(
        #[case] source: &str,
        #[case] symbols: &[SymbolInfo],
        #[case] fields: &[&str],
        #[case] supertypes: &[&str],
        #[case] options: Options,
        #[case] expected_diagnostics: &[Diagnostic],
    ) {
        // Arrange
        let service = initialize_server(
            &[(
                TEST_URI.clone(),
                source,
                symbols.to_vec(),
                fields.to_vec(),
                supertypes.to_vec(),
            )],
            &Default::default(),
        )
        .await;
        let doc = service.inner().document_map.get(&TEST_URI).unwrap();
        let language_name = doc.language_name.clone().unwrap();
        let language_data = service
            .inner()
            .language_map
            .get(&language_name)
            .as_deref()
            .cloned();

        // Act
        let diagnostics = get_diagnostics(
            &TEST_URI,
            doc.clone(),
            language_data,
            Arc::new(options.clone().into()),
            false,
        )
        .await;

        // Assert
        assert_eq!(diagnostics, expected_diagnostics)
    }
}
