use std::{
    collections::HashSet,
    ops::Deref,
    sync::{Arc, LazyLock},
};

use dashmap::DashMap;
use regex::Regex;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{Error, ErrorCode, Result},
    lsp_types::{
        Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag,
        DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
        FullDocumentDiagnosticReport, Location, Position, Range,
        RelatedFullDocumentDiagnosticReport, Url,
    },
};
use tree_sitter::{
    Language, Node, Query, QueryCursor, QueryError, QueryErrorKind, StreamingIterator as _,
    TreeCursor,
};
use ts_query_ls::{
    Options, PredicateParameter, PredicateParameterArity, PredicateParameterType,
    StringArgumentStyle,
};

use crate::{
    Backend, DocumentData, ImportedUri, LanguageData, QUERY_LANGUAGE, SymbolInfo,
    util::{CAPTURES_QUERY, NodeUtil as _, TextProviderRope, uri_to_basename},
};

use super::code_action::CodeActions;

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
static CAPTURE_DEFINITIONS_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        "
(named_node
  (capture) @capture.definition)

(list
  (capture) @capture.definition)

(anonymous_node
  (capture) @capture.definition)

(grouping
  (capture) @capture.definition)

(missing_node
  (capture) @capture.definition)
",
    )
    .unwrap()
});
static CAPTURE_REFERENCES_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(&QUERY_LANGUAGE, "(parameters (capture) @capture.reference)").unwrap()
});
pub static IDENTIFIER_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[a-zA-Z0-9_-][a-zA-Z0-9_.-]*$").unwrap());

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
    let ignore_missing_language = false;
    let cache = true;

    let items = get_diagnostics(
        uri,
        &backend.document_map,
        document,
        language_data,
        backend.options.clone(),
        ignore_missing_language,
        cache,
    )
    .await;

    Ok(DocumentDiagnosticReportResult::Report(
        DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
            // TODO: Pass related diagnostics for queries that depend on this one
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                result_id: None,
                items,
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

const ERROR_SEVERITY: Option<DiagnosticSeverity> = Some(DiagnosticSeverity::ERROR);
const WARNING_SEVERITY: Option<DiagnosticSeverity> = Some(DiagnosticSeverity::WARNING);
const HINT_SEVERITY: Option<DiagnosticSeverity> = Some(DiagnosticSeverity::HINT);

pub async fn get_diagnostics(
    uri: &Url,
    document_map: &DashMap<Url, DocumentData>,
    document: DocumentData,
    language_data: Option<Arc<LanguageData>>,
    options_arc: Arc<tokio::sync::RwLock<Options>>,
    ignore_missing_language: bool,
    cache: bool,
) -> Vec<Diagnostic> {
    let missing_language_diag = if !ignore_missing_language && language_data.is_none() {
        let message = if let Some(language_name) = document.language_name.as_ref() {
            format!("Language object for {language_name:?} not found")
        } else {
            String::from("Language name could not be determined")
        };
        Some(Diagnostic {
            message,
            severity: WARNING_SEVERITY,
            ..Default::default()
        })
    } else {
        None
    };

    let mut full_report = get_diagnostics_recursively(
        uri,
        document_map,
        document,
        language_data.clone(),
        options_arc.clone(),
        cache,
        &mut HashSet::new(),
    )
    .await;

    if let Some(diagnostic) = missing_language_diag {
        full_report.push(diagnostic);
    }

    // Check ABI version
    let options = options_arc.read().await;
    if let (Some(language_info), Some(abi_range)) =
        (language_data.as_deref(), &options.supported_abi_versions)
    {
        let abi = language_info.language.abi_version() as u32;
        if !abi_range.contains(&abi) {
            let start = abi_range.start();
            let end = abi_range.end();
            let range_str = if start == end {
                start.to_string()
            } else {
                format!("{start} through {end}")
            };
            full_report.push(Diagnostic {
                message: format!("Unsupported parser ABI {abi}, expected {range_str}"),
                severity: WARNING_SEVERITY,
                ..Default::default()
            });
        }
    }

    full_report
}

async fn get_diagnostics_recursively(
    uri: &Url,
    document_map: &DashMap<Url, DocumentData>,
    document: DocumentData,
    language_data: Option<Arc<LanguageData>>,
    options_arc: Arc<tokio::sync::RwLock<Options>>,
    cache: bool,
    seen: &mut HashSet<Url>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Box::pin(get_imported_query_diagnostics(
        document_map,
        options_arc.clone(),
        &document.imported_uris,
        language_data.clone(),
        seen,
    ))
    .await;

    let tree = document.tree.clone();
    let rope = document.rope.clone();
    let ld = language_data.clone();

    // Separately iterate over pattern definitions since this step can be costly and we want to
    // wrap in `spawn_blocking`. We can't merge this with the main iteration loop because it would
    // cause a race condition, due to holding the `options` lock while `await`ing.
    let handle = tokio::task::spawn_blocking(move || {
        let provider = TextProviderRope(&rope);
        let mut cursor = QueryCursor::new();
        let mut matches = cursor.matches(&DEFINITIONS_QUERY, tree.root_node(), &provider);
        let mut diagnostics = Vec::new();
        let Some(LanguageData {
            language,
            name: language_name,
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
                        severity: ERROR_SEVERITY,
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

    diagnostics.append(&mut handle.unwrap_or_default());

    let options = options_arc.read().await;
    let valid_captures = options
        .valid_captures
        .get(&uri_to_basename(uri).unwrap_or_default());
    let rope = &document.rope;
    let tree = &document.tree;

    let valid_predicates = &options.valid_predicates;
    let valid_directives = &options.valid_directives;
    let string_arg_style = &options.diagnostic_options.string_argument_style;
    let warn_unused_underscore_caps = options.diagnostic_options.warn_unused_underscore_captures;
    let symbols = language_data.as_deref().map(|ld| &ld.symbols_set);
    let fields = language_data.as_deref().map(|ld| &ld.fields_set);
    let supertypes = language_data.as_deref().map(|ld| &ld.supertype_map);
    let mut cursor = QueryCursor::new();
    let mut helper_cursor = QueryCursor::new();
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
                    let named = capture_name == "node.named";
                    let capture_text = if named {
                        capture_text
                    } else {
                        remove_unnecessary_escapes(&capture_text)
                    };
                    let sym = SymbolInfo {
                        label: capture_text.clone(),
                        named,
                    };
                    if !symbols.contains(&sym) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid node type: \"{capture_text}\""),
                            severity: ERROR_SEVERITY,
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
                                severity: ERROR_SEVERITY,
                                range,
                                ..Default::default()
                            });
                        } else if subtypes.is_empty() && !symbols.contains(&subtype_sym) {
                            diagnostics.push(Diagnostic {
                                message: format!("Invalid node type: \"{subtype_text}\""),
                                severity: ERROR_SEVERITY,
                                range,
                                ..Default::default()
                            });
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            message: format!("Node \"{supertype_text}\" is not a supertype"),
                            severity: ERROR_SEVERITY,
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
                            severity: ERROR_SEVERITY,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "error" => diagnostics.push(Diagnostic {
                    message: "Invalid syntax".to_owned(),
                    severity: ERROR_SEVERITY,
                    range,
                    ..Default::default()
                }),
                "missing" => diagnostics.push(Diagnostic {
                    message: format!("Missing \"{}\"", capture.node.kind()),
                    severity: ERROR_SEVERITY,
                    range,
                    ..Default::default()
                }),
                "capture.reference" => {
                    let mut matches = helper_cursor.matches(
                        &CAPTURE_DEFINITIONS_QUERY,
                        tree.root_node()
                            .child_with_descendant(capture.node)
                            .unwrap(),
                        provider,
                    );
                    let mut valid = false;
                    'outer: while let Some(m) = matches.next() {
                        for cap in m.captures {
                            if cap.node.text(rope) == capture_text {
                                valid = true;
                                break 'outer;
                            }
                        }
                    }
                    if !valid {
                        diagnostics.push(Diagnostic {
                            message: format!("Undeclared capture: \"{capture_text}\""),
                            severity: ERROR_SEVERITY,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "capture.definition" => {
                    if let Some(suffix) = capture_text.strip_prefix("@") {
                        if !suffix.starts_with('_')
                            && valid_captures
                                .is_some_and(|c| !c.contains_key(&String::from(suffix)))
                        {
                            diagnostics.push(Diagnostic {
                                message: format!(
                                    "Unsupported capture name \"{capture_text}\" (fix available)"
                                ),
                                severity: WARNING_SEVERITY,
                                range,
                                data: Some(CodeActions::PrefixUnderscore.into()),
                                ..Default::default()
                            });
                        } else if suffix.starts_with('_') && warn_unused_underscore_caps {
                            let mut matches = helper_cursor.matches(
                                &CAPTURE_REFERENCES_QUERY,
                                tree.root_node()
                                    .child_with_descendant(capture.node)
                                    .unwrap(),
                                provider,
                            );
                            let mut valid = false;
                            'outer: while let Some(m) = matches.next() {
                                for cap in m.captures {
                                    if cap.node.text(rope) == capture_text {
                                        valid = true;
                                        break 'outer;
                                    }
                                }
                            }
                            if !valid {
                                diagnostics.push(Diagnostic {
                                    message: String::from(
                                        "Unused `_`-prefixed capture (fix available)",
                                    ),
                                    severity: WARNING_SEVERITY,
                                    range,
                                    tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                                    data: Some(CodeActions::Remove.into()),
                                    ..Default::default()
                                });
                            }
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
                            severity: WARNING_SEVERITY,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "escape" => match capture_text.chars().nth(1) {
                    None | Some('"' | '\\' | 'n' | 'r' | 't' | '0') => {}
                    _ => {
                        diagnostics.push(Diagnostic {
                            message: String::from("Unnecessary escape sequence (fix available)"),
                            severity: WARNING_SEVERITY,
                            range,
                            data: Some(CodeActions::RemoveBackslash.into()),
                            ..Default::default()
                        });
                    }
                },
                "pattern" => {
                    let mut matches =
                        helper_cursor.matches(&CAPTURES_QUERY, capture.node, provider);
                    if matches.next().is_none() {
                        diagnostics.push(Diagnostic {
                            message: String::from(
                                "This pattern has no captures, and will not be processed",
                            ),
                            range,
                            severity: WARNING_SEVERITY,
                            tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                            data: Some(CodeActions::Remove.into()),
                            ..Default::default()
                        });
                    }
                }
                "string" => {
                    if *string_arg_style != StringArgumentStyle::PreferUnquoted {
                        continue;
                    }

                    // String contains escape sequences
                    if capture.node.named_child_count() > 0
                        || !IDENTIFIER_REGEX.is_match(&capture_text)
                    {
                        continue;
                    }

                    let mut range = range;
                    range.start.character -= 1;
                    range.end.character += 1;

                    diagnostics.push(Diagnostic {
                        message: String::from("Unnecessary quotations (fix available)"),
                        range,
                        severity: HINT_SEVERITY,
                        data: Some(CodeActions::Trim.into()),
                        ..Default::default()
                    });
                }
                "identifier" => {
                    if *string_arg_style != StringArgumentStyle::PreferQuoted {
                        continue;
                    }

                    diagnostics.push(Diagnostic {
                        message: String::from("Unquoted string argument (fix available)"),
                        range,
                        severity: HINT_SEVERITY,
                        data: Some(CodeActions::Enquote.into()),
                        ..Default::default()
                    });
                }
                _ => {}
            }
        }
    }
    diagnostics
}

async fn get_imported_query_diagnostics(
    document_map: &DashMap<Url, DocumentData>,
    options_arc: Arc<tokio::sync::RwLock<Options>>,
    imported_uris: &Vec<ImportedUri>,
    language_data: Option<Arc<LanguageData>>,
    seen: &mut HashSet<Url>,
) -> Vec<Diagnostic> {
    let mut items = Vec::new();
    for ImportedUri {
        start_col,
        end_col,
        name,
        uri,
    } in imported_uris
    {
        let range = Range {
            start: Position::new(0, *start_col),
            end: Position::new(0, *end_col),
        };
        if let Some(uri) = uri {
            if seen.contains(uri) {
                continue;
            }
            seen.insert(uri.clone());
            if let Some(doc) = document_map.get(uri).as_deref().cloned() {
                let mut severity = DiagnosticSeverity::HINT;
                let inner_diags = get_diagnostics_recursively(
                    uri,
                    document_map,
                    doc,
                    language_data.clone(),
                    options_arc.clone(),
                    true,
                    seen,
                )
                .await;
                let inner_diags: Vec<DiagnosticRelatedInformation> = inner_diags
                    .into_iter()
                    .map(|diag| {
                        if let Some(sev) = diag.severity {
                            // This misleadingly computes the maximum severity
                            severity = std::cmp::min(severity, sev);
                        }
                        DiagnosticRelatedInformation {
                            message: diag.message,
                            location: Location {
                                uri: uri.clone(),
                                range: diag.range,
                            },
                        }
                    })
                    .collect();
                if !inner_diags.is_empty() {
                    items.push(Diagnostic {
                        range,
                        message: format!("Issues in module {name:?}"),
                        severity: Some(severity),
                        related_information: Some(inner_diags),
                        ..Default::default()
                    });
                }
                continue;
            }
        }
        if name.is_empty() {
            items.push(Diagnostic {
                range,
                severity: WARNING_SEVERITY,
                message: String::from("Missing query module name"),
                ..Default::default()
            });
        } else {
            items.push(Diagnostic {
                range,
                severity: WARNING_SEVERITY,
                message: format!("Query module {name:?} not found"),
                ..Default::default()
            });
        }
    }
    items
}

fn remove_unnecessary_escapes(input: &str) -> String {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some(char @ ('\"' | '\\' | 'n' | 'r' | 't' | '0')) => {
                    result.push('\\');
                    result.push(char);
                }
                Some(char) => {
                    result.push(char);
                }
                None => {}
            }
        } else {
            result.push(c);
        }
    }

    result
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
                severity: WARNING_SEVERITY,
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
            severity: WARNING_SEVERITY,
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
                severity: WARNING_SEVERITY,
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
            message: format!("Missing parameter of type \"{type_}\""),
            severity: WARNING_SEVERITY,
            range: predicate_node.parent().unwrap().lsp_range(rope),
            ..Default::default()
        });
    }
}

#[cfg(test)]
mod test {
    use std::{
        collections::{BTreeMap, HashMap},
        sync::LazyLock,
    };
    use tower::{Service as _, ServiceExt as _};

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{
        Diagnostic, DiagnosticRelatedInformation, DiagnosticTag, DocumentDiagnosticParams,
        DocumentDiagnosticReport, DocumentDiagnosticReportResult, FullDocumentDiagnosticReport,
        Location, Position, Range, RelatedFullDocumentDiagnosticReport, TextDocumentIdentifier,
        Url, request::DocumentDiagnosticRequest,
    };
    use ts_query_ls::{
        DiagnosticOptions, Options, Predicate, PredicateParameter, PredicateParameterArity,
        PredicateParameterType, StringArgumentStyle,
    };

    use crate::{
        handlers::{
            code_action::CodeActions,
            diagnostic::{ERROR_SEVERITY, HINT_SEVERITY, WARNING_SEVERITY},
        },
        test_helpers::helpers::{
            Document, QUERY_TEST_URI, TEST_URI, initialize_server, lsp_request_to_jsonrpc_request,
            lsp_response_to_jsonrpc_response,
        },
    };

    static CPP_FILE_URI: LazyLock<Url> = LazyLock::new(|| {
        Url::from_file_path(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/queries/test_workspace/queries/cpp/test.scm"
        ))
        .unwrap()
    });
    static OTHER_FILE_URI: LazyLock<Url> = LazyLock::new(|| {
        Url::from_file_path(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/queries/test_workspace/queries/other/test.scm"
        ))
        .unwrap()
    });

    #[rstest]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @constant
(#match? @cons "^[A-Z][A-Z\\d_]*$"))"#,
        ),
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
            severity: WARNING_SEVERITY,
            message: String::from("Unsupported capture name \"@constant\" (fix available)"),
            data: Some(CodeActions::PrefixUnderscore.into()),
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
            severity: ERROR_SEVERITY,
            message: String::from("Undeclared capture: \"@cons\""),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"("*" @constant
(#match? @constant "^[A-Z][A-Z\\d_]*$"))"#,
        ),
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
                start: Position::new(0, 5),
                end: Position::new(0, 14),
            },
            severity: WARNING_SEVERITY,
            message: String::from("Unsupported capture name \"@constant\" (fix available)"),
            data: Some(CodeActions::PrefixUnderscore.into()),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"(MISSING "*") @keyword"#,
        ),
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
                start: Position::new(0, 14),
                end: Position::new(0, 22),
            },
            severity: WARNING_SEVERITY,
            message: String::from("Unsupported capture name \"@keyword\" (fix available)"),
            data: Some(CodeActions::PrefixUnderscore.into()),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"[ "*" ] @keyword"#,
        ),
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
                start: Position::new(0, 8),
                end: Position::new(0, 16),
            },
            severity: WARNING_SEVERITY,
            message: String::from("Unsupported capture name \"@keyword\" (fix available)"),
            data: Some(CodeActions::PrefixUnderscore.into()),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"("*") @keyword"#,
        ),
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
                start: Position::new(0, 6),
                end: Position::new(0, 14),
            },
            severity: WARNING_SEVERITY,
            message: String::from("Unsupported capture name \"@keyword\" (fix available)"),
            data: Some(CodeActions::PrefixUnderscore.into()),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((identifierr) @_constant
(#match? @_constant "^[A-Z][A-Z\\d_]*$"))

(identifier) @variable"#,
        ),
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
            severity: ERROR_SEVERITY,
            message: String::from("Invalid node type: \"identifierr\""),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable
(#match? @variable "^[A-Z][A-Z\\d_]*$"))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#eq? @variable.builtin self))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#eq? @variable.builtin))"#,
        ),
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
            severity: WARNING_SEVERITY,
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#eq? @variable.builtin self @variable.builtin))"#,
        ),
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
                start: Position { line: 1, character: 29 },
                end: Position { line: 1, character: 46 },
            },
            severity: WARNING_SEVERITY,
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin "self" "asdf" bar))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin self asdf "bar"))"#,
        ),
        Options {
            diagnostic_options: DiagnosticOptions {
                string_argument_style: StringArgumentStyle::PreferUnquoted,
                ..Default::default()
            },
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
        &[Diagnostic {
            range: Range {
                start: Position { line: 1, character: 35, },
                end: Position { line: 1, character: 40, },
            },
            severity: HINT_SEVERITY,
            code: None,
            code_description: None,
            source: None,
            message: String::from("Unnecessary quotations (fix available)"),
            related_information: None,
            tags: None,
            data: Some(CodeActions::Trim.into()),
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin self _ "bar"))"#,
        ),
        Options {
            diagnostic_options: DiagnosticOptions {
                string_argument_style: StringArgumentStyle::PreferQuoted,
                ..Default::default()
            },
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
        &[Diagnostic {
            range: Range {
                start: Position { line: 1, character: 25, },
                end: Position { line: 1, character: 29, },
            },
            severity: HINT_SEVERITY,
            code: None,
            code_description: None,
            source: None,
            message: String::from("Unquoted string argument (fix available)"),
            related_information: None,
            tags: None,
            data: Some(CodeActions::Enquote.into()),
        }, Diagnostic {
            range: Range {
                start: Position { line: 1, character: 30, },
                end: Position { line: 1, character: 31, },
            },
            severity: HINT_SEVERITY,
            code: None,
            code_description: None,
            source: None,
            message: String::from("Unquoted string argument (fix available)"),
            related_information: None,
            tags: None,
            data: Some(CodeActions::Enquote.into()),
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"(identifier) @_capture"#,
        ),
        Options {
            diagnostic_options: DiagnosticOptions::default(),
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
        &[Diagnostic {
            range: Range {
                start: Position { line: 0, character: 13, },
                end: Position { line: 0, character: 22, },
            },
            severity: WARNING_SEVERITY,
            code: None,
            code_description: None,
            source: None,
            message: String::from("Unused `_`-prefixed capture (fix available)"),
            related_information: None,
            tags: Some(vec![DiagnosticTag::UNNECESSARY]),
            data: Some(CodeActions::Remove.into()),
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin self asdf bar @variable.builtin))"#,
        ),
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
                    start: Position { line: 1, character: 39, },
                    end: Position { line: 1, character: 56, },
                },
                severity: WARNING_SEVERITY,
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin self asdf bar @variable.builtin))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin self))"#,
        ),
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
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#set! @variable.builtin self asdf))"#,
        ),
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
                    start: Position { line: 1, character: 30, },
                    end: Position { line: 1, character: 34, },
                },
                severity: WARNING_SEVERITY,
                code: None,
                code_description: None,
                source: None,
                message: String::from("Unexpected parameter: \"asdf\""),
                related_information: None,
                tags: None,
                data: None,
            },
        ]
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((identifier) @variable.builtin
(#sett! @variable.builtin self asdf bar @variable.builtin))"#,
        ),
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
                severity: WARNING_SEVERITY,
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
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"((definition) @_cap (#match? @_cap "\p\\")) "\?" @capture "#,
        ),
        Options {
            diagnostic_options: DiagnosticOptions { warn_unused_underscore_captures: false, ..Default::default() },
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 36,
                },
                end: Position {
                    line: 0,
                    character: 38,
                },
            },
            severity: WARNING_SEVERITY,
            message: String::from("Unnecessary escape sequence (fix available)"),
            data: Some(CodeActions::RemoveBackslash.into()),
            ..Default::default()
        }, Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 45,
                },
                end: Position {
                    line: 0,
                    character: 47,
                },
            },
            severity: WARNING_SEVERITY,
            message: String::from("Unnecessary escape sequence (fix available)"),
            data: Some(CodeActions::RemoveBackslash.into()),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"(identifier (identifier) (#set! foo bar))"#,
        ),
        Options::default(),
        &[Diagnostic {
            range: Range {
                start: Position::new(0, 12),
                end: Position::new(0, 24),
            },
            message: String::from("Invalid pattern structure"),
            severity: ERROR_SEVERITY,
            ..Default::default()
        }, Diagnostic {
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(0, 41),
            },
            severity: WARNING_SEVERITY,
            message: String::from("This pattern has no captures, and will not be processed"),
            data: Some(CodeActions::Remove.into()),
            tags: Some(vec![DiagnosticTag::UNNECESSARY]),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"(identifier name: (identifier) @capture)  (identifier asdf: (identifier) @capture)"#,
        ),
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("capture"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position::new(0, 12),
                end: Position::new(0, 16),
            },
            severity: ERROR_SEVERITY,
            message: String::from("Invalid pattern structure"),
            ..Default::default()
        }, Diagnostic {
            range: Range {
                start: Position::new(0, 54),
                end: Position::new(0, 58),
            },
            severity: ERROR_SEVERITY,
            message: String::from("Invalid field name: \"asdf\""),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"(identifier !asdf) @capture"#,
        ),
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("capture"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            range: Range {
                start: Position::new(0, 13),
                end: Position::new(0, 17),
            },
            severity: ERROR_SEVERITY,
            message: String::from("Invalid field name: \"asdf\""),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"; inherits: cpp
(identifier) @capture"#,
        ),
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("capture"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            message: String::from("Issues in module \"cpp\""),
            range: Range::new(Position::new(0, 12), Position::new(0, 15)),
            severity: ERROR_SEVERITY,
            related_information: Some(vec![
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: CPP_FILE_URI.clone(),
                        range: Range::new(Position::new(2, 0), Position::new(2, 7))
                    },
                    message: String::from("This pattern has no captures, and will not be processed")
                },
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: CPP_FILE_URI.clone(),
                        range: Range::new(Position::new(2, 1), Position::new(2, 6))
                    },
                    message: String::from("Invalid node type: \"squid\"")
                },
            ]),
            ..Default::default()
        }],
    )]
    #[case(
        (
            QUERY_TEST_URI.clone(),
            r#"; inherits: other
(identifier) @capture"#,
        ),
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("capture"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            message: String::from("Issues in module \"other\""),
            range: Range::new(Position::new(0, 12), Position::new(0, 17)),
            severity: WARNING_SEVERITY,
            related_information: Some(vec![
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: OTHER_FILE_URI.clone(),
                        range: Range::new(Position::new(2, 0), Position::new(2, 12))
                    },
                    message: String::from("This pattern has no captures, and will not be processed")
                },
            ]),
            ..Default::default()
        }],
    )]
    #[case(
        (
            TEST_URI.clone(),
            r#"; inherits: css,
(squid) @capture"#,
        ),
        Options {
            valid_captures: HashMap::from([(String::from("test"),
                BTreeMap::from([(String::from("capture"), String::default())]))]),
            ..Default::default()
        },
        &[Diagnostic {
            message: String::from("Query module \"css\" not found"),
            range: Range::new(Position::new(0, 12), Position::new(0, 15)),
            severity: WARNING_SEVERITY,
            ..Default::default()
        }, Diagnostic {
            message: String::from("Missing query module name"),
            range: Range::new(Position::new(0, 16), Position::new(0, 16)),
            severity: WARNING_SEVERITY,
            ..Default::default()
        }, Diagnostic {
            message: String::from("Language object for \"js\" not found"),
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            severity: WARNING_SEVERITY,
            ..Default::default()
        }],
    )]
    #[case(
        (Url::parse("file:///tmp/test.scm").unwrap(), ""),
        Default::default(),
        &[Diagnostic {
            message: String::from("Language name could not be determined"),
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            severity: WARNING_SEVERITY,
            ..Default::default()
        }],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), ""),
        {
            // Just in case our query language object changes ABI unexpectedly; we want to make
            // sure we are testing what we want to.
            pretty_assertions::assert_eq!(crate::QUERY_LANGUAGE.abi_version(), 15, "Unexpected ABI version");
            Options {
                supported_abi_versions: Some(13..=14),
                ..Default::default()
            }
        },
        &[Diagnostic {
            message: String::from("Unsupported parser ABI 15, expected 13 through 14"),
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            severity: WARNING_SEVERITY,
            ..Default::default()
        }],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), ""),
        {
            // Just in case our query language object changes ABI unexpectedly; we want to make
            // sure we are testing what we want to.
            pretty_assertions::assert_eq!(crate::QUERY_LANGUAGE.abi_version(), 15, "Unexpected ABI version");
            Options {
                supported_abi_versions: Some(13..=15),
                ..Default::default()
            }
        },
        &[],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), ""),
        {
            // Just in case our query language object changes ABI unexpectedly; we want to make
            // sure we are testing what we want to.
            pretty_assertions::assert_eq!(crate::QUERY_LANGUAGE.abi_version(), 15, "Unexpected ABI version");
            Options {
                supported_abi_versions: Some(15..=15),
                ..Default::default()
            }
        },
        &[],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), ""),
        {
            // Just in case our query language object changes ABI unexpectedly; we want to make
            // sure we are testing what we want to.
            pretty_assertions::assert_eq!(crate::QUERY_LANGUAGE.abi_version(), 15, "Unexpected ABI version");
            Options {
                supported_abi_versions: Some(13..=13),
                ..Default::default()
            }
        },
        &[Diagnostic {
            message: String::from("Unsupported parser ABI 15, expected 13"),
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            severity: WARNING_SEVERITY,
            ..Default::default()
        }],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), "(named_node/identifier) @capture"),
        Default::default(),
        &[
            Diagnostic {
                message: String::from("Invalid pattern structure"),
                range: Range::new(Position::new(0, 0), Position::new(0, 32)),
                severity: ERROR_SEVERITY,
                ..Default::default()
            },
            Diagnostic {
                message: String::from("Node \"named_node\" is not a supertype"),
                range: Range::new(Position::new(0, 1), Position::new(0, 11)),
                severity: ERROR_SEVERITY,
                ..Default::default()
            },
        ],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), "(definition/escape_sequence) @capture"),
        Default::default(),
        &[
            Diagnostic {
                message: String::from("Invalid pattern structure"),
                range: Range::new(Position::new(0, 0), Position::new(0, 37)),
                severity: ERROR_SEVERITY,
                ..Default::default()
            },
            Diagnostic {
                message: String::from("Node \"escape_sequence\" is not a subtype of \"definition\""),
                range: Range::new(Position::new(0, 12), Position::new(0, 27)),
                severity: ERROR_SEVERITY,
                ..Default::default()
            },
        ],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), "(definition/ecape_sequence) @capture"),
        Default::default(),
        &[
            Diagnostic {
                message: String::from("Node \"ecape_sequence\" is not a subtype of \"definition\""),
                range: Range::new(Position::new(0, 12), Position::new(0, 26)),
                severity: ERROR_SEVERITY,
                ..Default::default()
            },
        ],
    )]
    #[case(
        (QUERY_TEST_URI.clone(), "(deefinition/escape_sequence) @capture"),
        Default::default(),
        &[
            Diagnostic {
                message: String::from("Node \"deefinition\" is not a supertype"),
                range: Range::new(Position::new(0, 1), Position::new(0, 12)),
                severity: ERROR_SEVERITY,
                ..Default::default()
            },
        ],
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_diagnostics(
        #[case] document: Document<'_>,
        #[case] options: Options,
        #[case] expected_diagnostics: &[Diagnostic],
    ) {
        // Arrange
        let mut service = initialize_server(&[document.clone()], &options).await;

        // Act
        let actual_diagnostics = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<DocumentDiagnosticRequest>(
                DocumentDiagnosticParams {
                    text_document: TextDocumentIdentifier { uri: document.0 },
                    identifier: None,
                    previous_result_id: None,
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                },
            ))
            .await
            .map_err(|e| format!("textDocument/diagnostic call returned error: {e}"))
            .unwrap();

        // Assert
        assert_eq!(
            Some(
                lsp_response_to_jsonrpc_response::<DocumentDiagnosticRequest>(
                    DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
                        RelatedFullDocumentDiagnosticReport {
                            related_documents: None,
                            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                                result_id: None,
                                items: expected_diagnostics.to_vec(),
                            },
                        }
                    ),)
                )
            ),
            actual_diagnostics
        );
    }
}
