use std::{
    collections::{BTreeSet, HashMap, HashSet},
    sync::LazyLock,
};

use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};
use tree_sitter::{Query, QueryCursor, StreamingIterator as _, Tree};
use ts_query_ls::SerializableCapture;

use crate::{
    QUERY_LANGUAGE, SymbolInfo,
    util::{CAPTURES_QUERY, NodeUtil as _, TextProviderRope},
};

static DIAGNOSTICS_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        r#"
(ERROR) @e
(MISSING) @m
(anonymous_node (string (string_content) @a))
(named_node . name: (identifier) @n)
(named_node . supertype: (identifier) @supertype)
(missing_node name: (identifier) @n)
(missing_node name: (string (string_content) @a))
(field_definition name: (identifier) @f)
(capture) @c
(predicate
  name: (identifier) @_name
  (parameters
    .
    ; NOTE: Technically this can be a "_" but it doesn't work with anchors. Also rare?
    [(string) (identifier)] @arg)
    (#any-of? @_name "eq" "not-eq" "any-eq" "any-not-eq"
      "match" "not-match" "any-match" "any-not-match"
      "any-of" "not-any-of"))
(predicate
  name: (identifier) @_name
    (#any-of? @_name "eq" "not-eq" "any-eq" "any-not-eq")
  (parameters
    (capture)
    _
    _+ @bad_eq))
(predicate
  name: (identifier) @_name
    (#any-of? @_name "match" "not-match" "any-match" "any-not-match")
  (parameters
    (capture)
    _
    _+ @bad_match))
"#,
    )
    .unwrap()
});

pub fn get_diagnostics(
    tree: &Tree,
    rope: &Rope,
    provider: &TextProviderRope,
    symbols: &HashSet<SymbolInfo>,
    fields: &HashSet<String>,
    supertypes: &HashMap<SymbolInfo, BTreeSet<SymbolInfo>>,
    allowable_captures: Option<&BTreeSet<SerializableCapture>>,
) -> Vec<Diagnostic> {
    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(&DIAGNOSTICS_QUERY, tree.root_node(), provider);
    let mut diagnostics = vec![];
    let has_language_info = !symbols.is_empty();
    while let Some(match_) = matches.next() {
        for capture in match_.captures {
            let capture_name = DIAGNOSTICS_QUERY.capture_names()[capture.index as usize];
            let capture_text = capture.node.text(rope);
            let severity = Some(DiagnosticSeverity::ERROR);
            let range = capture.node.lsp_range(rope);
            match capture_name {
                "a" | "n" => {
                    if !has_language_info {
                        continue;
                    }
                    let sym = SymbolInfo {
                        label: capture_text.clone(),
                        named: capture_name == "n",
                    };
                    if !symbols.contains(&sym) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid node type: \"{capture_text}\""),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "supertype" => {
                    if !has_language_info {
                        continue;
                    }
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
                                severity,
                                range,
                                ..Default::default()
                            });
                        } else if subtypes.is_empty() && !symbols.contains(&subtype_sym) {
                            diagnostics.push(Diagnostic {
                                message: format!("Invalid node type: \"{subtype_text}\""),
                                severity,
                                range,
                                ..Default::default()
                            });
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            message: format!("Node \"{supertype_text}\" is not a supertype"),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "f" => {
                    if !has_language_info {
                        continue;
                    }
                    let field = capture_text;
                    if !fields.contains(&field) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid field name: \"{field}\""),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "e" => diagnostics.push(Diagnostic {
                    message: "Invalid syntax".to_owned(),
                    severity,
                    range,
                    ..Default::default()
                }),
                "m" => diagnostics.push(Diagnostic {
                    message: format!("Missing \"{}\"", capture.node.kind()),
                    severity,
                    range,
                    ..Default::default()
                }),
                "c" => {
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
                                severity,
                                range,
                                ..Default::default()
                            });
                        }
                    } else if let Some(suffix) = capture_text.strip_prefix("@") {
                        if !suffix.starts_with('_')
                            && allowable_captures.is_some_and(|c| {
                                !c.contains(&SerializableCapture {
                                    name: String::from(suffix),
                                    ..Default::default()
                                })
                            })
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
                "arg" => {
                    diagnostics.push(Diagnostic {
                        message: "First argument must be a capture".to_owned(),
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        ..Default::default()
                    });
                }
                "bad_eq" => {
                    diagnostics.push(Diagnostic {
                        message: r##""#eq?" family predicates cannot accept multiple arguments. Consider using "#any-of?""##.to_owned(),
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        ..Default::default()
                    });
                }
                "bad_match" => {
                    diagnostics.push(Diagnostic {
                        message:
                            r##""#match?" family predicates cannot accept multiple arguments"##
                                .to_owned(),
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        ..Default::default()
                    });
                }
                _ => {}
            }
        }
    }
    diagnostics
}
