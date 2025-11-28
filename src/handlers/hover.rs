use std::{collections::HashMap, sync::LazyLock};

use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Range},
};
use tracing::warn;
use tree_sitter::Query;
use ts_query_ls::{ParameterConstraint, PredicateParameterType};

use crate::{
    Backend, LspClient, QUERY_LANGUAGE, SymbolInfo,
    util::{
        FORMAT_IGNORE_REGEX, INHERITS_REGEX, NodeUtil, PosUtil, capture_at_pos,
        get_imported_module_under_cursor, remove_unnecessary_escapes, uri_to_basename,
    },
};

static HOVER_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/queries/query/hover.scm"
        )),
    )
    .unwrap()
});

/// Create a static hashmap from doc name to doc file (found in "docs/<name>.md")
macro_rules! include_docs_map {
    ($($name:literal),* $(,)?) => {
        LazyLock::new(|| {
            HashMap::from([$(
                ($name, include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/docs/", $name, ".md"))),
            )*])
        })
    };
}

static DOCS: LazyLock<HashMap<&'static str, &'static str>> = include_docs_map!(
    "missing",
    "wildcard",
    "anchor",
    "quantifier",
    "alternation",
    "error",
    "negation",
    "inherits",
    "format-ignore",
);

pub async fn hover<C: LspClient>(
    backend: &Backend<C>,
    params: HoverParams,
) -> Result<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let options = backend.options.read().await;

    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri} when handling hover");
        return Ok(None);
    };

    let tree = &doc.tree;
    let rope = &doc.rope;
    let language_data = doc
        .language_name
        .as_ref()
        .and_then(|name| backend.language_map.get(name));
    let supertypes = language_data.as_ref().map(|ld| &ld.supertype_map);

    let Some(capture) = capture_at_pos(tree, rope, &HOVER_QUERY, position.to_ts_point(rope)) else {
        return Ok(None);
    };
    let capture_name = HOVER_QUERY.capture_names()[capture.index as usize];
    let capture_text = capture.node.text(rope);
    let range = Some(capture.node.lsp_range(rope));

    Ok(match capture_name {
        doc_name if DOCS.contains_key(capture_name) => {
            let value = (*DOCS.get(doc_name).unwrap()).to_string();
            Some(Hover {
                range,
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
            })
        }
        "capture" => {
            let options = backend.options.read().await;
            if let Some(description) = uri_to_basename(uri).and_then(|base| {
                options
                    .valid_captures
                    .get(&base)
                    .and_then(|c| c.get(&capture_text[1..].to_string()))
            }) {
                let value = format!("## `{capture_text}`\n\n{description}");
                Some(Hover {
                    range,
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                })
            } else {
                None
            }
        }
        "identifier.node" => {
            let sym = SymbolInfo {
                label: capture_text,
                named: true,
            };
            if let Some(subtypes) = supertypes.and_then(|supertypes| supertypes.get(&sym)) {
                let value = if subtypes.is_empty() {
                    String::from("Subtypes could not be determined (parser ABI < 15)")
                } else {
                    subtypes.iter().fold(
                        format!("Subtypes of `({})`:\n\n```query", sym.label),
                        |acc, subtype| format!("{acc}\n{subtype}"),
                    ) + "\n```"
                };
                Some(Hover {
                    range,
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                })
            } else if let Some(language) = language_data.as_ref().map(|ld| &ld.language) {
                let syms = (0..language.node_kind_count() as u16)
                    .filter(|&id| {
                        if !(language.node_kind_is_visible(id)
                            || language.node_kind_is_supertype(id))
                            || !language.node_kind_is_named(id)
                        {
                            return false;
                        }
                        language
                            .node_kind_for_id(id)
                            .is_some_and(|kind| kind == sym.label)
                    })
                    .collect::<Vec<_>>();
                if syms.is_empty() {
                    None
                } else {
                    Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                "Symbol IDs: {}",
                                syms.iter()
                                    .map(ToString::to_string)
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        }),
                        range,
                    })
                }
            } else {
                None
            }
        }
        "anonymous" => {
            if let Some(language) = language_data.as_ref().map(|ld| &ld.language) {
                let string_content =
                    remove_unnecessary_escapes(&capture_text[1..capture_text.len() - 1]);
                let syms = (0..language.node_kind_count() as u16)
                    .filter(|&id| {
                        if !language.node_kind_is_visible(id)
                            || language.node_kind_is_supertype(id)
                            || language.node_kind_is_named(id)
                        {
                            return false;
                        }
                        language
                            .node_kind_for_id(id)
                            .is_some_and(|kind| kind == string_content)
                    })
                    .collect::<Vec<_>>();
                if syms.is_empty() {
                    None
                } else {
                    Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                "Symbol IDs: {}",
                                syms.iter()
                                    .map(ToString::to_string)
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        }),
                        range,
                    })
                }
            } else {
                None
            }
        }
        "field" => {
            if let Some(language) = language_data.as_ref().map(|ld| &ld.language) {
                let sym = (1..=language.field_count() as u16).find(|&id| {
                    language
                        .field_name_for_id(id)
                        .is_some_and(|name| name == capture_text)
                });
                sym.map(|sym| Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("Field ID: {sym}"),
                    }),
                    range,
                })
            } else {
                None
            }
        }
        "predicate" => {
            let parent = capture
                .node
                .parent()
                .expect("Should be children of the `(predicate)` node");
            let (Some(predicate_name), Some(predicate_type)) =
                (parent.named_child(0), parent.named_child(1))
            else {
                return Ok(None);
            };
            let validator = if predicate_type.text(rope) == "?" {
                &options.valid_predicates
            } else {
                &options.valid_directives
            };
            let mut range = predicate_name.lsp_range(rope);
            // Include # and ? in the range
            range.start.character -= 1;
            range.end.character += 1;
            if let Some(predicate) = validator.get(&predicate_name.text(rope)) {
                let mut value = format!("{}\n\n---\n\n## Parameters:\n\n", predicate.description);
                for param in &predicate.parameters {
                    value += format!(
                        "- Type: `{}` ({}{})\n",
                        param.type_,
                        param.arity,
                        if param.type_ != PredicateParameterType::Capture
                            && param.constraint != ParameterConstraint::None
                        {
                            format!("; constraint: {}", param.constraint)
                        } else {
                            String::new()
                        }
                    )
                    .as_str();
                    if let Some(desc) = &param.description {
                        value += format!("  - {desc}\n").as_str();
                    }
                }
                Some(Hover {
                    range: Some(range),
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                })
            } else {
                None
            }
        }
        "comment" => {
            if position.line == 0 && INHERITS_REGEX.is_match(&capture_text) {
                if let Some(module) = get_imported_module_under_cursor(&doc, position) {
                    let range = Some(Range::new(
                        Position::new(0, module.start_col),
                        Position::new(0, module.end_col),
                    ));
                    let hover_content = if let Some(import_doc) = module
                        .uri
                        .as_ref()
                        .and_then(|uri| backend.document_map.get(uri))
                    {
                        let doc_text = import_doc.rope.to_string();
                        format!("```query\n{doc_text}\n```")
                    } else {
                        String::from("*Document not found*")
                    };
                    return Ok(Some(Hover {
                        range,
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_content,
                        }),
                    }));
                }
                return Ok(Some(Hover {
                    range,
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: (*DOCS.get("inherits").unwrap()).to_string(),
                    }),
                }));
            }
            if FORMAT_IGNORE_REGEX.is_match(&capture_text) {
                Some(Hover {
                    range,
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: (*DOCS.get("format-ignore").unwrap()).to_string(),
                    }),
                })
            } else {
                None
            }
        }
        _ => None,
    })
}

#[cfg(test)]
mod test {
    use std::collections::{BTreeMap, HashMap};

    use ts_query_ls::{
        Options, ParameterConstraint, Predicate, PredicateParameter, PredicateParameterArity,
        PredicateParameterType,
    };

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{
        Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Range,
        TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
        request::HoverRequest,
    };

    use crate::test_helpers::helpers::{QUERY_TEST_URI, TestService, initialize_server};

    const SOURCE: &str = r"(ERROR) @error (definition) @node

(definition/test) @node

(MISSING definition) @node

(_) @any
_ @any

(function . (identifier)?)

(function (identifier)+)* @cap

[ (number) (boolean) ] @const

((number) @const (.set! foo bar))

(identifier !fieldname)

((number) @const (#eq? @const self))
";

    #[rstest]
    #[case(SOURCE, Position { line: 0, character: 2 }, Range::new(
        Position { line: 0, character: 1 },
        Position { line: 0, character: 6 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/error.md"
    )), BTreeMap::default())]
    #[case(SOURCE, Position { line: 4, character: 4 }, Range::new(
        Position { line: 4, character: 1 },
        Position { line: 4, character: 8 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/missing.md"
    )), BTreeMap::default())]
    #[case(SOURCE, Position { line: 6, character: 1 }, Range::new(
        Position { line: 6, character: 1 },
        Position { line: 6, character: 2 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/wildcard.md"
    )), BTreeMap::default())]
    #[case(SOURCE, Position { line: 7, character: 0 }, Range::new(
        Position { line: 7, character: 0 },
        Position { line: 7, character: 1 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/wildcard.md"
    )), BTreeMap::default())]
    #[case(SOURCE, Position { line: 0, character: 17 }, Range::new(
        Position { line: 0, character: 16 },
        Position { line: 0, character: 26 } ),
    r"Subtypes of `(definition)`:

```query
(anonymous_node)
(field_definition)
(grouping)
(list)
(missing_node)
(named_node)
(predicate)
```", BTreeMap::default())]
    #[case(SOURCE, Position { line: 2, character: 4 }, Range::new(
        Position { line: 2, character: 1 },
        Position { line: 2, character: 11 } ),
    r"Subtypes of `(definition)`:

```query
(anonymous_node)
(field_definition)
(grouping)
(list)
(missing_node)
(named_node)
(predicate)
```", BTreeMap::default())]
    #[case(SOURCE, Position { line: 4, character: 10 }, Range::new(
        Position { line: 4, character: 9 },
        Position { line: 4, character: 19 } ),
    r"Subtypes of `(definition)`:

```query
(anonymous_node)
(field_definition)
(grouping)
(list)
(missing_node)
(named_node)
(predicate)
```", BTreeMap::default())]
    #[case(SOURCE, Position { line: 0, character: 10 }, Range::new(
        Position { line: 0, character: 8 },
        Position { line: 0, character: 14 } ),
    r"## `@error`

An error node", BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 9, character: 10 }, Range::new(
        Position { line: 9, character: 10 },
        Position { line: 9, character: 11 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/anchor.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 9, character: 24 }, Range::new(
        Position { line: 9, character: 24 },
        Position { line: 9, character: 25 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/quantifier.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 11, character: 24 }, Range::new(
        Position { line: 11, character: 24 },
        Position { line: 11, character: 25 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/quantifier.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 11, character: 22 }, Range::new(
        Position { line: 11, character: 22 },
        Position { line: 11, character: 23 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/quantifier.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 13, character: 0 }, Range::new(
        Position { line: 13, character: 0 },
        Position { line: 13, character: 1 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/alternation.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 13, character: 21 }, Range::new(
        Position { line: 13, character: 21 },
        Position { line: 13, character: 22 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/alternation.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, Position { line: 15, character: 18 }, Range {
        start: Position::new(15, 18),
        end: Position::new(15, 23)
    },
    "Set a property\n\n---\n\n## Parameters:\n\n- Type: `string` (required; constraint: `[\"here\", \"there\"]`)\n  - A property\n", BTreeMap::default())]
    #[case(SOURCE, Position { line: 15, character: 22 }, Range {
        start: Position::new(15, 18),
        end: Position::new(15, 23)
    },
    "Set a property\n\n---\n\n## Parameters:\n\n- Type: `string` (required; constraint: `[\"here\", \"there\"]`)\n  - A property\n", BTreeMap::default())]
    #[case(SOURCE, Position { line: 15, character: 23 }, Range::default(), "", BTreeMap::default())]
    #[case(SOURCE, Position { line: 15, character: 21 }, Range {
        start: Position::new(15, 18),
        end: Position::new(15, 23)
    },
    "Set a property\n\n---\n\n## Parameters:\n\n- Type: `string` (required; constraint: `[\"here\", \"there\"]`)\n  - A property\n", BTreeMap::default())]
    #[case(SOURCE, Position { line: 17, character: 12 }, Range {
        start: Position::new(17, 12),
        end: Position::new(17, 13),
    },
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/negation.md"
    )), BTreeMap::default())]
    #[case(SOURCE, Position { line: 19, character: 18 }, Range {
        start: Position::new(19, 18),
        end: Position::new(19, 22)
    },
    "Check for equality\n\n---\n\n## Parameters:\n\n- Type: `capture` (required)\n  - A capture\n- Type: `string` (required)\n  - A string\n", BTreeMap::default())]
    #[case(";;; inherits: foo", Position { line: 0, character: 2 }, Range {
        start: Position::new(0, 0),
        end: Position::new(0, 17)
    },
    "## Inheriting queries\n\n```query\n; inherits: foo,bar\n```\n\nQueries can inherit other queries if they have an `; inherits:` comment as the\nfirst line of the query file. The language server will then act as though the\ntext of the inherited query files was placed at the top of the document, and\nwill provide diagnostics for the text in those queries as well (calculated with\nthe language information of the parent query). Queries will always inherit\nothers of the same type (e.g. a `highlights.scm` will only import other\n`highlights.scm`, never an `injections.scm`).\n\nNote that the syntax is very sensitive; there must be _exactly one_ space after\nthe `inherits:` keyword, and there must be no spaces in-between module names.\n", BTreeMap::default())]
    #[case("
;;; inherits: foo", Position { line: 1, character: 2 }, Range {
        start: Position::new(0, 0),
        end: Position::new(0, 17)
    },
    "", BTreeMap::default())]
    #[case(";;; format-ignore", Position { line: 0, character: 2 }, Range {
        start: Position::new(0, 0),
        end: Position::new(0, 17)
    },
    "## `; format-ignore`\n\nThe formatter will ignore nodes that are preceeded by a comment starting with\n`format-ignore`.\n\n```query\n((call_expression\n  function: (identifier) @function.builtin)\n  ; format-ignore\n  (#any-of? @function.builtin\n    \"printf\"   \"printf_s\"\n    \"vprintf\"  \"vprintf_s\"\n    \"scanf\"    \"scanf_s\"\n    \"vscanf\"   \"vscanf_s\"\n    \"wprintf\"  \"wprintf_s\"\n    \"vwprintf\" \"vwprintf_s\"\n    \"wscanf\"   \"wscanf_s\"\n    \"vwscanf\"  \"vwscanf_s\"\n    \"cscanf\"   \"_cscanf\"\n    \"printw\"\n    \"scanw\"))\n```\n", BTreeMap::default())]
    #[case("; inherits: cpp", Position { line: 0, character: 13 }, Range {
        start: Position::new(0, 12),
        end: Position::new(0, 15)
    },
    "```query\n; test query\n\n(squid)\n\n```", BTreeMap::default())]
    #[case("; inherits: squidward", Position { line: 0, character: 13 }, Range {
        start: Position::new(0, 12),
        end: Position::new(0, 21)
    },
    "*Document not found*", BTreeMap::default())]
    #[case("(named_node)", Position { line: 0, character: 4 }, Range {
        start: Position::new(0, 1),
        end: Position::new(0, 11)
    }, "Symbol IDs: 40", BTreeMap::default())]
    #[case("(MISSING identifier)", Position { line: 0, character: 12 }, Range {
        start: Position::new(0, 9),
        end: Position::new(0, 19)
    }, "Symbol IDs: 6, 7", BTreeMap::default())]
    #[case("(definition/named_node)", Position { line: 0, character: 12 }, Range {
        start: Position::new(0, 12),
        end: Position::new(0, 22)
    }, "Symbol IDs: 40", BTreeMap::default())]
    #[case("\"MISSING\"", Position { line: 0, character: 0 }, Range {
        start: Position::new(0, 0),
        end: Position::new(0, 9)
    }, "Symbol IDs: 18", BTreeMap::default())]
    #[case(r#""MISSING""#, Position { line: 0, character: 2 }, Range {
        start: Position::new(0, 0),
        end: Position::new(0, 9)
    }, "Symbol IDs: 18", BTreeMap::default())]
    #[case(r#"(MISSING "MISSING")"#, Position { line: 0, character: 12 }, Range {
        start: Position::new(0, 9),
        end: Position::new(0, 18)
    }, "Symbol IDs: 18", BTreeMap::default())]
    #[case(r#"(definition/"MISSING")"#, Position { line: 0, character: 14 }, Range {
        start: Position::new(0, 12),
        end: Position::new(0, 21)
    }, "Symbol IDs: 18", BTreeMap::default())]
    #[case(r#""MIS\SING""#, Position { line: 0, character: 4 }, Range {
        start: Position::new(0, 0),
        end: Position::new(0, 10)
    }, "Symbol IDs: 18", BTreeMap::default())]
    #[case("(missing_node name: (identifier) @variable !type)", Position { line: 0, character: 15 }, Range {
        start: Position::new(0, 14),
        end: Position::new(0, 18)
    }, "Field ID: 1", BTreeMap::default())]
    #[case("(missing_node name: (identifier) @variable !type)", Position { line: 0, character: 46 }, Range {
        start: Position::new(0, 44),
        end: Position::new(0, 48)
    }, "Field ID: 5", BTreeMap::default())]
    #[tokio::test(flavor = "current_thread")]
    async fn hover(
        #[case] source: &str,
        #[case] position: Position,
        #[case] range: Range,
        #[case] hover_content: &str,
        #[case] captures: BTreeMap<String, String>,
    ) {
        // Arrange
        let mut service = initialize_server(
            &[(QUERY_TEST_URI.clone(), source)],
            &Options {
                valid_captures: HashMap::from([(String::from("test"), captures)]),
                valid_predicates: BTreeMap::from([(
                    String::from("eq"),
                    Predicate {
                        description: String::from("Check for equality"),
                        parameters: vec![
                            PredicateParameter {
                                description: Some(String::from("A capture")),
                                type_: PredicateParameterType::Capture,
                                arity: PredicateParameterArity::Required,
                                constraint: ParameterConstraint::None,
                            },
                            PredicateParameter {
                                description: Some(String::from("A string")),
                                type_: PredicateParameterType::String,
                                arity: PredicateParameterArity::Required,
                                constraint: ParameterConstraint::None,
                            },
                        ],
                    },
                )]),
                valid_directives: BTreeMap::from([(
                    String::from("set"),
                    Predicate {
                        description: String::from("Set a property"),
                        parameters: vec![PredicateParameter {
                            description: Some(String::from("A property")),
                            type_: PredicateParameterType::String,
                            arity: PredicateParameterArity::Required,
                            constraint: ParameterConstraint::Enum(vec![
                                String::from("here"),
                                String::from("there"),
                            ]),
                        }],
                    },
                )]),
                ..Default::default()
            },
        )
        .await;

        // Act
        let tokens = service
            .request::<HoverRequest>(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: QUERY_TEST_URI.clone(),
                    },
                    position,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await;

        // Assert
        let expected = if hover_content.is_empty() {
            None
        } else {
            Some(Hover {
                range: Some(range),
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from(hover_content),
                }),
            })
        };
        assert_eq!(expected, tokens,);
    }
}
