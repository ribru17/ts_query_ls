use std::{collections::HashMap, sync::LazyLock};

use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind},
};
use tracing::warn;
use tree_sitter::Query;

use crate::{
    Backend, QUERY_LANGUAGE, SymbolInfo,
    util::{NodeUtil, ToTsPoint, capture_at_pos, uri_to_basename},
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
);

pub async fn hover(backend: &Backend, params: HoverParams) -> Result<Option<Hover>> {
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
            let value = DOCS.get(doc_name).unwrap().to_string();
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
                let value = format!("## `{}`\n\n{}", capture_text, description);
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
                        |acc, subtype| format!("{acc}\n{}", subtype),
                    ) + "\n```"
                };
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
                    value += format!("- Type: `{}` ({})\n", param.type_, param.arity).as_str();
                    if let Some(desc) = &param.description {
                        value += format!("  - {}\n", desc).as_str();
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
        _ => None,
    })
}

#[cfg(test)]
mod test {
    use std::collections::{BTreeMap, HashMap};

    use ts_query_ls::{
        Options, Predicate, PredicateParameter, PredicateParameterArity, PredicateParameterType,
    };

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Range,
        TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
        request::HoverRequest,
    };

    use crate::test_helpers::helpers::{
        TEST_URI, initialize_server, lsp_request_to_jsonrpc_request,
        lsp_response_to_jsonrpc_response,
    };

    const SOURCE: &str = r"(ERROR) @error (supertype) @node

(supertype/test) @node

(MISSING supertype) @node

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
    #[case(SOURCE, vec!["supertype"], Position { line: 0, character: 2 }, Range::new(
        Position { line: 0, character: 1 },
        Position { line: 0, character: 6 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/error.md"
    )), Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 4, character: 4 }, Range::new(
        Position { line: 4, character: 1 },
        Position { line: 4, character: 8 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/missing.md"
    )), Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 6, character: 1 }, Range::new(
        Position { line: 6, character: 1 },
        Position { line: 6, character: 2 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/wildcard.md"
    )), Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 7, character: 0 }, Range::new(
        Position { line: 7, character: 0 },
        Position { line: 7, character: 1 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/wildcard.md"
    )), Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 0, character: 17 }, Range::new(
        Position { line: 0, character: 16 },
        Position { line: 0, character: 25 } ),
    r"Subtypes of `(supertype)`:

```query
(test)
(test2)
```", Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 2, character: 4 }, Range::new(
        Position { line: 2, character: 1 },
        Position { line: 2, character: 10 } ),
    r"Subtypes of `(supertype)`:

```query
(test)
(test2)
```", Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 4, character: 10 }, Range::new(
        Position { line: 4, character: 9 },
        Position { line: 4, character: 18 } ),
    r"Subtypes of `(supertype)`:

```query
(test)
(test2)
```", Default::default())]
    #[case(SOURCE, vec!["supertype"], Position { line: 0, character: 10 }, Range::new(
        Position { line: 0, character: 8 },
        Position { line: 0, character: 14 } ),
    r"## `@error`

An error node", BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 9, character: 10 }, Range::new(
        Position { line: 9, character: 10 },
        Position { line: 9, character: 11 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/anchor.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 9, character: 24 }, Range::new(
        Position { line: 9, character: 24 },
        Position { line: 9, character: 25 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/quantifier.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 11, character: 24 }, Range::new(
        Position { line: 11, character: 24 },
        Position { line: 11, character: 25 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/quantifier.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 11, character: 22 }, Range::new(
        Position { line: 11, character: 22 },
        Position { line: 11, character: 23 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/quantifier.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 13, character: 0 }, Range::new(
        Position { line: 13, character: 0 },
        Position { line: 13, character: 1 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/alternation.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 13, character: 21 }, Range::new(
        Position { line: 13, character: 21 },
        Position { line: 13, character: 22 } ),
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/alternation.md"
    )), BTreeMap::from([(String::from("error"), String::from("An error node"))]))]
    #[case(SOURCE, vec![], Position { line: 15, character: 18 }, Range {
        start: Position::new(15, 18),
        end: Position::new(15, 23)
    },
    "Set a property\n\n---\n\n## Parameters:\n\n- Type: `string` (required)\n  - A property\n", BTreeMap::default())]
    #[case(SOURCE, vec![], Position { line: 15, character: 22 }, Range {
        start: Position::new(15, 18),
        end: Position::new(15, 23)
    },
    "Set a property\n\n---\n\n## Parameters:\n\n- Type: `string` (required)\n  - A property\n", BTreeMap::default())]
    #[case(SOURCE, vec![], Position { line: 15, character: 23 }, Range::default(), "", BTreeMap::default())]
    #[case(SOURCE, vec![], Position { line: 15, character: 21 }, Range {
        start: Position::new(15, 18),
        end: Position::new(15, 23)
    },
    "Set a property\n\n---\n\n## Parameters:\n\n- Type: `string` (required)\n  - A property\n", BTreeMap::default())]
    #[case(SOURCE, vec![], Position { line: 17, character: 12 }, Range {
        start: Position::new(17, 12),
        end: Position::new(17, 13),
    },
    include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/negation.md"
    )), BTreeMap::default())]
    #[case(SOURCE, vec![], Position { line: 19, character: 18 }, Range {
        start: Position::new(19, 18),
        end: Position::new(19, 22)
    },
    "Check for equality\n\n---\n\n## Parameters:\n\n- Type: `capture` (required)\n  - A capture\n- Type: `string` (required)\n  - A string\n", BTreeMap::default())]
    #[tokio::test(flavor = "current_thread")]
    async fn hover(
        #[case] source: &str,
        #[case] supertypes: Vec<&str>,
        #[case] position: Position,
        #[case] range: Range,
        #[case] hover_content: &str,
        #[case] captures: BTreeMap<String, String>,
    ) {
        // Arrange
        let mut service = initialize_server(
            &[(TEST_URI.clone(), source, Vec::new(), Vec::new(), supertypes)],
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
                            },
                            PredicateParameter {
                                description: Some(String::from("A string")),
                                type_: PredicateParameterType::String,
                                arity: PredicateParameterArity::Required,
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
                        }],
                    },
                )]),
                ..Default::default()
            },
        )
        .await;

        // Act
        let tokens = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<HoverRequest>(
                HoverParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                        },
                        position,
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                },
            ))
            .await
            .unwrap();

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
        assert_eq!(
            Some(lsp_response_to_jsonrpc_response::<HoverRequest>(expected)),
            tokens,
        );
    }
}
