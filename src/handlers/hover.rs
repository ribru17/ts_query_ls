use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind},
};

use crate::{
    util::{NodeUtil, ToTsPoint},
    Backend, SymbolInfo,
};

pub async fn hover(backend: &Backend, params: HoverParams) -> Result<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    if let (Some(tree), Some(rope), Some(supertypes)) = (
        backend.cst_map.get(uri),
        &backend.document_map.get(uri),
        backend.supertype_map_map.get(uri),
    ) {
        let Some(node) = tree
            .root_node()
            .descendant_for_point_range(position.to_ts_point(rope), position.to_ts_point(rope))
        else {
            return Ok(None);
        };
        let node_text = node.text(rope);
        let node_range = node.lsp_range(rope);
        let sym = SymbolInfo {
            label: node_text.clone(),
            named: true,
        };

        // TODO: Support documentation for default predicates.
        if node.kind() == "identifier"
            && node
                .parent()
                .is_some_and(|p| p.kind() == "named_node" || p.kind() == "missing_node")
        {
            if let Some(subtypes) = supertypes.get(&sym).and_then(|subtypes| {
                (subtypes.iter().fold(
                    format!("Subtypes of `({node_text})`:\n\n```query"),
                    |acc, subtype| format!("{acc}\n{}", subtype),
                ) + "\n```")
                    .into()
            }) {
                return Ok(Some(Hover {
                    range: Some(node_range),
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: subtypes,
                    }),
                }));
            } else if node_text == "ERROR" {
                return Ok(Some(Hover {
                    range: Some(node_range),
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: String::from(
                            r"### The `ERROR` Node

When the parser encounters text it does not recognize, it represents this node
as `(ERROR)` in the syntax tree. These error nodes can be queried just like
normal nodes:

```query
(ERROR) @error-node
```",
                        ),
                    }),
                }));
            }
        } else if node.kind() == "MISSING" {
            return Ok(Some(Hover {
                range: Some(node_range),
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from(
                        r"### The `MISSING` Node

If the parser is able to recover from erroneous text by inserting a missing token and then reducing, it will insert that
missing node in the final tree so long as that tree has the lowest error cost. These missing nodes appear as seemingly normal
nodes in the tree, but they are zero tokens wide, and are internally represented as a property of the actual terminal node
that was inserted, instead of being its own kind of node, like the `ERROR` node. These special missing nodes can be queried
using `(MISSING)`:

```query
(MISSING) @missing-node
```",
                    ),
                }),
            }));
        } else if node.kind() == "_" {
            return Ok(Some(Hover {
                range: Some(node_range),
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: String::from(
                        r"### The Wildcard Node

A wildcard node is represented with an underscore (`_`), it matches any node.
This is similar to `.` in regular expressions.
There are two types, `(_)` will match any named node,
and `_` will match any named or anonymous node.

For example, this pattern would match any node inside a call:

```query
(call (_) @call.inner)
```",
                    ),
                }),
            }));
        }
    }

    Ok(None)
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        request::HoverRequest, Hover, HoverContents, HoverParams, MarkupContent, MarkupKind,
        Position, Range, TextDocumentIdentifier, TextDocumentPositionParams,
        WorkDoneProgressParams,
    };

    use crate::test_helpers::helpers::{
        initialize_server, lsp_request_to_jsonrpc_request, lsp_response_to_jsonrpc_response,
        TEST_URI,
    };

    const SOURCE: &str = r"(ERROR) @error (supertype) @node

(supertype/test) @node

(MISSING supertype) @node

(_) @any
_ @any";

    #[rstest]
    #[case(SOURCE, vec!["supertype"], Position { line: 0, character: 2 }, Range::new(
        Position { line: 0, character: 1 },
        Position { line: 0, character: 6 } ),
    r"### The `ERROR` Node

When the parser encounters text it does not recognize, it represents this node
as `(ERROR)` in the syntax tree. These error nodes can be queried just like
normal nodes:

```query
(ERROR) @error-node
```")]
    #[case(SOURCE, vec!["supertype"], Position { line: 4, character: 4 }, Range::new(
        Position { line: 4, character: 1 },
        Position { line: 4, character: 8 } ),
    r"### The `MISSING` Node

If the parser is able to recover from erroneous text by inserting a missing token and then reducing, it will insert that
missing node in the final tree so long as that tree has the lowest error cost. These missing nodes appear as seemingly normal
nodes in the tree, but they are zero tokens wide, and are internally represented as a property of the actual terminal node
that was inserted, instead of being its own kind of node, like the `ERROR` node. These special missing nodes can be queried
using `(MISSING)`:

```query
(MISSING) @missing-node
```")]
    #[case(SOURCE, vec!["supertype"], Position { line: 6, character: 1 }, Range::new(
        Position { line: 6, character: 1 },
        Position { line: 6, character: 2 } ),
    r"### The Wildcard Node

A wildcard node is represented with an underscore (`_`), it matches any node.
This is similar to `.` in regular expressions.
There are two types, `(_)` will match any named node,
and `_` will match any named or anonymous node.

For example, this pattern would match any node inside a call:

```query
(call (_) @call.inner)
```")]
    #[case(SOURCE, vec!["supertype"], Position { line: 7, character: 0 }, Range::new(
        Position { line: 7, character: 0 },
        Position { line: 7, character: 1 } ),
    r"### The Wildcard Node

A wildcard node is represented with an underscore (`_`), it matches any node.
This is similar to `.` in regular expressions.
There are two types, `(_)` will match any named node,
and `_` will match any named or anonymous node.

For example, this pattern would match any node inside a call:

```query
(call (_) @call.inner)
```")]
    #[case(SOURCE, vec!["supertype"], Position { line: 0, character: 17 }, Range::new(
        Position { line: 0, character: 16 },
        Position { line: 0, character: 25 } ),
    r"Subtypes of `(supertype)`:

```query
(test)
(test2)
```")]
    #[case(SOURCE, vec!["supertype"], Position { line: 2, character: 4 }, Range::new(
        Position { line: 2, character: 1 },
        Position { line: 2, character: 10 } ),
    r"Subtypes of `(supertype)`:

```query
(test)
(test2)
```")]
    #[case(SOURCE, vec!["supertype"], Position { line: 4, character: 10 }, Range::new(
        Position { line: 4, character: 9 },
        Position { line: 4, character: 18 } ),
    r"Subtypes of `(supertype)`:

```query
(test)
(test2)
```")]
    #[tokio::test(flavor = "current_thread")]
    async fn hover(
        #[case] source: &str,
        #[case] supertypes: Vec<&str>,
        #[case] position: Position,
        #[case] range: Range,
        #[case] hover_content: &str,
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), source, Vec::new(), Vec::new(), supertypes)])
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
        let actual = Some(Hover {
            range: Some(range),
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: String::from(hover_content),
            }),
        });
        assert_eq!(
            tokens,
            Some(lsp_response_to_jsonrpc_response::<HoverRequest>(actual))
        );
    }
}
