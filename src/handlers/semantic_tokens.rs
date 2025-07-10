use std::sync::LazyLock;

use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        Range, SemanticToken, SemanticTokens, SemanticTokensParams, SemanticTokensRangeParams,
        SemanticTokensRangeResult, SemanticTokensResult, Url,
    },
};
use tracing::warn;
use tree_sitter::{Query, QueryCursor, StreamingIterator};

use crate::{
    Backend, QUERY_LANGUAGE, SymbolInfo,
    util::{FORMAT_IGNORE_REGEX, INHERITS_REGEX, NodeUtil, TextProviderRope, ToTsPoint},
};

static SEM_TOK_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        r#"(named_node (identifier) @ident)
(missing_node (identifier) @ident)
(comment) @comment
"#,
    )
    .unwrap()
});

pub async fn semantic_tokens_full(
    backend: &Backend,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    Ok(get_semantic_tokens(backend, params.text_document.uri, None)
        .await
        .map(Into::into))
}

pub async fn semantic_tokens_range(
    backend: &Backend,
    params: SemanticTokensRangeParams,
) -> Result<Option<SemanticTokensRangeResult>> {
    Ok(
        get_semantic_tokens(backend, params.text_document.uri, Some(params.range))
            .await
            .map(Into::into),
    )
}

async fn get_semantic_tokens(
    backend: &Backend,
    uri: Url,
    range: Option<Range>,
) -> Option<SemanticTokens> {
    let Some(doc) = backend.document_map.get(&uri) else {
        warn!("No document found for URI: {uri} when retrieving semantic tokens");
        return None;
    };
    let mut tokens = Vec::new();
    let tree = &doc.tree;
    let rope = &doc.rope;
    let language_data = doc
        .language_name
        .as_ref()
        .and_then(|name| backend.language_map.get(name));
    let supertypes = language_data.as_ref().map(|ld| &ld.supertype_map);
    let query = &SEM_TOK_QUERY;
    let mut cursor = QueryCursor::new();
    if let Some(range) = range {
        cursor.set_point_range(range.start.to_ts_point(rope)..range.end.to_ts_point(rope));
    }
    let provider = TextProviderRope(rope);
    let mut matches = cursor.matches(query, tree.root_node(), &provider);
    let mut prev_line = 0;
    let mut prev_col = 0;

    while let Some(match_) = matches.next() {
        for cap in match_.captures.iter() {
            let capture_name = SEM_TOK_QUERY.capture_names()[cap.index as usize];
            let node = &cap.node;
            let node_text = node.text(rope);
            let start_row = node.start_position().row as u32;
            let start_col = node.start_position().column as u32;
            let delta_line = start_row - prev_line;
            let delta_start = if start_row - prev_line == 0 {
                start_col - prev_col
            } else {
                start_col
            };
            match capture_name {
                // Highlight supertypes and ERROR nodes
                "ident" => {
                    // Identifiers are always ASCII characters, meaning they count for the same
                    // number of code points in each encoding (and we can count them by bytes)
                    let length = node.byte_range().len() as u32;
                    if node_text == "ERROR" {
                        tokens.push(SemanticToken {
                            delta_line,
                            delta_start,
                            length,
                            token_type: 1,
                            token_modifiers_bitset: 1,
                        });
                        prev_line = start_row;
                        prev_col = start_col;
                    } else if supertypes.is_some_and(|supertypes| {
                        supertypes.contains_key(&SymbolInfo {
                            label: node_text,
                            named: true,
                        })
                    }) {
                        tokens.push(SemanticToken {
                            delta_line,
                            delta_start,
                            length,
                            token_type: 0,
                            token_modifiers_bitset: 0,
                        });
                        prev_line = start_row;
                        prev_col = start_col;
                    }
                }
                // Highlight special comments (inherits, format ignore)
                "comment" => {
                    if let Some(fmt_ignore) = FORMAT_IGNORE_REGEX
                        .captures(&node_text)
                        .and_then(|c| c.get(1))
                    {
                        const FMT_IGNORE_LEN: u32 = 13;
                        let offset = fmt_ignore.start() as u32;
                        tokens.push(SemanticToken {
                            delta_line,
                            delta_start: delta_start + offset,
                            length: FMT_IGNORE_LEN,
                            token_type: 3,
                            token_modifiers_bitset: 0,
                        });
                        prev_line = start_row;
                        prev_col = start_col + offset;
                        continue;
                    };
                    if start_row != 0 {
                        continue;
                    }
                    let Some(mods) = INHERITS_REGEX.captures(&node_text).and_then(|c| c.get(1))
                    else {
                        continue;
                    };
                    let offset = mods.start() as u32;
                    let mods = mods.as_str().split(',');

                    // Add a token for `inherits:`
                    const INHERITS_LEN: u32 = 9;
                    let mut delta_start = delta_start + offset - INHERITS_LEN - 1;
                    tokens.push(SemanticToken {
                        delta_line: 0,
                        delta_start,
                        length: INHERITS_LEN,
                        token_type: 3,
                        token_modifiers_bitset: 0,
                    });
                    delta_start = INHERITS_LEN + 1;

                    let mut start_col = start_col + offset;
                    let mut delta_line = delta_line;
                    for module in mods {
                        // We assert that modules are valid ASCII characters, so we can index them
                        // by byte count.
                        let length = module.len() as u32;
                        tokens.push(SemanticToken {
                            delta_line,
                            delta_start,
                            length,
                            token_type: 2,
                            token_modifiers_bitset: 0,
                        });
                        start_col += length + 1;
                        delta_start = length + 1;
                        delta_line = 0;
                    }
                    prev_line = start_row;
                    prev_col = start_col;
                }
                _ => {}
            }
        }
    }

    Some(SemanticTokens {
        result_id: None,
        data: tokens,
    })
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        PartialResultParams, SemanticToken, SemanticTokens, SemanticTokensParams,
        SemanticTokensResult, TextDocumentIdentifier, WorkDoneProgressParams,
        request::SemanticTokensFullRequest,
    };

    use crate::test_helpers::helpers::{
        TEST_URI, initialize_server, lsp_request_to_jsonrpc_request,
        lsp_response_to_jsonrpc_response,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn semantic_tokens_full() {
        // Arrange
        let source = r"; inherits: c,cuda

(ERROR) @error (supertype) @node (supertype) @node

(supertype) @node

; Weird
(MISSING ERROR) @missingerror

(MISSING supertype) @missingsupertype

;;;format-ignore
(foo)
        ";
        let mut service = initialize_server(
            &[(TEST_URI.clone(), source)],
            &[(
                String::from("js"),
                Vec::new(),
                Vec::new(),
                vec!["supertype"],
            )],
            &Default::default(),
        )
        .await;

        // Act
        let tokens = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<SemanticTokensFullRequest>(
                SemanticTokensParams {
                    partial_result_params: PartialResultParams {
                        partial_result_token: None,
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    text_document: TextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                    },
                },
            ))
            .await
            .unwrap();

        // Assert
        let actual = Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: vec![
                // ; inherits:
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 9,
                    token_type: 3,
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 10,
                    length: 1,
                    token_type: 2,
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 4,
                    token_type: 2,
                    token_modifiers_bitset: 0,
                },
                // ERROR, Supertypes
                SemanticToken {
                    delta_line: 2,
                    delta_start: 1,
                    length: 5,
                    token_type: 1,
                    token_modifiers_bitset: 1,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 15,
                    length: 9,
                    token_type: 0,
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 18,
                    length: 9,
                    token_type: 0,
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 2,
                    delta_start: 1,
                    length: 9,
                    token_type: 0,
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 3,
                    delta_start: 9,
                    length: 5,
                    token_type: 1,
                    token_modifiers_bitset: 1,
                },
                SemanticToken {
                    delta_line: 2,
                    delta_start: 9,
                    length: 9,
                    token_type: 0,
                    token_modifiers_bitset: 0,
                },
                // format-ignore
                SemanticToken {
                    delta_line: 2,
                    delta_start: 3,
                    length: 13,
                    token_type: 3,
                    token_modifiers_bitset: 0,
                },
            ],
        }));
        assert_eq!(
            tokens,
            Some(lsp_response_to_jsonrpc_response::<SemanticTokensFullRequest>(actual))
        );
    }
}
