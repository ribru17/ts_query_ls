use std::{collections::HashMap, vec};

use ropey::Rope;
use serde::{Deserialize, Serialize};
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
        Diagnostic, Position, Range, TextEdit, Url, WorkspaceEdit,
    },
};
use tree_sitter::{QueryCursor, Tree};

use crate::{
    Backend, LspClient,
    util::{
        CAPTURES_QUERY, NodeUtil, PosUtil, RangeUtil, TextProviderRope, get_current_capture_node,
        get_references,
    },
};

#[repr(u8)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(into = "u8", try_from = "u8")]
pub enum CodeActions {
    RemoveBackslash,
    PrefixUnderscore,
    Remove,
    Trim,
    Enquote,
}

impl From<CodeActions> for serde_json::Value {
    fn from(value: CodeActions) -> Self {
        serde_json::to_value(value).expect("Invalid code action value")
    }
}

impl From<CodeActions> for u8 {
    fn from(e: CodeActions) -> Self {
        e as u8
    }
}

impl TryFrom<u8> for CodeActions {
    type Error = &'static str;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        match value {
            0 => Ok(CodeActions::RemoveBackslash),
            1 => Ok(CodeActions::PrefixUnderscore),
            2 => Ok(CodeActions::Remove),
            3 => Ok(CodeActions::Trim),
            4 => Ok(CodeActions::Enquote),
            _ => Err("Invalid value"),
        }
    }
}

pub fn diag_to_code_action(
    tree: &Tree,
    rope: &Rope,
    diagnostic: Diagnostic,
    uri: &Url,
) -> Option<CodeActionOrCommand> {
    match serde_json::from_value::<CodeActions>(diagnostic.data.clone()?) {
        Ok(CodeActions::RemoveBackslash) => Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: String::from("Remove unnecessary backslash"),
            kind: Some(CodeActionKind::QUICKFIX),
            is_preferred: Some(true),
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    uri.clone(),
                    vec![TextEdit {
                        new_text: String::from(""),
                        range: Range::new(
                            diagnostic.range.start,
                            Position::new(
                                diagnostic.range.start.line,
                                diagnostic.range.start.character + 1,
                            ),
                        ),
                    }],
                )])),
                ..Default::default()
            }),
            diagnostics: Some(vec![diagnostic]),
            ..Default::default()
        })),
        Ok(CodeActions::PrefixUnderscore) => {
            let root = tree.root_node();
            let current_node =
                get_current_capture_node(root, diagnostic.range.start.to_ts_point(rope))?;
            let mut cursor = QueryCursor::new();
            let provider = TextProviderRope(rope);
            let refs = get_references(
                &root,
                &current_node,
                &CAPTURES_QUERY,
                &mut cursor,
                &provider,
                rope,
            );
            let edits = refs
                .into_iter()
                .map(|node| {
                    let mut range = node.lsp_range(rope);
                    range.start.character += 1;
                    range.end.character = range.start.character;
                    TextEdit {
                        new_text: String::from("_"),
                        range,
                    }
                })
                .collect();
            Some(CodeActionOrCommand::CodeAction(CodeAction {
                title: String::from("Prefix capture name with underscore"),
                kind: Some(CodeActionKind::QUICKFIX),
                is_preferred: Some(true),
                edit: Some(WorkspaceEdit {
                    changes: Some(HashMap::from([(uri.clone(), edits)])),
                    ..Default::default()
                }),
                diagnostics: Some(vec![diagnostic]),
                ..Default::default()
            }))
        }
        Ok(CodeActions::Remove) => Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: String::from("Remove pattern"),
            kind: Some(CodeActionKind::QUICKFIX),
            is_preferred: Some(true),
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    uri.clone(),
                    vec![TextEdit {
                        new_text: String::from(""),
                        range: diagnostic.range,
                    }],
                )])),
                ..Default::default()
            }),
            diagnostics: Some(vec![diagnostic]),
            ..Default::default()
        })),
        Ok(CodeActions::Trim) => {
            let mut range = diagnostic.range;
            range.start.character += 1;
            range.end.character -= 1;
            let new_text = range.text(rope);
            Some(CodeActionOrCommand::CodeAction(CodeAction {
                title: String::from("Trim quotations from string"),
                kind: Some(CodeActionKind::QUICKFIX),
                is_preferred: Some(true),
                edit: Some(WorkspaceEdit {
                    changes: Some(HashMap::from([(
                        uri.clone(),
                        vec![TextEdit {
                            new_text,
                            range: diagnostic.range,
                        }],
                    )])),
                    ..Default::default()
                }),
                diagnostics: Some(vec![diagnostic]),
                ..Default::default()
            }))
        }
        Ok(CodeActions::Enquote) => {
            let new_text = diagnostic.range.text(rope);
            let new_text = format!("\"{new_text}\"");
            Some(CodeActionOrCommand::CodeAction(CodeAction {
                title: String::from("Add quotations"),
                kind: Some(CodeActionKind::QUICKFIX),
                is_preferred: Some(true),
                edit: Some(WorkspaceEdit {
                    changes: Some(HashMap::from([(
                        uri.clone(),
                        vec![TextEdit {
                            new_text,
                            range: diagnostic.range,
                        }],
                    )])),
                    ..Default::default()
                }),
                diagnostics: Some(vec![diagnostic]),
                ..Default::default()
            }))
        }
        Err(_) => None,
    }
}

pub async fn code_action<C: LspClient>(
    backend: &Backend<C>,
    params: CodeActionParams,
) -> Result<Option<CodeActionResponse>> {
    let uri = &params.text_document.uri;
    let diagnostics = params.context.diagnostics;
    let Some(doc) = backend.document_map.get(uri) else {
        return Ok(None);
    };

    let actions: Vec<CodeActionOrCommand> = diagnostics
        .into_iter()
        .filter_map(|diagnostic| diag_to_code_action(&doc.tree, &doc.rope, diagnostic, uri))
        .collect();

    if actions.is_empty() {
        Ok(None)
    } else {
        Ok(Some(actions))
    }
}
#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{
        CodeAction, CodeActionContext, CodeActionKind, Diagnostic, Position, Range,
        TextDocumentIdentifier, TextEdit, WorkspaceEdit,
    };
    use tower_lsp::lsp_types::{
        CodeActionOrCommand, CodeActionParams, PartialResultParams, WorkDoneProgressParams,
        request::CodeActionRequest,
    };
    use ts_query_ls::Options;

    use crate::handlers::code_action::CodeActions;
    use crate::test_helpers::helpers::{TEST_URI, TestService, initialize_server};

    #[rstest]
    #[case(r#""\p" @_somecap"#, Default::default(), Position::new(0, 2), CodeActionContext {
        diagnostics: vec![Diagnostic {
            message: String::from("bad escape"),
            range: Range::new(Position::new(0, 1), Position::new(0, 3)),
            data: Some(serde_json::to_value(CodeActions::RemoveBackslash).unwrap()),
            ..Default::default()
        }],
        ..Default::default()
    }, &[CodeActionOrCommand::CodeAction(CodeAction {
        title: String::from("Remove unnecessary backslash"),
        kind: Some(CodeActionKind::QUICKFIX),
        is_preferred: Some(true),
        diagnostics: Some(vec![Diagnostic {
            message: String::from("bad escape"),
            range: Range::new(Position::new(0, 1), Position::new(0, 3)),
            data: Some(serde_json::to_value(CodeActions::RemoveBackslash).unwrap()),
            ..Default::default()
        }]),
        edit: Some(WorkspaceEdit {
            changes: Some(HashMap::from([(
                TEST_URI.clone(),
                    vec![TextEdit {
                        range: Range::new(Position::new(0, 1), Position::new(0, 2)),
                        new_text: String::from("")
                    }]
            )])),
            ..Default::default()
        }),
        ..Default::default()
    })])]
    #[case(r#"((comment) @jsdoc_comment
  (#lua-match? @jsdoc_comment ".*"))"#, Default::default(), Position::new(0, 15), CodeActionContext {
        diagnostics: vec![Diagnostic {
            message: String::from("bad cap"),
            range: Range::new(Position::new(0, 11), Position::new(0, 24)),
            data: Some(serde_json::to_value(CodeActions::PrefixUnderscore).unwrap()),
            ..Default::default()
        }],
        ..Default::default()
    }, &[CodeActionOrCommand::CodeAction(CodeAction {
        title: String::from("Prefix capture name with underscore"),
        kind: Some(CodeActionKind::QUICKFIX),
        is_preferred: Some(true),
        diagnostics: Some(vec![Diagnostic {
            message: String::from("bad cap"),
            range: Range::new(Position::new(0, 11), Position::new(0, 24)),
            data: Some(serde_json::to_value(CodeActions::PrefixUnderscore).unwrap()),
            ..Default::default()
        }]),
        edit: Some(WorkspaceEdit {
            changes: Some(HashMap::from([(
                TEST_URI.clone(),
                    vec![TextEdit {
                        range: Range::new(Position::new(0, 12), Position::new(0, 12)),
                        new_text: String::from("_")
                    }, TextEdit {
                        range: Range::new(Position::new(1, 16), Position::new(1, 16)),
                        new_text: String::from("_")
                    }]
            ), ])),
            ..Default::default()
        }),
        ..Default::default()
    })])]
    #[case(r#"((comment) @jsdoc_comment
  (#lua-match? @jsdoc_comment "asdf"))"#, Default::default(), Position::new(1, 32), CodeActionContext {
        diagnostics: vec![Diagnostic {
            message: String::from("Unnecessary string quotation"),
            range: Range::new(Position::new(1, 30), Position::new(1, 36)),
            data: Some(serde_json::to_value(CodeActions::Trim).unwrap()),
            ..Default::default()
        }],
        ..Default::default()
    }, &[CodeActionOrCommand::CodeAction(CodeAction {
        title: String::from("Trim quotations from string"),
        kind: Some(CodeActionKind::QUICKFIX),
        is_preferred: Some(true),
        diagnostics: Some(vec![Diagnostic {
            message: String::from("Unnecessary string quotation"),
            range: Range::new(Position::new(1, 30), Position::new(1, 36)),
            data: Some(serde_json::to_value(CodeActions::Trim).unwrap()),
            ..Default::default()
        }]),
        edit: Some(WorkspaceEdit {
            changes: Some(HashMap::from([(
                TEST_URI.clone(),
                    vec![TextEdit {
                        range: Range::new(Position::new(1, 30), Position::new(1, 36)),
                        new_text: String::from("asdf")
                    }]
            ), ])),
            ..Default::default()
        }),
        ..Default::default()
    })])]
    #[tokio::test(flavor = "current_thread")]
    async fn server_code_action(
        #[case] source: &str,
        #[case] options: Options,
        #[case] cursor: Position,
        #[case] context: CodeActionContext,
        #[case] expected_code_actions: &[CodeActionOrCommand],
    ) {
        // Arrange
        let mut service = initialize_server(&[(TEST_URI.clone(), source)], &options).await;

        // Act
        let code_actions = service
            .request::<CodeActionRequest>(CodeActionParams {
                context,
                range: Range::new(cursor, cursor),
                text_document: TextDocumentIdentifier {
                    uri: TEST_URI.clone(),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await;

        // Assert
        let expected_code_actions = Some(expected_code_actions.to_vec());
        assert_eq!(expected_code_actions, code_actions)
    }
}
