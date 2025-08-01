use std::fs;

use ropey::Rope;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        Location, ProgressParams, ProgressParamsValue, SymbolInformation, SymbolKind, Url,
        WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd, WorkDoneProgressReport,
        WorkspaceSymbolParams, notification::Progress,
    },
};
use tree_sitter::{QueryCursor, StreamingIterator};

use crate::{
    Backend, LspClient,
    util::{
        CAPTURES_QUERY, NodeUtil, TextProviderRope, get_scm_files, get_work_done_token,
        is_subsequence, parse,
    },
};

pub async fn symbol<C: LspClient>(
    backend: &Backend<C>,
    params: WorkspaceSymbolParams,
) -> Result<Option<Vec<SymbolInformation>>> {
    let mut symbols = Vec::new();
    let query = params.query;
    let token =
        get_work_done_token(backend, params.work_done_progress_params.work_done_token).await;

    let dirs = backend
        .workspace_paths
        .read()
        .as_deref()
        .cloned()
        .unwrap_or_default();

    let files = get_scm_files(&dirs).collect::<Vec<_>>();
    let file_count = files.len();
    let file_count_div_100 = file_count as f64 * 0.01;
    let mut num_processed_files = 0;
    let mut progress_percent = 0;

    if let Some(token) = token.clone() {
        backend
            .client
            .send_notification::<Progress>(ProgressParams {
                token,
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                    WorkDoneProgressBegin {
                        title: "Finding workspace symbols".into(),
                        percentage: Some(0),
                        message: Some(format!("0/{file_count} files indexed")),
                        cancellable: Some(false),
                    },
                )),
            })
            .await
    }

    for path in files.into_iter() {
        let container_name = path
            .file_name()
            .and_then(|f| f.to_str())
            .map(ToString::to_string);
        if let (Ok(content), Ok(uri)) = (fs::read_to_string(&path), Url::from_file_path(path)) {
            let rope = &Rope::from_str(&content);
            let tree = parse(rope, None);

            let provider = &TextProviderRope(rope);
            let mut cursor = QueryCursor::new();
            let mut matches = cursor.matches(&CAPTURES_QUERY, tree.root_node(), provider);

            while let Some(match_) = matches.next() {
                for capture in match_.captures {
                    let capture_node = capture.node;
                    let node_text = capture_node.text(rope);

                    if is_subsequence(&query, &node_text) {
                        symbols.push(SymbolInformation {
                            name: node_text,
                            kind: SymbolKind::VARIABLE,
                            location: Location::new(uri.clone(), capture_node.lsp_range(rope)),
                            container_name: container_name.clone(),
                            tags: None,
                            #[allow(deprecated)]
                            deprecated: None,
                        });
                    }
                }
            }
        }
        num_processed_files += 1;
        let percentage = (num_processed_files as f64 / file_count_div_100).floor() as u32;
        if percentage > progress_percent + 4 {
            progress_percent = percentage;
            if let Some(token) = token.clone() {
                backend
                    .client
                    .send_notification::<Progress>(ProgressParams {
                        token,
                        value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                            WorkDoneProgressReport {
                                percentage: Some(progress_percent),
                                message: Some(format!(
                                    "{num_processed_files}/{file_count} files indexed"
                                )),
                                cancellable: Some(false),
                            },
                        )),
                    })
                    .await
            }
        }
    }

    if let Some(token) = token.clone() {
        backend
            .client
            .send_notification::<Progress>(ProgressParams {
                token,
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(format!("{file_count}/{file_count} files indexed")),
                })),
            })
            .await
    }
    Ok(Some(symbols))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use tower_lsp::lsp_types::{
        Location, NumberOrString, PartialResultParams, Position, ProgressParams,
        ProgressParamsValue, Range, SymbolInformation, SymbolKind, Url, WorkDoneProgress,
        WorkDoneProgressCreateParams, WorkDoneProgressEnd, WorkDoneProgressParams,
        WorkDoneProgressReport, WorkspaceSymbolParams, WorkspaceSymbolResponse,
        notification::Progress,
        request::{WorkDoneProgressCreate, WorkspaceSymbolRequest},
    };

    use crate::test_helpers::helpers::{MockRequest, TestService, initialize_server};

    #[tokio::test(flavor = "current_thread")]
    async fn workspace_symbol() {
        // Arrange
        let mut service = initialize_server(&[], &Default::default()).await;
        fn make_range(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Range {
            Range::new(
                Position::new(start_line, start_col),
                Position::new(end_line, end_col),
            )
        }

        // Act
        let actual_tokens = service
            .request::<WorkspaceSymbolRequest>(WorkspaceSymbolParams {
                query: String::from(""),
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await;

        // Assert
        let cpp_folds_uri = Url::from_file_path(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/test_workspace/queries/cpp/folds.scm"
        ))
        .unwrap();
        let other_highlights_uri = Url::from_file_path(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/test_workspace/queries/other/highlights.scm"
        ))
        .unwrap();
        #[allow(deprecated)]
        let expected_tokens = Some(WorkspaceSymbolResponse::Flat(vec![
            SymbolInformation {
                name: String::from("@fold.region"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: cpp_folds_uri.clone(),
                    range: make_range(0, 22, 0, 34),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("folds.scm")),
            },
            SymbolInformation {
                name: String::from("@fold.imports"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: cpp_folds_uri.clone(),
                    range: make_range(2, 20, 2, 33),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("folds.scm")),
            },
            SymbolInformation {
                name: String::from("@variable"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: other_highlights_uri.clone(),
                    range: make_range(5, 2, 5, 11),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("highlights.scm")),
            },
            SymbolInformation {
                name: String::from("@function"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: other_highlights_uri.clone(),
                    range: make_range(7, 11, 7, 20),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("highlights.scm")),
            },
            SymbolInformation {
                name: String::from("@constant"),
                kind: SymbolKind::VARIABLE,
                location: Location {
                    uri: other_highlights_uri.clone(),
                    range: make_range(13, 2, 13, 11),
                },
                deprecated: None,
                tags: None,
                container_name: Some(String::from("highlights.scm")),
            },
        ]));
        assert_eq!(expected_tokens, actual_tokens);
        assert_eq!(
            service.inner().client.get_requests()[0],
            MockRequest::from_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                token: NumberOrString::String(String::from("00000000-1111-2222-3333-444444444444"))
            })
        );
        assert!(service.inner().client.get_notifications().contains(
            &MockRequest::from_notification::<Progress>(ProgressParams {
                token: NumberOrString::String(String::from("00000000-1111-2222-3333-444444444444")),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                    WorkDoneProgressReport {
                        message: Some(String::from("3/4 files indexed")),
                        cancellable: Some(false),
                        percentage: Some(75)
                    }
                ))
            })
        ));
        assert!(service.inner().client.get_notifications().contains(
            &MockRequest::from_notification::<Progress>(ProgressParams {
                token: NumberOrString::String(String::from("00000000-1111-2222-3333-444444444444")),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(String::from("4/4 files indexed"))
                }))
            })
        ));
    }
}
