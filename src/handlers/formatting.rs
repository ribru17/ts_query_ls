use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{DocumentFormattingParams, TextEdit};

use crate::{
    Backend,
    util::{diff, format_document},
};

pub async fn formatting(
    backend: &Backend,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;
    let tree = match backend.cst_map.get(&uri) {
        None => return Ok(None),
        Some(val) => val,
    };
    let rope = match backend.document_map.get(&uri) {
        None => return Ok(None),
        Some(val) => val,
    };

    if let Some(formatted_doc) = format_document(&rope, &tree) {
        Ok(Some(diff(rope.to_string().as_str(), &formatted_doc, &rope)))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        DidChangeTextDocumentParams, DocumentFormattingParams, FormattingOptions,
        TextDocumentContentChangeEvent, TextDocumentIdentifier, VersionedTextDocumentIdentifier,
        WorkDoneProgressParams, notification::DidChangeTextDocument, request::Formatting,
    };

    use crate::test_helpers::helpers::{
        TEST_URI, initialize_server, jsonrpc_response_to_lsp_value,
        lsp_notification_to_jsonrpc_request, lsp_request_to_jsonrpc_request,
    };

    #[rstest]
    #[case(
        r"(    node   
            )         @cap                 
;;;; comment     ",
        r"(node) @cap

; comment"
    )]
    #[case(
        r#"   (
        (   identifier  )
        @type
  (   .lua-match?   @type"^[A-Z]"))"#,
        r#"((identifier) @type
  (#lua-match? @type "^[A-Z]"))"#
    )]
    #[case(
        r#" ( MISSING    "somenode" )    @missing
 (cap) @node"#,
        r#"(MISSING "somenode") @missing

(cap) @node"#
    )]
    #[case(
        r#" ( MISSING    "somenode" ))    @missing
 (cap) @node"#,
        r#" ( MISSING    "somenode" ))    @missing
 (cap) @node"#
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_formatting(#[case] before: &str, #[case] after: &str) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), before, Vec::new(), Vec::new(), Vec::new())])
                .await;

        // Act
        let delta = service
            .ready()
            .await
            .unwrap()
            .call(lsp_request_to_jsonrpc_request::<Formatting>(
                DocumentFormattingParams {
                    text_document: TextDocumentIdentifier {
                        uri: TEST_URI.clone(),
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                    options: FormattingOptions::default(),
                },
            ))
            .await
            .unwrap();
        let mut edits =
            jsonrpc_response_to_lsp_value::<Formatting>(delta.unwrap()).unwrap_or_default();
        edits.sort_by(|a, b| {
            let range_a = a.range;
            let range_b = b.range;
            range_b.start.cmp(&range_a.start)
        });
        service
            .ready()
            .await
            .unwrap()
            .call(
                lsp_notification_to_jsonrpc_request::<DidChangeTextDocument>(
                    DidChangeTextDocumentParams {
                        text_document: VersionedTextDocumentIdentifier {
                            uri: TEST_URI.clone(),
                            version: 1,
                        },
                        content_changes: edits
                            .iter()
                            .map(|e| TextDocumentContentChangeEvent {
                                range: Some(e.range),
                                text: e.new_text.clone(),
                                range_length: None,
                            })
                            .collect(),
                    },
                ),
            )
            .await
            .unwrap();

        // Assert
        let doc = service.inner().document_map.get(&TEST_URI).unwrap();
        assert_eq!(doc.to_string(), String::from(after));
    }
}
