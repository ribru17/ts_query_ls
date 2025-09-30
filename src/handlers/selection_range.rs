use tower_lsp::lsp_types::{SelectionRange, SelectionRangeParams};
use tracing::warn;

use crate::{
    Backend, LspClient,
    util::{NodeUtil, PosUtil},
};

pub fn selection_range<C: LspClient>(
    backend: &Backend<C>,
    params: &SelectionRangeParams,
) -> Option<Vec<SelectionRange>> {
    let uri = &params.text_document.uri;
    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri}");
        return None;
    };
    let tree = &doc.tree;
    let rope = &doc.rope;
    let mut results = Vec::with_capacity(params.positions.len());
    for position in &params.positions {
        let ts_point = position.to_ts_point(rope);
        let mut node = tree.root_node();
        let descendant = node
            .named_descendant_for_point_range(ts_point, ts_point)
            .unwrap_or(node);
        let mut selection_range = SelectionRange {
            parent: None,
            range: node.lsp_range(rope),
        };
        while let Some(child) = node.child_with_descendant(descendant) {
            node = child;

            let range = node.lsp_range(rope);
            if range == selection_range.range {
                continue;
            }

            let new_selection_range = SelectionRange {
                range,
                parent: Some(selection_range.into()),
            };
            selection_range = new_selection_range;
        }
        results.push(selection_range);
    }
    Some(results)
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower_lsp::lsp_types::{
        PartialResultParams, Position, Range, SelectionRange, TextDocumentIdentifier,
        WorkDoneProgressParams,
    };

    use tower_lsp::lsp_types::{SelectionRangeParams, request::SelectionRangeRequest};

    use crate::{
        Options,
        test_helpers::helpers::{
            COMPLEX_FILE, SIMPLE_FILE, TEST_URI, TestService, initialize_server,
        },
    };

    #[rstest]
    #[case(
        &SIMPLE_FILE,
        vec![Position { line: 1, character: 13 }],
        Some(vec![vec![
            Range::new(Position::new(0, 0), Position::new(3, 0)),
            Range::new(Position::new(0, 0), Position::new(1, 31)),
            Range::new(Position::new(1, 1), Position::new(1, 30)),
            Range::new(Position::new(1, 10), Position::new(1, 29)),
            Range::new(Position::new(1, 10), Position::new(1, 19)),
            Range::new(Position::new(1, 11), Position::new(1, 19)),
        ]])
    )]
    #[case(
        &COMPLEX_FILE,
        vec![
            Position { line: 1, character: 13 },
            Position { line: 8, character: 20 },
        ],
        Some(vec![vec![
            Range::new(Position::new(0, 0), Position::new(32, 0)),
            Range::new(Position::new(0, 0), Position::new(1, 39)),
            Range::new(Position::new(1, 2), Position::new(1, 38)),
            Range::new(Position::new(1, 9), Position::new(1, 37)),
            Range::new(Position::new(1, 9), Position::new(1, 27)),
        ], vec![
            Range::new(Position::new(0, 0), Position::new(32, 0)),
            Range::new(Position::new(4, 0), Position::new(18, 43)),
            Range::new(Position::new(6, 2), Position::new(10, 3)),
            Range::new(Position::new(6, 13), Position::new(10, 3)),
            Range::new(Position::new(7, 4), Position::new(8, 43)),
            Range::new(Position::new(8, 6), Position::new(8, 42)),
            Range::new(Position::new(8, 7), Position::new(8, 22)),
        ]])
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_selection_range(
        #[case] document_text: &str,
        #[case] positions: Vec<Position>,
        #[case] expected_ranges: Option<Vec<Vec<Range>>>,
    ) {
        // Arrange
        let mut service =
            initialize_server(&[(TEST_URI.clone(), document_text)], &Options::default()).await;
        let expected_selection_ranges = if let Some(ranges_list) = expected_ranges {
            let mut results = Vec::new();
            for ranges in ranges_list {
                let mut ranges = ranges.into_iter();
                let first = ranges.next().expect("ranges must not be empty");
                let result = ranges.fold(
                    SelectionRange {
                        range: first,
                        parent: None,
                    },
                    |parent, range| SelectionRange {
                        range,
                        parent: Some(parent.into()),
                    },
                );
                results.push(result);
            }
            Some(results)
        } else {
            None
        };

        // Act
        let selection_ranges = service
            .request::<SelectionRangeRequest>(SelectionRangeParams {
                text_document: TextDocumentIdentifier {
                    uri: TEST_URI.clone(),
                },
                positions,
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await;

        // Assert
        assert_eq!(expected_selection_ranges, selection_ranges);
    }
}
