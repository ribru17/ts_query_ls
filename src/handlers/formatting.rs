use std::collections::HashSet;
use std::ops::Deref as _;
use std::sync::LazyLock;

use regex::Regex;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{DocumentFormattingParams, Range, TextEdit};
use tracing::warn;
use tree_sitter::{
    Node, Query, QueryCursor, QueryMatch, QueryPredicateArg, StreamingIterator as _, TreeCursor,
};

use crate::Backend;
use crate::QUERY_LANGUAGE;
use crate::util::{NodeUtil as _, TextProviderRope, byte_offset_to_lsp_position};

pub async fn formatting(
    backend: &Backend,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = &params.text_document.uri;
    let Some(doc) = backend.document_map.get(uri) else {
        warn!("No document found for URI: {uri} when handling formatting");
        return Ok(None);
    };
    let rope = &doc.rope;
    let root = &doc.tree.root_node();

    if let Some(formatted_doc) = format_document(rope, root) {
        Ok(Some(diff(rope.to_string().as_str(), &formatted_doc, rope)))
    } else {
        Ok(None)
    }
}

static LINE_START: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^([^\S\r\n]*)").unwrap());
static NEWLINES: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\n+").unwrap());
static COMMENT_PAT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^;+(\s*.*?)\s*$").unwrap());
static CRLF: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\r\n?").unwrap());

static FORMAT_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/queries/query/formatting.scm"
        )),
    )
    .unwrap()
});

pub fn diff(left: &str, right: &str, rope: &Rope) -> Vec<TextEdit> {
    use dissimilar::Chunk;

    let chunks = dissimilar::diff(left, right);

    let mut offset = 0;
    let mut edits = vec![];

    let mut chunks = chunks.into_iter().peekable();
    while let Some(chunk) = chunks.next() {
        if let (Chunk::Delete(deleted), Some(&Chunk::Insert(inserted))) = (chunk, chunks.peek()) {
            chunks.next().unwrap();
            let deleted_len = deleted.len();
            let start = byte_offset_to_lsp_position(offset, rope).unwrap();
            let end = byte_offset_to_lsp_position(offset + deleted_len, rope).unwrap();
            edits.push(TextEdit {
                new_text: inserted.to_owned(),
                range: Range { start, end },
            });
            offset += deleted_len;
            continue;
        }

        match chunk {
            Chunk::Equal(text) => {
                offset += text.len();
            }
            Chunk::Delete(deleted) => {
                let deleted_len = deleted.len();
                let start = byte_offset_to_lsp_position(offset, rope).unwrap();
                let end = byte_offset_to_lsp_position(offset + deleted_len, rope).unwrap();
                edits.push(TextEdit {
                    new_text: String::new(),
                    range: Range { start, end },
                });
                offset += deleted_len;
            }
            Chunk::Insert(inserted) => {
                let pos = byte_offset_to_lsp_position(offset, rope).unwrap();
                edits.push(TextEdit {
                    new_text: inserted.to_owned(),
                    range: Range {
                        start: pos,
                        end: pos,
                    },
                });
            }
        }
    }
    edits
}

const INDENT_STR: &str = "  ";
const TEXT_WIDTH: usize = 100;

fn append_lines(lines: &mut Vec<String>, lines_to_append: &[String]) {
    for (i, line) in lines_to_append.iter().enumerate() {
        lines.last_mut().unwrap().push_str(line);
        if i != lines_to_append.len() - 1 {
            lines.push(String::new());
        }
    }
}

#[derive(Default)]
struct FormatMap {
    ignore: HashSet<usize>,
    indent_begin: HashSet<usize>,
    indent_dedent: HashSet<usize>,
    prepend_space: HashSet<usize>,
    prepend_newline: HashSet<usize>,
    append_space: HashSet<usize>,
    append_newline: HashSet<usize>,
    cancel_append: HashSet<usize>,
    cancel_prepend: HashSet<usize>,
    conditional_newline: HashSet<usize>,
    lookahead_newline: HashSet<usize>,
    comment_fix: HashSet<usize>,
    make_pound: HashSet<usize>,
    remove: HashSet<usize>,
}

pub fn format_document(rope: &Rope, root: &Node) -> Option<String> {
    if root.has_error() {
        return None;
    }
    let mut map = FormatMap::default();
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(rope);
    let mut matches = cursor.matches(&FORMAT_QUERY, *root, &provider);

    'matches: while let Some(match_) = matches.next() {
        for predicate in FORMAT_QUERY.general_predicates(match_.pattern_index) {
            let keep = handle_predicate(match_, &predicate.operator, &predicate.args, rope);
            if !keep {
                continue 'matches;
            }
        }
        for capture in match_.captures {
            let name = FORMAT_QUERY.capture_names()[capture.index as usize];
            if name.starts_with('_') {
                continue;
            }
            let capture_set = match name {
                "format.ignore" => &mut map.ignore,
                "format.indent.begin" => &mut map.indent_begin,
                "format.indent.dedent" => &mut map.indent_dedent,
                "format.prepend-space" => &mut map.prepend_space,
                "format.prepend-newline" => &mut map.prepend_newline,
                "format.append-space" => &mut map.append_space,
                "format.append-newline" => &mut map.append_newline,
                "format.cancel-append" => &mut map.cancel_append,
                "format.cancel-prepend" => &mut map.cancel_prepend,
                "format.conditional-newline" => &mut map.conditional_newline,
                "format.lookahead-newline" => &mut map.lookahead_newline,
                "format.comment-fix" => &mut map.comment_fix,
                "format.make-pound" => &mut map.make_pound,
                "format.remove" => &mut map.remove,
                _ => panic!("Unsupported format &mut capture"),
            };
            capture_set.insert(capture.node.id());
        }
    }

    let mut lines = vec![String::new()];

    format_iter(rope, root, &mut lines, &map, 0, &mut root.walk());

    Some(lines.join("\n") + "\n")
}

fn format_iter<'a>(
    rope: &Rope,
    node: &Node<'a>,
    lines: &mut Vec<String>,
    map: &FormatMap,
    mut level: usize,
    cursor: &mut TreeCursor<'a>,
) {
    if !cursor.goto_first_child() {
        return;
    }

    // Sometimes 2 queries apply append twice. This is to prevent the case from happening
    let mut apply_newline = false;
    loop {
        let child = cursor.node();
        let id = &child.id();
        if apply_newline {
            apply_newline = false;
            lines.push(INDENT_STR.repeat(level));
        }
        if map.ignore.contains(id) {
            let text = CRLF
                .replace_all(child.text(rope).as_str(), "\n")
                .trim_matches('\n')
                .split('\n')
                .map(ToOwned::to_owned)
                .collect::<Vec<String>>();
            append_lines(lines, &text);
        } else if !map.remove.contains(id) {
            if !map.cancel_prepend.contains(id) {
                if map.prepend_newline.contains(id) {
                    lines.push(INDENT_STR.repeat(level));
                } else if map.prepend_space.contains(id) {
                    let byte_length = child.end_byte() - child.start_byte();
                    let broader_byte_length = node.end_byte() - child.start_byte();
                    if !map.conditional_newline.contains(id) {
                        lines.last_mut().unwrap().push(' ');
                    } else if byte_length + 1 + lines.last().unwrap().len() > TEXT_WIDTH
                        || (map.lookahead_newline.contains(id)
                            && broader_byte_length + lines.last().unwrap().len() > TEXT_WIDTH)
                    {
                        lines.push(INDENT_STR.repeat(level));
                    } else {
                        lines.last_mut().unwrap().push(' ');
                    }
                }
            }
            if map.comment_fix.contains(id) {
                let text = child.text(rope);
                if let Some(mat) = COMMENT_PAT.captures(text.as_str()) {
                    lines
                        .last_mut()
                        .unwrap()
                        .push_str([";", mat.get(1).unwrap().as_str()].concat().as_str());
                }
            } else if map.make_pound.contains(id) {
                lines.last_mut().unwrap().push('#');
            // Stop recursively formatting on leaf nodes (or strings, which should not have their
            // inner content touched)
            } else if child.child_count() == 0 || child.kind() == "string" {
                let text = NEWLINES
                    .split(
                        CRLF.replace_all(child.text(rope).as_str(), "\n")
                            .trim_matches('\n'),
                    )
                    .map(ToOwned::to_owned)
                    .collect::<Vec<String>>();
                append_lines(lines, &text);
            } else {
                format_iter(rope, &child, lines, map, level, cursor);
            }
            if map.indent_begin.contains(id) {
                level += 1;
                apply_newline = true;
            } else if map.indent_dedent.contains(id) {
                lines.last_mut().unwrap().drain(0..2);
            }
        }
        if map.cancel_append.contains(id) {
            apply_newline = false;
        } else if map.append_newline.contains(id) {
            apply_newline = true;
        } else if map.append_space.contains(id) {
            lines.last_mut().unwrap().push(' ');
        }

        if !cursor.goto_next_sibling() {
            break;
        }
    }

    cursor.goto_parent();
}

fn handle_predicate(
    match_: &QueryMatch,
    directive: &str,
    args: &std::boxed::Box<[tree_sitter::QueryPredicateArg]>,
    rope: &Rope,
) -> bool {
    match directive {
        "is-start-of-line?" | "not-is-start-of-line?" => {
            if let QueryPredicateArg::Capture(cap_idx) = &args[0] {
                let range = match_
                    .nodes_for_capture_index(*cap_idx)
                    .next()
                    .unwrap()
                    .range();
                let line = rope.line(range.start_point.row).to_string();
                let pre_whitespace = LINE_START
                    .captures(line.as_str())
                    .and_then(|c| c.get(1))
                    .map_or(0, |m| m.len());
                let is_start = pre_whitespace == range.start_point.column;
                if directive == "not-is-start-of-line?" {
                    return !is_start;
                }
                return is_start;
            }
            true
        }
        "not-kind-eq?" => {
            if let QueryPredicateArg::Capture(cap_idx) = &args[0] {
                let node_type = match match_.nodes_for_capture_index(*cap_idx).next() {
                    None => return true,
                    Some(node) => node.kind(),
                };
                for arg in &args[1..] {
                    if let QueryPredicateArg::String(kind) = arg {
                        if node_type == kind.deref() {
                            return false;
                        }
                    }
                }
            }
            true
        }
        &_ => false,
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
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_trailing_whitespace.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_trailing_whitespace.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_predicates.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_predicates.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_missing.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_missing.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_syntax_error.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_syntax_error.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_complex.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_complex.scm")),
    )]
    #[tokio::test(flavor = "current_thread")]
    async fn server_formatting(#[case] before: &str, #[case] after: &str) {
        // Arrange
        let mut service = initialize_server(
            &[(
                TEST_URI.clone(),
                before,
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            )],
            &Default::default(),
        )
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
        assert_eq!(doc.rope.to_string(), String::from(after));
    }
}
