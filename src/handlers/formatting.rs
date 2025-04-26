use std::collections::{HashMap, HashSet};
use std::ops::Deref as _;
use std::sync::LazyLock;

use regex::Regex;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{DocumentFormattingParams, Range, TextEdit};
use tree_sitter::{
    Node, Query, QueryCursor, QueryMatch, QueryPredicateArg, StreamingIterator as _, Tree,
    TreeCursor,
};

use crate::Backend;
use crate::QUERY_LANGUAGE;
use crate::util::{NodeUtil as _, TextProviderRope, byte_offset_to_lsp_position};

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

static LINE_START: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^([^\S\r\n]*)").unwrap());
static NEWLINES: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\n+").unwrap());
static COMMENT_PAT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^;+(\s*.*?)\s*$").unwrap());
static CRLF: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\r\n?").unwrap());

static FORMAT_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        r#"
;;query
;; Ignore next node with `; format-ignore`
(
  (comment) @_pattern
  .
  (_) @format.ignore
  (#match? @_pattern "^;+\\s*format\\-ignore"))

;; Add newlines to top level nodes
;; Preserve inline comments
(program
  . (_)
  (comment) @format.prepend-newline
  (#is-start-of-line? @format.prepend-newline))
(program
  . (_)
  (comment) @_comment
  .
  (comment) @format.prepend-newline
  (#not-is-start-of-line? @_comment)
  (#is-start-of-line? @format.prepend-newline))
;; Making sure all top-level patterns are separated
(program
  (_) @format.append-newline)
(program
  (_) @format.cancel-append .)
(program
  . (_)
  [
    (list)
    (grouping)
    (named_node)
    (anonymous_node)
    (missing_node)
    (field_definition)
  ] @format.prepend-newline)

(program
  (comment) @_comment
  .
  [
    (list)
    (grouping)
    (named_node)
    (anonymous_node)
    (missing_node)
    (field_definition)
    (comment)
  ] @format.cancel-prepend
  (#is-start-of-line? @_comment)
  (#not-match? @_comment "^;+\\s*inherits:")
  (#not-match? @_comment "^;+\\s*extends\\s*$"))

;; delims
[
  ":"
  "."
] @format.append-space
(predicate "." @format.cancel-append @format.make-pound)
(
  "." @format.prepend-space @format.cancel-append
  .
  ")")

;; List handler
;; Only starts indent if 2 or more elements
(list
  "[" @format.indent.begin
  "]" @format.indent.dedent)
;; Otherwise, remove brackets
(list
  "[" @format.remove @format.cancel-append
  .
  (_) @format.cancel-append
  .
  "]" @format.remove)
;; [ ... ] @capture1 @capture2
;; Append newlines for nodes inside the list
(list
  (_) @format.append-newline
  (#not-kind-eq? @format.append-newline "capture" "quantifier"))

;; (_), "_" and _ handler
;; Start indents if it's one of these patterns
(named_node
  [
    "_"
    name: (identifier)
  ] @format.indent.begin
  .
  [
    (list)              ; (foo [...])
    (grouping)          ; (foo ((foo)))
    (negated_field)     ; (foo !field)
    (field_definition)  ; (foo field: (...))
    (named_node)        ; (foo (bar))
    (predicate)         ; (named_node (#set!))
    (anonymous_node)
    (missing_node)
    "."
  ])
;; Honoring comment's position within a node
(named_node
  [
    "_"
    name: (identifier)
  ] @format.indent.begin
  .
  (comment) @_comment
  (#is-start-of-line? @_comment))
(named_node
  [
    "_"
    name: (identifier)
  ] @format.indent.begin @format.cancel-append
  .
  "."? @format.prepend-newline
  .
  (comment) @format.prepend-space
  (#not-is-start-of-line? @format.prepend-space))

;; Add newlines for other nodes, in case the top node is indented
(named_node
  [
    (list)
    (grouping)
    (negated_field)
    (field_definition)
    (named_node)
    (predicate)
    (anonymous_node)
    (missing_node)
    "."
  ] @format.append-newline)

;; Collapse closing parentheses
(named_node
  [
    "_"
    name: (identifier)
    (_)
  ] @format.cancel-append
  .
  ")"
  (#not-kind-eq? @format.cancel-append "comment"))

;; All captures should be separated with a space
(capture) @format.prepend-space

; ( (_) ) handler
(grouping
  "("
  .
  [
    (named_node)                  ; ((foo))
    (list)                        ; ([foo] (...))
    (anonymous_node)              ; ("foo")
    (missing_node)
    (grouping . (_))
  ] @format.indent.begin
  .
  (_))
(grouping
  "("
  .
  (grouping) @format.indent.begin
  (predicate))
(grouping
  "("
  [
    (anonymous_node)
    (named_node)
    (missing_node)
    (list)
    (predicate)
    (grouping . (_))
    (field_definition)
    "."
  ] @format.append-newline
  (_) .)
;; Collapsing closing parens
(grouping
  (_) @format.cancel-append . ")"
  (#not-kind-eq? @format.cancel-append "comment"))
(grouping
  (capture) @format.prepend-space)
(missing_node
  name: (_) @format.prepend-space)
;; Remove unnecessary parens
(grouping
  "(" @format.remove
  .
  (_)
  .
  ")" @format.remove .)
(grouping
  "(" @format.remove
  .
  [
    (grouping)
    (anonymous_node
      name: (string) .)
    (missing_node
      name: (_) .)
    (named_node
      [
        "_"
        name: (identifier)
      ] .)
  ]
  .
  ")" @format.remove
  .
  (capture))

; Separate this query to avoid capture duplication
(predicate
  "(" @format.indent.begin @format.cancel-append)
(predicate
  (parameters
    (comment) @format.prepend-newline
    .
    (_) @format.cancel-prepend)
  (#is-start-of-line? @format.prepend-newline))
(predicate
  (parameters
    (_) @format.prepend-space)
  (#set! conditional-newline))
(predicate
  (parameters
    .
    (capture)
    . (_) @format.prepend-space)
  (#set! lookahead-newline)
  (#set! conditional-newline))

;; Comment related handlers
(comment) @format.append-newline @format.comment-fix
;; Preserve end of line comments
(
  [
    "."
    ":"
    (list)
    (grouping)
    (named_node)
    (anonymous_node)
    (missing_node)
    (negated_field)
  ] @format.cancel-append
  .
  (quantifier)?
  .
  "."? @format.prepend-newline ; Make sure anchor are not eol but start of newline
  .
  (comment) @format.prepend-space
  (#not-is-start-of-line? @format.prepend-space))
"#,
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

pub fn format_document(rope: &Rope, tree: &Tree) -> Option<String> {
    let root = tree.root_node();
    if root.has_error() {
        return None;
    }
    let mut map: HashMap<&str, HashMap<usize, HashSet<&str>>> = HashMap::from([
        ("format.ignore", HashMap::new()),
        ("format.indent.begin", HashMap::new()),
        ("format.indent.dedent", HashMap::new()),
        ("format.prepend-space", HashMap::new()),
        ("format.prepend-newline", HashMap::new()),
        ("format.append-space", HashMap::new()),
        ("format.append-newline", HashMap::new()),
        ("format.cancel-append", HashMap::new()),
        ("format.cancel-prepend", HashMap::new()),
        ("format.comment-fix", HashMap::new()),
        ("format.make-pound", HashMap::new()),
        ("format.remove", HashMap::new()),
    ]);

    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(rope);
    let mut matches = cursor.matches(&FORMAT_QUERY, root, &provider);

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
            let settings = map
                .get_mut(name)
                .unwrap()
                .entry(capture.node.id())
                .or_default();
            for prop in FORMAT_QUERY.property_settings(match_.pattern_index) {
                settings.insert(prop.key.deref());
            }
        }
    }

    let mut lines = vec![String::new()];

    format_iter(
        rope,
        &tree.root_node(),
        &mut lines,
        &map,
        0,
        &mut tree.walk(),
    );

    Some(lines.join("\n"))
}

fn format_iter<'a>(
    rope: &Rope,
    node: &Node<'a>,
    lines: &mut Vec<String>,
    map: &HashMap<&str, HashMap<usize, HashSet<&str>>>,
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
        if map.get("format.ignore").unwrap().contains_key(id) {
            let text = CRLF
                .replace_all(child.text(rope).as_str(), "\n")
                .trim_matches('\n')
                .split('\n')
                .map(ToOwned::to_owned)
                .collect::<Vec<String>>();
            append_lines(lines, &text);
        } else if !map.get("format.remove").unwrap().contains_key(id) {
            if !map.get("format.cancel-prepend").unwrap().contains_key(id) {
                if map.get("format.prepend-newline").unwrap().contains_key(id) {
                    lines.push(INDENT_STR.repeat(level));
                } else if let Some(md_key) = map.get("format.prepend-space").unwrap().get(id) {
                    let byte_length = child.end_byte() - child.start_byte();
                    let broader_byte_length = node.end_byte() - child.start_byte();
                    if !md_key.contains("conditional-newline") {
                        lines.last_mut().unwrap().push(' ');
                    } else if byte_length + 1 + lines.last().unwrap().len() > TEXT_WIDTH
                        || (md_key.contains("lookahead-newline")
                            && broader_byte_length + lines.last().unwrap().len() > TEXT_WIDTH)
                    {
                        lines.push(INDENT_STR.repeat(level));
                    } else {
                        lines.last_mut().unwrap().push(' ');
                    }
                }
            }
            if map.get("format.comment-fix").unwrap().contains_key(id) {
                let text = child.text(rope);
                if let Some(mat) = COMMENT_PAT.captures(text.as_str()) {
                    lines
                        .last_mut()
                        .unwrap()
                        .push_str([";", mat.get(1).unwrap().as_str()].concat().as_str());
                }
            } else if map.get("format.make-pound").unwrap().contains_key(id) {
                lines.last_mut().unwrap().push('#');
            } else if child.named_child_count() == 0 || child.kind() == "string" {
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
            if map.get("format.indent.begin").unwrap().contains_key(id) {
                level += 1;
                apply_newline = true;
            } else if map.get("format.indent.dedent").unwrap().contains_key(id) {
                lines.last_mut().unwrap().drain(0..2);
            }
        }
        if map.get("format.cancel-append").unwrap().contains_key(id) {
            apply_newline = false;
        } else if map.get("format.append-newline").unwrap().contains_key(id) {
            apply_newline = true;
        } else if map.get("format.append-space").unwrap().contains_key(id) {
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
        let mut service = initialize_server(
            &[(TEST_URI.clone(), before, Vec::new(), Vec::new(), Vec::new())],
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
        assert_eq!(doc.to_string(), String::from(after));
    }
}
