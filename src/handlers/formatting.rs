use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use lazy_static::lazy_static;
use streaming_iterator::StreamingIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{DocumentFormattingParams, TextEdit};
use tree_sitter::{Query, QueryCursor};

use crate::{
    util::{self, format_iter, handle_predicate, TextProviderRope},
    Backend, QUERY_LANGUAGE,
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
    let root = tree.root_node();
    if root.has_error() {
        return Ok(None);
    }
    let mut cursor = QueryCursor::new();
    let provider = TextProviderRope(&rope);
    let mut matches = cursor.matches(&FORMAT_QUERY, root, &provider);

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

    'matches: while let Some(match_) = matches.next() {
        for predicate in FORMAT_QUERY.general_predicates(match_.pattern_index) {
            let keep = handle_predicate(match_, &predicate.operator, &predicate.args, &rope);
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

    let mut edits = vec![String::new()];

    format_iter(&rope, &tree.root_node(), &mut edits, &map, 0);

    Ok(Some(util::diff(
        rope.to_string().as_str(),
        edits.join("\n").as_str(),
        &rope,
    )))
}

lazy_static! {
    static ref FORMAT_QUERY: Query = Query::new(
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
    .unwrap();
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use tower::{Service, ServiceExt};
    use tower_lsp::lsp_types::{
        notification::DidChangeTextDocument, request::Formatting, DidChangeTextDocumentParams,
        DocumentFormattingParams, FormattingOptions, TextDocumentContentChangeEvent,
        TextDocumentIdentifier, VersionedTextDocumentIdentifier, WorkDoneProgressParams,
    };

    use crate::test_helpers::helpers::{
        initialize_server, jsonrpc_response_to_lsp_value, lsp_notification_to_jsonrpc_request,
        lsp_request_to_jsonrpc_request, TEST_URI,
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
            initialize_server(&[(TEST_URI.clone(), before, Vec::new(), Vec::new())]).await;

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
