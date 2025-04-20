use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::{self},
    ops::Deref,
    path::Path,
    sync::LazyLock,
};

use regex::Regex;
use ropey::Rope;
use serde_json::Value;
use streaming_iterator::StreamingIterator;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, TextDocumentContentChangeEvent, TextEdit, Url,
};
use tracing::warn;
use tree_sitter::{
    wasmtime::Engine, InputEdit, Language, Node, Point, Query, QueryCursor, QueryMatch,
    QueryPredicateArg, TextProvider, Tree, TreeCursor, WasmStore,
};

use crate::{Backend, Options, SymbolInfo, ENGINE, QUERY_LANGUAGE};

static LINE_START: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^([^\S\r\n]*)").unwrap());
static NEWLINES: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\n+").unwrap());
static COMMENT_PAT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^;+(\s*.*?)\s*$").unwrap());
static CRLF: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\r\n?").unwrap());
static LANGUAGE_REGEX_1: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"queries/([^/]+)/[^/]+\.scm$").unwrap());
static LANGUAGE_REGEX_2: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"tree-sitter-([^/]+)/queries/[^/]+\.scm$").unwrap());
pub static CAPTURES_QUERY: LazyLock<Query> =
    LazyLock::new(|| Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap());
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

/// Returns the starting byte of the character if the position is in the middle of a character.
pub fn lsp_position_to_byte_offset(position: Position, rope: &Rope) -> Result<usize, ropey::Error> {
    let line_char = rope.try_line_to_char(position.line as usize)?;
    let line_cu = rope.try_char_to_utf16_cu(line_char)?;
    rope.try_char_to_byte(rope.try_utf16_cu_to_char(line_cu + position.character as usize)?)
}

fn byte_offset_to_lsp_position(offset: usize, rope: &Rope) -> Result<Position, ropey::Error> {
    let line_idx = rope.try_byte_to_line(offset)?;

    let line_utf16_cu_idx = {
        let char_idx = rope.try_line_to_char(line_idx)?;
        rope.try_char_to_utf16_cu(char_idx)?
    };

    let character_utf16_cu_idx = {
        let char_idx = rope.try_byte_to_char(offset)?;
        rope.try_char_to_utf16_cu(char_idx)?
    };

    let line = line_idx as u32;
    let character = (character_utf16_cu_idx - line_utf16_cu_idx) as u32;

    Ok(Position { line, character })
}

fn byte_offset_to_ts_point(index: usize, rope: &Rope) -> Result<Point, ropey::Error> {
    let line = rope.try_byte_to_line(index)?;
    let char = index - rope.try_line_to_byte(line)?;
    Ok(Point {
        row: line,
        column: char,
    })
}

pub trait ToTsPoint {
    fn to_ts_point(&self, rope: &Rope) -> Point;
}

impl ToTsPoint for Position {
    fn to_ts_point(&self, rope: &Rope) -> Point {
        byte_offset_to_ts_point(lsp_position_to_byte_offset(*self, rope).unwrap(), rope).unwrap()
    }
}

fn ts_point_to_lsp_position(point: Point, rope: &Rope) -> Position {
    let offset = rope.line_to_byte(point.row) + point.column;
    byte_offset_to_lsp_position(offset, rope).unwrap()
}

pub fn get_current_capture_node(root: Node, point: Point) -> Option<Node> {
    root.named_descendant_for_point_range(point, point)
        .and_then(|node| {
            if node.kind() == "capture" {
                Some(node)
            } else {
                node.parent().filter(|parent| parent.kind() == "capture")
            }
        })
}

pub struct TextProviderRope<'a>(pub &'a Rope);

impl<'a> TextProvider<&'a [u8]> for &'a TextProviderRope<'a> {
    type I = ChunksBytes<'a>;
    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        ChunksBytes(self.0.byte_slice(node.byte_range()).chunks())
    }
}

pub struct ChunksBytes<'a>(ropey::iter::Chunks<'a>);

impl<'a> Iterator for ChunksBytes<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(str::as_bytes)
    }
}

pub fn get_references<'a>(
    root: &'a Node,
    node: &'a Node,
    query: &'a Query,
    cursor: &'a mut QueryCursor,
    provider: &'a TextProviderRope,
    rope: &'a Rope,
) -> impl Iterator<Item = Node<'a>> + 'a {
    cursor
        .matches(query, root.child_with_descendant(*node).unwrap(), provider)
        .map_deref(|match_| {
            match_.captures.iter().filter_map(|cap| {
                if cap.node.kind() == node.kind() && cap.node.text(rope) == node.text(rope) {
                    Some(cap.node)
                } else {
                    None
                }
            })
        })
        .flatten()
}

pub fn node_is_or_has_ancestor(root: Node, node: Node, kind: &str) -> bool {
    let mut optional_current_node = root.child_with_descendant(node);
    while let Some(unwrapped_current_node) = optional_current_node {
        if unwrapped_current_node.kind() == kind {
            return true;
        }
        optional_current_node = unwrapped_current_node.child_with_descendant(node);
    }
    false
}

pub fn lsp_textdocchange_to_ts_inputedit(
    rope: &Rope,
    change: &TextDocumentContentChangeEvent,
) -> Result<InputEdit, Box<dyn std::error::Error>> {
    let text = change.text.as_str();
    let text_end_byte_count = text.len();

    let range = if let Some(range) = change.range {
        range
    } else {
        let start = byte_offset_to_lsp_position(0, rope)?;
        let end = byte_offset_to_lsp_position(text_end_byte_count, rope)?;
        Range { start, end }
    };

    let start_position = range.start.to_ts_point(rope);
    let start_byte = lsp_position_to_byte_offset(range.start, rope)?;
    let old_end_position = range.end.to_ts_point(rope);
    let old_end_byte = lsp_position_to_byte_offset(range.end, rope)?;

    let new_end_byte = start_byte as usize + text_end_byte_count;

    let new_end_position = {
        if new_end_byte >= rope.len_bytes() {
            let line_idx = text.lines().count();
            let line_byte_idx = ropey::str_utils::line_to_byte_idx(text, line_idx);
            let row = rope.len_lines() + line_idx;
            let column = text_end_byte_count - line_byte_idx;
            Ok(Point { row, column })
        } else {
            byte_offset_to_ts_point(new_end_byte, rope)
        }
    }?;

    Ok(InputEdit {
        start_byte,
        old_end_byte,
        new_end_byte,
        start_position,
        old_end_position,
        new_end_position,
    })
}

const DYLIB_EXTENSIONS: [&str; 3] = [".so", ".dll", ".dylib"];

pub fn get_language(uri: &Url, options: &Options) -> Option<Language> {
    let mut language_retrieval_regexes: Vec<Regex> = options
        .language_retrieval_patterns
        .clone()
        .unwrap_or_default()
        .iter()
        .map(|r| Regex::new(r).unwrap())
        .collect();
    language_retrieval_regexes.push(LANGUAGE_REGEX_1.clone());
    language_retrieval_regexes.push(LANGUAGE_REGEX_2.clone());
    let mut captures = None;
    for re in language_retrieval_regexes {
        if let Some(caps) = re.captures(uri.as_str()) {
            captures = Some(caps);
            break;
        }
    }
    let lang = captures
        .and_then(|captures| captures.get(1))
        .and_then(|cap| {
            let cap_str = cap.as_str();
            get_language_object(
                options
                    .parser_aliases
                    .as_ref()
                    .and_then(|map| map.get(cap_str))
                    .unwrap_or(&cap_str.to_owned())
                    .as_str(),
                &options.parser_install_directories,
                &ENGINE,
            )
        });
    lang
}

pub fn get_language_object(
    name: &str,
    directories: &Option<Vec<String>>,
    engine: &Engine,
) -> Option<Language> {
    let name = name.replace('-', "_");
    let language_fn_name = format!("tree_sitter_{name}");

    if let Some(directories) = directories {
        for directory in directories {
            for dylib_extension in DYLIB_EXTENSIONS {
                let object_name = [name.as_str(), dylib_extension].concat();
                let library_path = Path::new(directory).join(&object_name);
                if let Ok(library) = unsafe { libloading::Library::new(library_path) } {
                    let language = unsafe {
                        let language_fn: libloading::Symbol<unsafe extern "C" fn() -> Language> =
                            library
                                .get(language_fn_name.as_bytes())
                                .expect("Failed to load symbol");
                        language_fn()
                    };
                    std::mem::forget(library);
                    return Some(language);
                }
            }
            if let Some(lang) = get_language_object_wasm(name.as_str(), directory, engine) {
                return Some(lang);
            }
        }
    }
    None
}

fn get_language_object_wasm(name: &str, directory: &String, engine: &Engine) -> Option<Language> {
    let object_name = format!("tree-sitter-{name}.wasm");
    // NOTE: If WasmStore could be passed around threads safely, we could just create one global
    // store and put all of the WASM modules in there.
    let mut language_store = WasmStore::new(engine).ok()?;
    let library_path = Path::new(directory).join(&object_name);
    if let Ok(wasm) = fs::read(library_path) {
        return language_store.load_language(name, &wasm).ok();
    }
    None
}

pub trait NodeUtil {
    /// Get the document text of this node.
    fn text(&self, rope: &Rope) -> String;
    /// Get the LSP range spanning the node's range.
    fn lsp_range(&self, rope: &Rope) -> Range;
}

impl NodeUtil for Node<'_> {
    fn text(&self, rope: &Rope) -> String {
        rope.byte_slice(self.byte_range()).to_string()
    }

    fn lsp_range(&self, rope: &Rope) -> Range {
        Range {
            start: ts_point_to_lsp_position(self.start_position(), rope),
            end: ts_point_to_lsp_position(self.end_position(), rope),
        }
    }
}

static DIAGNOSTICS_QUERY: LazyLock<Query> = LazyLock::new(|| {
    Query::new(
        &QUERY_LANGUAGE,
        r#"
(ERROR) @e
(MISSING) @m
(anonymous_node (string (string_content) @a))
(named_node . name: (identifier) @n)
(named_node . supertype: (identifier) @supertype)
(missing_node name: (identifier) @n)
(missing_node name: (string (string_content) @a))
(field_definition name: (identifier) @f)
(parameters (capture) @c)
(predicate
  name: (identifier) @_name
  (parameters
    .
    ; NOTE: Technically this can be a "_" but it doesn't work with anchors. Also rare?
    [(string) (identifier)] @arg)
    (#any-of? @_name "eq" "not-eq" "any-eq" "any-not-eq"
      "match" "not-match" "any-match" "any-not-match"
      "any-of" "not-any-of"))
(predicate
  name: (identifier) @_name
    (#any-of? @_name "eq" "not-eq" "any-eq" "any-not-eq")
  (parameters
    (capture)
    _
    _+ @bad_eq))
(predicate
  name: (identifier) @_name
    (#any-of? @_name "match" "not-match" "any-match" "any-not-match")
  (parameters
    (capture)
    _
    _+ @bad_match))
"#,
    )
    .unwrap()
});

pub fn get_diagnostics(
    tree: &Tree,
    rope: &Rope,
    provider: &TextProviderRope,
    symbols: &HashSet<SymbolInfo>,
    fields: &HashSet<String>,
    supertypes: &HashMap<SymbolInfo, BTreeSet<SymbolInfo>>,
) -> Vec<Diagnostic> {
    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(&DIAGNOSTICS_QUERY, tree.root_node(), provider);
    let mut diagnostics = vec![];
    let has_language_info = !symbols.is_empty();
    while let Some(match_) = matches.next() {
        for capture in match_.captures {
            let capture_name = DIAGNOSTICS_QUERY.capture_names()[capture.index as usize];
            let capture_text = capture.node.text(rope);
            let severity = Some(DiagnosticSeverity::ERROR);
            let range = capture.node.lsp_range(rope);
            match capture_name {
                "a" | "n" => {
                    if !has_language_info {
                        continue;
                    }
                    let sym = SymbolInfo {
                        label: capture_text.clone(),
                        named: capture_name == "n",
                    };
                    if !symbols.contains(&sym) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid node type: \"{capture_text}\""),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "supertype" => {
                    if !has_language_info {
                        continue;
                    }
                    let supertype_text = capture_text;
                    let sym = SymbolInfo {
                        label: supertype_text.clone(),
                        named: true,
                    };
                    if let Some(subtypes) = supertypes.get(&sym) {
                        let subtype = capture.node.next_named_sibling().unwrap();
                        let subtype_text = subtype.text(rope);
                        let subtype_sym = SymbolInfo {
                            label: subtype_text.clone(),
                            named: true,
                        };
                        let range = subtype.lsp_range(rope);
                        // Only run this check when subtypes is not empty, to account for parsers
                        // generated with ABI < 15
                        if !subtypes.is_empty() && !subtypes.contains(&subtype_sym) {
                            diagnostics.push(Diagnostic {
                                message: format!("Node \"{subtype_text}\" is not a subtype of \"{supertype_text}\""),
                                severity,
                                range,
                                ..Default::default()
                            });
                        } else if subtypes.is_empty() && !symbols.contains(&subtype_sym) {
                            diagnostics.push(Diagnostic {
                                message: format!("Invalid node type: \"{subtype_text}\""),
                                severity,
                                range,
                                ..Default::default()
                            });
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            message: format!("Node \"{supertype_text}\" is not a supertype"),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "f" => {
                    if !has_language_info {
                        continue;
                    }
                    let field = capture_text;
                    if !fields.contains(&field) {
                        diagnostics.push(Diagnostic {
                            message: format!("Invalid field name: \"{field}\""),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "e" => diagnostics.push(Diagnostic {
                    message: "Invalid syntax".to_owned(),
                    severity,
                    range,
                    ..Default::default()
                }),
                "m" => diagnostics.push(Diagnostic {
                    message: format!("Missing \"{}\"", capture.node.kind()),
                    severity,
                    range,
                    ..Default::default()
                }),
                "c" => {
                    let mut cursor = QueryCursor::new();
                    let query = &CAPTURES_QUERY;
                    let mut matches = cursor.matches(
                        query,
                        tree.root_node()
                            .child_with_descendant(capture.node)
                            .unwrap(),
                        provider,
                    );
                    let mut valid = false;
                    // NOTE: Find a simpler way to do this?
                    'outer: while let Some(m) = matches.next() {
                        for cap in m.captures {
                            if let Some(parent) = cap.node.parent() {
                                if parent.kind() != "parameters"
                                    && cap.node.text(rope) == capture_text
                                {
                                    valid = true;
                                    break 'outer;
                                }
                            }
                        }
                    }
                    if !valid {
                        diagnostics.push(Diagnostic {
                            message: format!("Undeclared capture: \"{capture_text}\""),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "arg" => {
                    diagnostics.push(Diagnostic {
                        message: "First argument must be a capture".to_owned(),
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        ..Default::default()
                    });
                }
                "bad_eq" => {
                    diagnostics.push(Diagnostic {
                        message: r##""#eq?" family predicates cannot accept multiple arguments. Consider using "#any-of?""##.to_owned(),
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        ..Default::default()
                    });
                }
                "bad_match" => {
                    diagnostics.push(Diagnostic {
                        message:
                            r##""#match?" family predicates cannot accept multiple arguments"##
                                .to_owned(),
                        range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        ..Default::default()
                    });
                }
                _ => {}
            }
        }
    }
    diagnostics
}

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

pub fn handle_predicate(
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

fn get_first_valid_file_config(workspace_uris: Vec<Url>) -> Option<Options> {
    for folder_url in workspace_uris {
        if let Ok(path) = folder_url.to_file_path() {
            let config_path = path.join("tsqueryrc.json");
            if config_path.is_file() {
                let data = fs::read_to_string(config_path).ok()?;
                if let Ok(options) = serde_json::from_str(&data) {
                    return options;
                }
            }
        }
    }
    None
}

pub fn set_configuration_options(backend: &Backend, options: Value, workspace_uris: Vec<Url>) {
    let Ok(parsed_options) = serde_json::from_value::<Options>(options) else {
        warn!("Unable to parse configuration settings!",);
        return;
    };
    if let Ok(mut options) = backend.options.write() {
        options.parser_install_directories = parsed_options.parser_install_directories;
        options.parser_aliases = parsed_options.parser_aliases;
        options.language_retrieval_patterns = parsed_options.language_retrieval_patterns;

        if let Some(file_options) = get_first_valid_file_config(workspace_uris) {
            // Don't merge parser_install_directories, since these are dependent on the local
            // user's installation paths
            options.parser_aliases = file_options.parser_aliases;
            options.language_retrieval_patterns = file_options.language_retrieval_patterns;
        }
    } else {
        warn!("Failed to update configuration options; lock could not be acquired");
    }
}
