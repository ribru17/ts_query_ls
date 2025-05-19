use std::{
    fs::{self},
    path::Path,
    sync::LazyLock,
};

use regex::Regex;
use ropey::Rope;
use serde_json::Value;
use streaming_iterator::StreamingIterator;
use tower_lsp::lsp_types::{Position, Range, TextDocumentContentChangeEvent, Url};
use tracing::warn;
use tree_sitter::{
    InputEdit, Language, Node, Point, Query, QueryCapture, QueryCursor, TextProvider, Tree,
    WasmStore,
};

use crate::{Backend, ENGINE, Options, QUERY_LANGUAGE};

static LANGUAGE_REGEX_1: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"queries/([^/]+)/[^/]+\.scm$").unwrap());
static LANGUAGE_REGEX_2: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"tree-sitter-([^/]+)/queries/[^/]+\.scm$").unwrap());
pub static CAPTURES_QUERY: LazyLock<Query> =
    LazyLock::new(|| Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap());

/// Returns the starting byte of the character if the position is in the middle of a character.
pub fn lsp_position_to_byte_offset(position: Position, rope: &Rope) -> Result<usize, ropey::Error> {
    let line_char = rope.try_line_to_char(position.line as usize)?;
    let line_cu = rope.try_char_to_utf16_cu(line_char)?;
    rope.try_char_to_byte(rope.try_utf16_cu_to_char(line_cu + position.character as usize)?)
}

pub fn byte_offset_to_lsp_position(offset: usize, rope: &Rope) -> Result<Position, ropey::Error> {
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

pub fn edit_rope(rope: &mut Rope, range: Range, new_text: &str) {
    let start_row_char_idx = rope.line_to_char(range.start.line as usize);
    let start_row_cu = rope.char_to_utf16_cu(start_row_char_idx);
    let start_col_char_idx =
        rope.utf16_cu_to_char(start_row_cu + range.start.character as usize) - start_row_char_idx;
    let end_row_char_idx = rope.line_to_char(range.end.line as usize);
    let end_row_cu = rope.char_to_utf16_cu(end_row_char_idx);
    let end_col_char_idx =
        rope.utf16_cu_to_char(end_row_cu + range.end.character as usize) - end_row_char_idx;

    let start_char_idx = start_row_char_idx + start_col_char_idx;
    let end_char_idx = end_row_char_idx + end_col_char_idx;
    rope.remove(start_char_idx..end_char_idx);

    if !new_text.is_empty() {
        rope.insert(start_char_idx, new_text);
    }
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

pub fn get_language_name(uri: &Url, options: &Options) -> Option<String> {
    let mut language_retrieval_regexes: Vec<Regex> = options
        .language_retrieval_patterns
        .clone()
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
    captures
        .and_then(|captures| captures.get(1))
        .map(|capture| {
            options
                .parser_aliases
                .get(capture.as_str())
                .cloned()
                .unwrap_or(capture.as_str().to_owned())
        })
}

pub fn get_language(name: &str, options: &Options) -> Option<Language> {
    let directories = &options.parser_install_directories;
    let name = name.replace('-', "_");
    let language_fn_name = format!("tree_sitter_{name}");

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
        if let Some(lang) = get_language_object_wasm(name.as_str(), directory) {
            return Some(lang);
        }
    }
    None
}

fn get_language_object_wasm(name: &str, directory: &String) -> Option<Language> {
    let object_name = format!("tree-sitter-{name}.wasm");
    // NOTE: If WasmStore could be passed around threads safely, we could just create one global
    // store and put all of the WASM modules in there.
    let mut language_store = WasmStore::new(&ENGINE).ok()?;
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

fn get_first_valid_file_config(workspace_uris: Vec<Url>) -> Option<Options> {
    for folder_url in workspace_uris {
        if let Ok(mut path) = folder_url.to_file_path() {
            let mut config_path = path.join(".tsqueryrc.json");
            loop {
                if config_path.is_file() {
                    let data = fs::read_to_string(&config_path)
                        .ok()
                        .and_then(|data| serde_json::from_str(&data).ok());
                    if let Some(options) = data {
                        return options;
                    }
                }
                path = match path.parent() {
                    Some(parent) => parent.into(),
                    None => break,
                };
                config_path = path.join(".tsqueryrc.json");
            }
        }
    }
    None
}

pub async fn set_configuration_options(
    backend: &Backend,
    init_options: Option<Value>,
    workspace_uris: Vec<Url>,
) {
    let mut options = backend.options.write().await;
    if let Some(init_options) = init_options {
        if let Ok(parsed_options) = serde_json::from_value::<Options>(init_options) {
            options.parser_install_directories = parsed_options.parser_install_directories;
            options.parser_aliases = parsed_options.parser_aliases;
            options.language_retrieval_patterns = parsed_options.language_retrieval_patterns;
            options.valid_captures = parsed_options.valid_captures;
            options.valid_predicates = parsed_options.valid_predicates;
            options.valid_directives = parsed_options.valid_directives;
        } else {
            warn!("Unable to parse configuration settings!");
        };
    }

    if let Some(file_options) = get_first_valid_file_config(workspace_uris) {
        // Merge parser_install_directories, since these are dependent on the local user's
        // installation paths
        let mut config_file_install_dirs = file_options.parser_install_directories;
        config_file_install_dirs.extend(options.parser_install_directories.clone());
        options.parser_install_directories = config_file_install_dirs;
        options.parser_aliases = file_options.parser_aliases;
        options.language_retrieval_patterns = file_options.language_retrieval_patterns;
        options.valid_captures = file_options.valid_captures;
        options.valid_predicates = file_options.valid_predicates;
        options.valid_directives = file_options.valid_directives;
    }
}

pub fn uri_to_basename(uri: &Url) -> Option<String> {
    uri.to_file_path().ok().and_then(|path| {
        path.file_stem()
            .map(|os_str| os_str.to_string_lossy().into_owned())
    })
}

/// Return the innermost capture at the given position, if any.
pub fn capture_at_pos<'t>(
    tree: &'t Tree,
    rope: &Rope,
    query: &Query,
    point: Point,
) -> Option<QueryCapture<'t>> {
    let provider = TextProviderRope(rope);
    let mut cursor = QueryCursor::new();
    let mut p2 = point;
    p2.column += 1;

    cursor.set_point_range(point..p2);
    let mut matches = cursor.matches(query, tree.root_node(), &provider);

    let mut innermost_capture = None;
    while let Some(match_) = matches.next() {
        for capture in match_.captures {
            if capture.node.start_position() > point || capture.node.end_position() <= point {
                continue;
            }
            innermost_capture = Some(*capture)
        }
    }

    innermost_capture
}
