use std::{
    fs::{self},
    ops::Deref,
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
    InputEdit, Language, Node, Point, Query, QueryCursor, QueryMatch, QueryPredicateArg,
    TextProvider, WasmStore, wasmtime::Engine,
};

use crate::{Backend, ENGINE, Options, QUERY_LANGUAGE};

static LINE_START: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^([^\S\r\n]*)").unwrap());
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

pub async fn set_configuration_options(
    backend: &Backend,
    options: Value,
    workspace_uris: Vec<Url>,
) {
    let Ok(parsed_options) = serde_json::from_value::<Options>(options) else {
        warn!("Unable to parse configuration settings!",);
        return;
    };

    let mut options = backend.options.write().await;
    options.parser_install_directories = parsed_options.parser_install_directories;
    options.parser_aliases = parsed_options.parser_aliases;
    options.language_retrieval_patterns = parsed_options.language_retrieval_patterns;
    options.allowable_captures = parsed_options.allowable_captures;

    if let Some(file_options) = get_first_valid_file_config(workspace_uris) {
        // Merge parser_install_directories, since these are dependent on the local user's
        // installation paths
        if let Some(mut config_file_dirs) = file_options.parser_install_directories {
            config_file_dirs.extend(
                options
                    .parser_install_directories
                    .clone()
                    .unwrap_or_default(),
            );
            options.parser_install_directories = Some(config_file_dirs);
        }
        options.parser_aliases = file_options.parser_aliases;
        options.language_retrieval_patterns = file_options.language_retrieval_patterns;
        options.allowable_captures = file_options.allowable_captures;
    }
}

pub fn uri_to_basename(uri: &Url) -> Option<String> {
    uri.to_file_path().ok().and_then(|path| {
        path.file_stem()
            .map(|os_str| os_str.to_string_lossy().into_owned())
    })
}
