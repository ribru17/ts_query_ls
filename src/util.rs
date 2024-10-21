use std::{collections::HashSet, fs, path::Path};

use ropey::Rope;
use streaming_iterator::StreamingIterator;
use tower_lsp::lsp_types::*;
use tree_sitter::{
    wasmtime::Engine, InputEdit, Language, Node, Point, Query, QueryCursor, Tree, WasmStore,
};

use crate::SymbolInfo;

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

pub fn byte_offset_to_ts_point(index: usize, rope: &Rope) -> Result<Point, ropey::Error> {
    let line = rope.try_byte_to_line(index)?;
    let char = index - rope.try_line_to_byte(line)?;
    Ok(Point {
        row: line,
        column: char,
    })
}

pub fn lsp_position_to_ts_point(position: Position, rope: &Rope) -> Point {
    byte_offset_to_ts_point(lsp_position_to_byte_offset(position, rope).unwrap(), rope).unwrap()
}

pub fn ts_point_to_lsp_position(point: Point, rope: &Rope) -> Position {
    let offset = rope.line_to_byte(point.row) + point.column;
    byte_offset_to_lsp_position(offset, rope).unwrap()
}

pub fn ts_node_to_lsp_range(node: Node, rope: &Rope) -> Range {
    Range {
        start: ts_point_to_lsp_position(node.start_position(), rope),
        end: ts_point_to_lsp_position(node.end_position(), rope),
    }
}

pub fn get_current_capture_node(root: Node, point: Point) -> Option<Node> {
    root.named_descendant_for_point_range(point, point)
        .and_then(|node| {
            if node.grammar_name() == "capture" {
                Some(node)
            } else {
                node.parent()
                    .filter(|parent| parent.grammar_name() == "capture")
            }
        })
}

pub fn get_references<'a>(
    uri: &'a Url,
    root: &'a Node,
    node: &'a Node,
    query: &'a Query,
    cursor: &'a mut QueryCursor,
    contents: &'a [u8],
    rope: &'a Rope,
) -> impl Iterator<Item = Location> + 'a {
    return cursor
        .matches(query, root.child_with_descendant(*node).unwrap(), contents)
        .map_deref(|match_| {
            match_.captures.iter().filter_map(|cap| {
                if cap.node.grammar_name() == node.grammar_name()
                    && get_node_text(cap.node, rope) == get_node_text(*node, rope)
                {
                    Some(Location {
                        uri: uri.clone(),
                        range: ts_node_to_lsp_range(cap.node, rope),
                    })
                } else {
                    None
                }
            })
        })
        .flatten();
}

pub fn node_is_or_has_ancestor(root: Node, node: Node, kind: &str) -> bool {
    let mut optional_current_node = root.child_with_descendant(node);
    while let Some(unwrapped_current_node) = optional_current_node {
        if unwrapped_current_node.grammar_name() == kind {
            return true;
        }
        optional_current_node = unwrapped_current_node.child_with_descendant(node);
    }
    false
}

pub fn lsp_textdocchange_to_ts_inputedit(
    source: &Rope,
    change: &TextDocumentContentChangeEvent,
) -> Result<InputEdit, Box<dyn std::error::Error>> {
    let text = change.text.as_str();
    let text_end_byte_count = text.as_bytes().len();

    let range = if let Some(range) = change.range {
        range
    } else {
        let start = byte_offset_to_lsp_position(0, source)?;
        let end = byte_offset_to_lsp_position(text_end_byte_count, source)?;
        Range { start, end }
    };

    let start_position = lsp_position_to_ts_point(range.start, source);
    let start_byte = lsp_position_to_byte_offset(range.start, source)?;
    let old_end_position = lsp_position_to_ts_point(range.end, source);
    let old_end_byte = lsp_position_to_byte_offset(range.end, source)?;

    let new_end_byte = start_byte as usize + text_end_byte_count;

    let new_end_position = {
        if new_end_byte >= source.len_bytes() {
            let line_idx = text.lines().count();
            let line_byte_idx = ropey::str_utils::line_to_byte_idx(text, line_idx);
            let row = source.len_lines() + line_idx;
            let column = text_end_byte_count - line_byte_idx;
            Ok(tree_sitter::Point { row, column })
        } else {
            byte_offset_to_ts_point(new_end_byte, source)
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

pub fn get_language(
    name: &str,
    directories: &Option<Vec<String>>,
    engine: &Engine,
) -> Option<Language> {
    let language_fn_name = format!("tree_sitter_{}", name.replace('-', "_"));

    if let Some(directories) = directories {
        for directory in directories {
            for dylib_extension in DYLIB_EXTENSIONS {
                let object_name = [name, dylib_extension].concat();
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
            if let Some(lang) = get_language_wasm(name, directory, engine) {
                return Some(lang);
            }
        }
    }
    None
}

fn get_language_wasm(name: &str, directory: &String, engine: &Engine) -> Option<Language> {
    let object_name = ["tree-sitter-", name, ".wasm"].concat();
    // NOTE: If WasmStore could be passed around threads safely, we could just create one global
    // store and put all of the WASM modules in there.
    let mut language_store = WasmStore::new(engine).unwrap();
    let library_path = Path::new(directory).join(&object_name);
    if let Ok(wasm) = fs::read(library_path) {
        let language = language_store.load_language(name, &wasm).unwrap();
        return Some(language);
    }
    None
}

pub fn get_node_text(node: Node, rope: &Rope) -> String {
    rope.byte_slice(node.start_byte()..node.end_byte())
        .to_string()
}

const DIAGNOSTICS_QUERY: &str = r#"
(ERROR) @e
(anonymous_node (string (string_content) @a))
(named_node name: (identifier) @n)
(field_definition name: (identifier) @f)
"#;

pub fn get_diagnostics(
    tree: &Tree,
    rope: &Rope,
    contents: &String,
    symbols: &HashSet<SymbolInfo>,
    fields: &HashSet<String>,
) -> Vec<Diagnostic> {
    let mut cursor = QueryCursor::new();
    let query = Query::new(&tree_sitter_query::language(), DIAGNOSTICS_QUERY).unwrap();
    let mut matches = cursor.matches(&query, tree.root_node(), contents.as_bytes());
    let mut diagnostics = vec![];
    while let Some(match_) = matches.next() {
        for capture in match_.captures {
            let capture_name = query.capture_names()[capture.index as usize];
            let severity = Some(DiagnosticSeverity::ERROR);
            let range = ts_node_to_lsp_range(capture.node, rope);
            match capture_name {
                "a" | "n" => {
                    let sym = SymbolInfo {
                        label: get_node_text(capture.node, rope),
                        named: capture_name == "n",
                    };
                    if !symbols.contains(&sym) {
                        diagnostics.push(Diagnostic {
                            message: "Invalid node type!".to_owned(),
                            severity,
                            range,
                            ..Default::default()
                        });
                    }
                }
                "f" => {
                    let field = get_node_text(capture.node, rope);
                    if !fields.contains(&field) {
                        diagnostics.push(Diagnostic {
                            message: "Invalid field type!".to_owned(),
                            severity,
                            range,
                            ..Default::default()
                        })
                    }
                }
                "e" => diagnostics.push(Diagnostic {
                    message: "Invalid syntax!".to_owned(),
                    severity,
                    range,
                    ..Default::default()
                }),
                _ => {}
            }
        }
    }
    diagnostics
}
