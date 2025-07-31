use std::{
    cell::RefCell,
    fs::{self},
    path::{Path, PathBuf},
    sync::LazyLock,
};

use regex::Regex;
use ropey::Rope;
use serde_json::Value;
use streaming_iterator::StreamingIterator;
use tower_lsp::{
    LanguageServer,
    lsp_types::{
        DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportKind,
        DocumentDiagnosticReportResult, NumberOrString, Position, ProgressToken, Range,
        RelatedFullDocumentDiagnosticReport, TextDocumentContentChangeEvent,
        TextDocumentIdentifier, Url, WorkDoneProgressCreateParams, request::WorkDoneProgressCreate,
    },
};
use tracing::{error, warn};
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point, Query, QueryCapture, QueryCursor, TextProvider, Tree,
    WasmStore,
};

use crate::{Backend, DocumentData, ENGINE, ImportedUri, Options, QUERY_LANGUAGE};

pub static CAPTURES_QUERY: LazyLock<Query> =
    LazyLock::new(|| Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap());
pub static INHERITS_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^;+\s*inherits: ([a-zA-Z0-9\-_,]+)").unwrap());
pub static FORMAT_IGNORE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^;+\s*(format-ignore)").unwrap());

thread_local! {
    static QUERY_PARSER: RefCell<Parser> = {
        let mut parser = Parser::new();
        parser.set_language(&QUERY_LANGUAGE).expect("Query language should load");
        parser.into()
    };
}

/// Parse the text in the rope as Tree-sitter query source code.
pub fn parse(rope: &Rope, old_tree: Option<&Tree>) -> Tree {
    QUERY_PARSER.with_borrow_mut(|parser| {
        let len_bytes = rope.len_bytes();
        parser
            .parse_with_options(
                &mut |byte, _| {
                    if byte <= len_bytes {
                        let (chunk, start_byte, _, _) = rope.chunk_at_byte(byte);
                        &chunk.as_bytes()[byte - start_byte..]
                    } else {
                        &[]
                    }
                },
                old_tree,
                None,
            )
            .expect("Parsing should have completed")
    })
}

pub trait PosUtil {
    fn to_char_idx(&self, rope: &Rope) -> usize;

    fn to_byte_offset(&self, rope: &Rope) -> usize;

    fn to_ts_point(&self, rope: &Rope) -> Point;

    fn char(&self, rope: &Rope) -> char {
        rope.char(self.to_char_idx(rope))
    }
}

impl PosUtil for Position {
    fn to_char_idx(&self, rope: &Rope) -> usize {
        let row_char_idx = rope.line_to_char(self.line as usize);
        let row_cu = rope.char_to_utf16_cu(row_char_idx);
        rope.utf16_cu_to_char(row_cu + self.character as usize)
    }

    /// Returns the starting byte of the character if the position is in the middle of a character.
    fn to_byte_offset(&self, rope: &Rope) -> usize {
        rope.char_to_byte(self.to_char_idx(rope))
    }

    fn to_ts_point(&self, rope: &Rope) -> Point {
        self.to_byte_offset(rope).to_ts_point(rope)
    }
}

pub trait RangeUtil {
    fn text(&self, rope: &Rope) -> String;
}

impl RangeUtil for Range {
    fn text(&self, rope: &Rope) -> String {
        rope.slice(self.start.to_char_idx(rope)..self.end.to_char_idx(rope))
            .to_string()
    }
}

pub trait ByteUtil {
    fn to_lsp_pos(&self, rope: &Rope) -> Position;

    fn to_ts_point(&self, rope: &Rope) -> Point;
}

impl ByteUtil for usize {
    fn to_lsp_pos(&self, rope: &Rope) -> Position {
        let line_idx = rope.byte_to_line(*self);

        let line_utf16_cu_idx = {
            let char_idx = rope.line_to_char(line_idx);
            rope.char_to_utf16_cu(char_idx)
        };

        let character_utf16_cu_idx = {
            let char_idx = rope.byte_to_char(*self);
            rope.char_to_utf16_cu(char_idx)
        };

        let line = line_idx as u32;
        let character = (character_utf16_cu_idx - line_utf16_cu_idx) as u32;

        Position { line, character }
    }

    fn to_ts_point(&self, rope: &Rope) -> Point {
        let line = rope.byte_to_line(*self);
        let char = self - rope.line_to_byte(line);
        Point {
            row: line,
            column: char,
        }
    }
}

trait PointUtil {
    fn to_lsp_pos(&self, rope: &Rope) -> Position;
}

impl PointUtil for Point {
    fn to_lsp_pos(&self, rope: &Rope) -> Position {
        let offset = rope.line_to_byte(self.row) + self.column;
        offset.to_lsp_pos(rope)
    }
}

/// Gets the `(capture)` node at the cursor, if any.
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
    let start_char = range.start.to_char_idx(rope);
    let end_char = range.end.to_char_idx(rope);
    rope.remove(start_char..end_char);

    if !new_text.is_empty() {
        rope.insert(start_char, new_text);
    }
}

pub trait TextDocChangeUtil {
    fn to_tsedit(&self, rope: &Rope) -> InputEdit;
}

impl TextDocChangeUtil for TextDocumentContentChangeEvent {
    fn to_tsedit(&self, rope: &Rope) -> InputEdit {
        let text = self.text.as_str();
        let text_end_byte_count = text.len();

        let range = self.range.unwrap_or_else(|| {
            let start = Position::new(0, 0);
            let end = (rope.len_bytes() - 1).to_lsp_pos(rope);
            Range { start, end }
        });

        let start_position = range.start.to_ts_point(rope);
        let start_byte = range.start.to_byte_offset(rope);
        let old_end_position = range.end.to_ts_point(rope);
        let old_end_byte = range.end.to_byte_offset(rope);

        let new_end_byte = start_byte + text_end_byte_count;

        let new_end_position = {
            if new_end_byte >= rope.len_bytes() {
                let line_idx = text.lines().count();
                let line_byte_idx = ropey::str_utils::line_to_byte_idx(text, line_idx);
                let row = rope.len_lines() + line_idx;
                let column = text_end_byte_count - line_byte_idx;
                Point { row, column }
            } else {
                new_end_byte.to_ts_point(rope)
            }
        };

        InputEdit {
            start_byte,
            old_end_byte,
            new_end_byte,
            start_position,
            old_end_position,
            new_end_position,
        }
    }
}

const DYLIB_EXTENSIONS: [&str; 3] = [".so", ".dll", ".dylib"];

/// Get the language name of a URI, following user-specified language aliases.
pub fn get_language_name(uri: &Url, options: &Options) -> Option<String> {
    let language_retrieval_regexes = &options.language_retrieval_patterns;
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

/// Get the language name of a file without following aliases.
pub fn get_language_name_raw(path: &Path, options: &Options) -> Option<String> {
    let language_retrieval_regexes = &options.language_retrieval_patterns;
    let path = path.canonicalize().ok()?;
    let path_str = &path.to_string_lossy();
    for re in language_retrieval_regexes {
        if let Some(caps) = re.captures(path_str).and_then(|caps| caps.get(1)) {
            return Some(caps.as_str().to_owned());
        }
    }
    None
}

/// Get the language object of the given name.
pub fn get_language(name: &str, options: &Options) -> Option<Language> {
    // Return query language object for mock tests
    if cfg!(test) && name == "query" {
        return Some(QUERY_LANGUAGE.clone());
    }

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
    let mut language_store = WasmStore::new(&ENGINE).ok()?;
    let library_path = Path::new(directory).join(&object_name);
    if let Ok(wasm) = fs::read(library_path) {
        let lang = language_store.load_language(name, &wasm);
        return match lang {
            Err(err) => {
                warn!("Error loading language {name}: {err}");
                None
            }
            Ok(lang) => Some(lang),
        };
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
            start: self.start_position().to_lsp_pos(rope),
            end: self.end_position().to_lsp_pos(rope),
        }
    }
}

fn get_first_valid_file_config(workspace_uris: Vec<PathBuf>) -> Option<Options> {
    for mut path in workspace_uris {
        let mut config_path = path.join(".tsqueryrc.json");
        loop {
            if config_path.is_file()
                && let Some(options) = fs::read_to_string(&config_path)
                    .ok()
                    .and_then(|data| serde_json::from_str(&data).ok())
            {
                return options;
            }
            // Traverse up the file tree in search of a config file
            path = match path.parent() {
                Some(parent) => parent.into(),
                None => break,
            };
            config_path = path.join(".tsqueryrc.json");
        }
    }
    None
}

pub async fn set_configuration_options(
    backend: &Backend,
    init_options: Option<Value>,
    workspace_uris: Vec<PathBuf>,
) {
    let mut options = backend.options.write().await;
    *options = Options::default();

    if let Some(init_options) = init_options {
        if let Ok(parsed_options) = serde_json::from_value::<Options>(init_options) {
            *options = parsed_options;
        } else {
            warn!("Unable to parse configuration settings!");
        };
    }

    if let Some(mut file_options) = get_first_valid_file_config(workspace_uris) {
        // Merge parser_install_directories, since these are dependent on the local user's
        // installation paths
        let mut config_file_install_dirs = options.parser_install_directories.clone();
        config_file_install_dirs.append(&mut file_options.parser_install_directories);
        file_options.parser_install_directories = config_file_install_dirs;

        *options = file_options;
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

#[cfg(test)]
pub fn get_scm_files(directories: &[PathBuf]) -> impl Iterator<Item = PathBuf> {
    let mut files: Vec<_> = directories
        .iter()
        .flat_map(|directory| {
            ignore::Walk::new(directory)
                .filter_map(|e| e.ok())
                .filter(|e| {
                    e.file_type().is_some_and(|ft| ft.is_file())
                        && e.path().extension().is_some_and(|ext| ext == "scm")
                })
                .map(|e| e.path().to_owned())
        })
        .collect();
    // When testing, sort files to prevent flakiness
    files.sort();
    files.into_iter()
}

#[cfg(not(test))]
pub fn get_scm_files(directories: &[PathBuf]) -> impl Iterator<Item = PathBuf> {
    directories.iter().flat_map(|directory| {
        ignore::Walk::new(directory)
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.file_type().is_some_and(|ft| ft.is_file())
                    && e.path().extension().is_some_and(|ext| ext == "scm")
            })
            .map(|e| e.path().to_owned())
    })
}

pub fn get_file_uris(
    dirs: &[PathBuf],
    options: &Options,
    language_name: &str,
    query_type: &str,
) -> Vec<Url> {
    let mut urls = Vec::new();

    for scm_file in get_scm_files(dirs) {
        if scm_file.file_stem().is_some_and(|stem| stem == query_type)
            && let Some(lang_name) = get_language_name_raw(&scm_file, options)
            && lang_name.as_str() == language_name
        {
            urls.push(Url::from_file_path(&scm_file).unwrap());
        }
    }

    urls
}

/// Returns a list of URIs corresponding to the modules in the `; inherits: ` chain. `None` if the
/// module could not be found.
pub fn get_imported_uris(
    workspace_dirs: &[PathBuf],
    options: &Options,
    uri: &Url,
    rope: &Rope,
    tree: &Tree,
) -> Vec<ImportedUri> {
    let mut uris = Vec::new();
    let Some(start_comment) = tree
        .root_node()
        .child(0)
        .filter(|node| node.kind() == "comment" && node.start_position().row == 0)
    else {
        return uris;
    };
    let comment_text = start_comment.text(rope);
    let Some(modules) = INHERITS_REGEX
        .captures(&comment_text)
        .and_then(|c| c.get(1))
    else {
        return uris;
    };
    let Some(query_name) = uri_to_basename(uri) else {
        return uris;
    };

    let mut byte_offset = (start_comment.start_byte() + modules.start()) as u32;
    for module in modules.as_str().split(',') {
        let (start, end) = (byte_offset, byte_offset + module.len() as u32);
        byte_offset = end + 1;
        if module.is_empty() {
            uris.push(ImportedUri::new(start, end, module.to_string(), None));
            continue;
        }
        let module_uris = get_file_uris(workspace_dirs, options, module, &query_name);
        if module_uris.len() > 1 {
            warn!(
                "Imported module {module} has more than one associated file location, analyzing the first one"
            );
        }
        uris.push(ImportedUri::new(
            start,
            end,
            module.to_string(),
            module_uris.first().cloned(),
        ));
    }

    uris
}

/// Check if a string is a subsequence of another string; in order words, it is contained in the
/// other string with possible gaps between characters.
pub fn is_subsequence(sub: &str, main: &str) -> bool {
    let mut sub_iter = sub.chars().peekable();
    let mut main_iter = main.chars();

    while let Some(&sub_char) = sub_iter.peek() {
        match main_iter.next() {
            Some(main_char) if main_char == sub_char => {
                sub_iter.next();
            }
            None => return false,
            _ => {}
        }
    }

    true
}

pub fn get_imported_module_under_cursor<'d>(
    document: &'d DocumentData,
    position: &Position,
) -> Option<&'d ImportedUri> {
    if position.line != 0 {
        return None;
    }
    let tree = &document.tree;
    let rope = &document.rope;
    let ts_point = position.to_ts_point(rope);
    let comment_node = tree
        .root_node()
        .descendant_for_point_range(ts_point, ts_point)
        .filter(|node| node.kind() == "comment")?;
    let node_text = comment_node.text(rope);
    let modules = INHERITS_REGEX.captures(&node_text).and_then(|c| c.get(1))?;
    let cursor_offset = position.to_byte_offset(rope);
    let mut comment_offset = comment_node.start_byte() + modules.start();

    let module_name = modules.as_str().split(',').find_map(|module| {
        let end = comment_offset + module.len();
        let cursor_in_module = cursor_offset >= comment_offset && cursor_offset < end;
        comment_offset = end + 1;
        cursor_in_module.then(|| module.to_string())
    })?;

    document
        .imported_uris
        .iter()
        .find(|import| import.name == module_name)
}

/// Push diagnostics to the client (only if it does not support pull diagnostics).
pub async fn push_diagnostics(backend: &Backend, uri: Url) {
    if backend
        .client_capabilities
        .read()
        .await
        .text_document
        .as_ref()
        .and_then(|td| td.diagnostic.as_ref())
        .is_none()
        // WARNING: This is *NOT* the same as `.get().as_deref().cloned()`!!!! That will still
        // allow deadlocks to occur while this code will not!!! This is because as_deref() keeps the
        // borrowed ref in scope, in the backround, until the cloned value is dropped. When using
        // map(), the ref is dropped within the map closure and thus deadlocks are prevented. Many
        // Bothans died to bring us this information.
        && let Some(document) = backend.document_map.get(&uri).map(|doc| doc.clone())
    {
        let version = document.version;

        let Ok(diagnostics) = backend
            .diagnostic(DocumentDiagnosticParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                identifier: None,
                previous_result_id: None,
                partial_result_params: Default::default(),
                work_done_progress_params: Default::default(),
            })
            .await
        else {
            warn!("Error retrieving push diagnostics for {uri}");
            return;
        };

        if let DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
            RelatedFullDocumentDiagnosticReport {
                related_documents,
                full_document_diagnostic_report,
            },
        )) = diagnostics
        {
            backend
                .client
                .publish_diagnostics(uri, full_document_diagnostic_report.items, version)
                .await;

            for (uri, report) in related_documents.unwrap_or_default() {
                if let DocumentDiagnosticReportKind::Full(report) = report
                    && let Some(version) =
                        backend.document_map.get(&uri).and_then(|doc| doc.version)
                {
                    backend
                        .client
                        .publish_diagnostics(uri, report.items, Some(version))
                        .await;
                }
            }
        }
    }
}

/// Return the given progress token, or create one and issue a new work done request (if the client
/// supports them) if it does not exist.
pub async fn get_work_done_token(
    backend: &Backend,
    work_done_token: Option<ProgressToken>,
) -> Option<ProgressToken> {
    if let Some(token) = work_done_token {
        Some(token)
    } else if backend
        .client_capabilities
        .try_read()
        .expect("Client capabilities should only be set once")
        .window
        .as_ref()
        .is_some_and(|w| w.work_done_progress == Some(true))
    {
        let token = NumberOrString::String(uuid::Uuid::new_v4().to_string());

        if let Err(error) = backend
            .client
            .send_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                token: token.clone(),
            })
            .await
        {
            error!("{error}");
            None
        } else {
            Some(token)
        }
    } else {
        None
    }
}
