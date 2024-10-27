use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
    env::set_current_dir,
    ops::Deref,
    path::PathBuf,
    sync::{Arc, RwLock},
};
use streaming_iterator::StreamingIterator;

use dashmap::DashMap;
use regex::Regex;
use ropey::Rope;
use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::*,
    Client, LanguageServer, LspService, Server,
};
use tree_sitter::{wasmtime::Engine, Language, Parser, Query, QueryCursor, Tree};
use util::{
    format_iter, get_current_capture_node, get_diagnostics, get_language, get_node_text,
    get_references, handle_predicate, lsp_position_to_byte_offset, lsp_position_to_ts_point,
    lsp_textdocchange_to_ts_inputedit, node_is_or_has_ancestor,
};

lazy_static! {
    static ref ENGINE: Engine = Engine::default();
    static ref QUERY_LANGUAGE: Language = tree_sitter_query::language();
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
    (anonymous_node
      name: (string) .)
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
    (negated_field)
  ] @format.cancel-append
  .
  (quantifier)?
  .
  "."? @format.prepend-newline ; Make sure anchor are not eol but start of newline
  .
  (comment) @format.prepend-space
  (#not-is-start-of-line? @format.prepend-space))
"#
    )
    .unwrap();
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct SymbolInfo {
    label: String,
    named: bool,
}

struct Backend {
    client: Client,
    document_map: DashMap<Url, Rope>,
    ast_map: DashMap<Url, Tree>,
    symbols_set_map: DashMap<Url, HashSet<SymbolInfo>>,
    symbols_vec_map: DashMap<Url, Vec<SymbolInfo>>,
    fields_set_map: DashMap<Url, HashSet<String>>,
    fields_vec_map: DashMap<Url, Vec<String>>,
    options: Arc<RwLock<Options>>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Options {
    parser_install_directories: Option<Vec<String>>,
    parser_aliases: Option<BTreeMap<String, String>>,
    language_retrieval_patterns: Option<Vec<String>>,
}

mod util;

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls initialize: {:?}", params),
            )
            .await;
        if let Some(root_uri) = params.root_uri {
            let root = PathBuf::from(root_uri.path());
            if set_current_dir(&root).is_err() {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to set root directory to {:?}", root),
                    )
                    .await;
            };
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(["@", "\"", "\\", "("].map(|c| c.to_owned()).into()),
                    ..CompletionOptions::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let changed_options =
            if let Ok(options) = serde_json::from_value::<Options>(params.settings) {
                options
            } else {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        "Unable to parse configuration settings!",
                    )
                    .await;
                return;
            };
        let mut options = self.options.write().unwrap();
        options.parser_install_directories = changed_options.parser_install_directories;
        options.parser_aliases = changed_options.parser_aliases;
        options.language_retrieval_patterns = changed_options.language_retrieval_patterns;
    }

    async fn shutdown(&self) -> Result<()> {
        self.client
            .log_message(MessageType::LOG, "ts_query_ls shutdown".to_owned())
            .await;
        Ok(())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls goto_definition: {:?}", params),
            )
            .await;
        let uri = params.text_document_position_params.text_document.uri;
        let pos = Position {
            line: params.text_document_position_params.position.line,
            character: 0,
        };
        let range = Range::new(pos, pos);
        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri,
            range,
        })))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = &params.text_document.uri;
        self.client
            .log_message(
                MessageType::LOG,
                format!("ts_query_ls did_open: {:?}", params),
            )
            .await;
        let contents = params.text_document.text;
        let rope = Rope::from_str(&contents);
        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error loading Query grammar");
        self.document_map.insert(uri.clone(), rope.clone());
        self.ast_map
            .insert(uri.clone(), parser.parse(&contents, None).unwrap());

        // Get language, if it exists
        let mut lang = None;
        if let Ok(options) = self.options.read() {
            let mut language_retrieval_regexes: Vec<Regex> = options
                .language_retrieval_patterns
                .clone()
                .unwrap_or(vec![])
                .iter()
                .map(|r| Regex::new(r).unwrap())
                .collect();
            language_retrieval_regexes.push(Regex::new(r#"queries/([^/]+)/[^/]+\.scm$"#).unwrap());
            language_retrieval_regexes
                .push(Regex::new(r#"tree-sitter-([^/]+)/queries/[^/]+\.scm$"#).unwrap());
            let mut captures = None;
            for re in language_retrieval_regexes {
                if let Some(caps) = re.captures(uri.as_str()) {
                    captures = Some(caps);
                    break;
                }
            }
            lang = captures
                .and_then(|captures| captures.get(1))
                .and_then(|cap| {
                    let cap_str = cap.as_str();
                    get_language(
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
        }

        // Initialize language info
        let mut symbols_vec: Vec<SymbolInfo> = vec![];
        let mut symbols_set: HashSet<SymbolInfo> = HashSet::new();
        let mut fields_vec: Vec<String> = vec![];
        let mut fields_set: HashSet<String> = HashSet::new();
        if let Some(lang) = lang {
            let error_symbol = SymbolInfo {
                label: "ERROR".to_owned(),
                named: true,
            };
            symbols_set.insert(error_symbol.clone());
            symbols_vec.push(error_symbol);
            for i in 0..lang.node_kind_count() as u16 {
                let named = lang.node_kind_is_named(i);
                let label = if named {
                    lang.node_kind_for_id(i).unwrap().to_owned()
                } else {
                    lang.node_kind_for_id(i)
                        .unwrap()
                        .replace('\\', r#"\\"#)
                        .replace('"', r#"\""#)
                        .replace("\n", r#"\n"#)
                };
                let symbol_info = SymbolInfo { label, named };
                if symbols_set.contains(&symbol_info) || !lang.node_kind_is_visible(i) {
                    continue;
                }
                symbols_set.insert(symbol_info.clone());
                symbols_vec.push(symbol_info);
            }
            // Field IDs go from 1 to nfields inclusive (extra index 0 maps to NULL)
            for i in 1..=lang.field_count() as u16 {
                let field_name = lang.field_name_for_id(i).unwrap().to_owned();
                if !fields_set.contains(&field_name) {
                    fields_set.insert(field_name.clone());
                    fields_vec.push(field_name);
                }
            }
        }
        self.symbols_vec_map.insert(uri.to_owned(), symbols_vec);
        self.symbols_set_map.insert(uri.to_owned(), symbols_set);
        self.fields_vec_map.insert(uri.to_owned(), fields_vec);
        self.fields_set_map.insert(uri.to_owned(), fields_set);

        // Publish diagnostics
        if let (Some(tree), Some(symbols), Some(fields)) = (
            self.ast_map.get(uri),
            self.symbols_set_map.get(uri),
            self.fields_set_map.get(uri),
        ) {
            self.client
                .publish_diagnostics(
                    uri.clone(),
                    get_diagnostics(&tree, &rope, &contents, &symbols, &fields),
                    None,
                )
                .await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = &params.text_document.uri;
        let mut rope = self.document_map.get_mut(uri).unwrap();
        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error loading Query grammar");

        let mut edits = vec![];
        for change in &params.content_changes {
            let text = change.text.as_str();
            let text_bytes = text.as_bytes();
            let text_end_byte_idx = text_bytes.len();

            let range = if let Some(range) = change.range {
                range
            } else {
                let start_line_idx = rope.byte_to_line(0);
                let end_line_idx = rope.byte_to_line(text_end_byte_idx);

                let start = Position::new(start_line_idx as u32, 0);
                let end = Position::new(end_line_idx as u32, 0);
                Range { start, end }
            };

            edits.push(lsp_textdocchange_to_ts_inputedit(&rope, change).unwrap());

            let start_row_char_idx = rope.line_to_char(range.start.line as usize);
            let start_row_cu = rope.char_to_utf16_cu(start_row_char_idx);
            let start_col_char_idx = rope
                .utf16_cu_to_char(start_row_cu + range.start.character as usize)
                - start_row_char_idx;
            let end_row_char_idx = rope.line_to_char(range.end.line as usize);
            let end_row_cu = rope.char_to_utf16_cu(end_row_char_idx);
            let end_col_char_idx =
                rope.utf16_cu_to_char(end_row_cu + range.end.character as usize) - end_row_char_idx;

            let start_char_idx = start_row_char_idx + start_col_char_idx;
            let end_char_idx = end_row_char_idx + end_col_char_idx;
            rope.remove(start_char_idx..end_char_idx);

            if !change.text.is_empty() {
                rope.insert(start_char_idx, text);
            }
        }
        let contents = rope.to_string();
        let result = {
            let mut old_tree = self.ast_map.get_mut(uri).unwrap();

            for edit in edits {
                old_tree.edit(&edit);
            }

            parser.parse(&contents, Some(&old_tree))
        };

        if let Some(tree) = result {
            *self.ast_map.get_mut(uri).unwrap() = tree.clone();
            // Update diagnostics
            if let (Some(symbols), Some(fields)) =
                (self.symbols_set_map.get(uri), self.fields_set_map.get(uri))
            {
                self.client
                    .publish_diagnostics(
                        uri.clone(),
                        get_diagnostics(&tree, &rope, &contents, &symbols, &fields),
                        None,
                    )
                    .await;
            }
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;

        let Some(tree) = self.ast_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No AST built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let Some(rope) = self.document_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No document built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let cur_pos = lsp_position_to_ts_point(params.text_document_position.position, &rope);
        let current_node = match get_current_capture_node(tree.root_node(), cur_pos) {
            None => return Ok(None),
            Some(value) => value,
        };

        let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let contents = Cow::from(rope.clone());
        let contents = contents.as_bytes();

        let mut parser = Parser::new();
        parser
            .set_language(&QUERY_LANGUAGE)
            .expect("Error setting language for Query parser");

        Ok(Some(
            get_references(
                uri,
                &tree.root_node(),
                &current_node,
                &query,
                &mut cursor,
                contents,
                &rope,
            )
            .collect(),
        ))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let Some(tree) = self.ast_map.get(&uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No AST built for URI: {:?}", uri),
                )
                .await;
            return Ok(None);
        };
        let Some(rope) = self.document_map.get(&uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No document built for URI: {:?}", uri),
                )
                .await;
            return Ok(None);
        };
        // NOTE: Can we use Cow here?
        let contents = Cow::from(rope.clone());
        let contents = contents.as_bytes();
        let current_node = match get_current_capture_node(
            tree.root_node(),
            lsp_position_to_ts_point(params.text_document_position.position, &rope),
        ) {
            None => return Ok(None),
            Some(value) => value,
        };
        let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let new_name = params.new_name;
        let identifier_pattern = Regex::new(r#"^[a-zA-Z0-9.\-_\$]+$"#).unwrap();
        if !identifier_pattern.is_match(new_name.as_str()) {
            return Err(jsonrpc::Error::invalid_params(
                "New name is not a valid identifier",
            ));
        }
        let mut text_document_edits: Vec<TextDocumentEdit> = vec![];
        get_references(
            &uri,
            &tree.root_node(),
            &current_node,
            &query,
            &mut cursor,
            contents,
            &rope,
        )
        .for_each(|mut elem| {
            // Don't include the preceding `@`
            elem.range.start.character += 1;
            text_document_edits.push(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier {
                    uri: elem.uri,
                    // TODO: Support versioned edits
                    version: None,
                },
                edits: vec![OneOf::Left(TextEdit {
                    range: elem.range,
                    new_text: new_name.clone(),
                })],
            });
        });
        // Apply edits from end to start, to prevent offset inaccuracies
        text_document_edits.sort_by(|a, b| {
            if let OneOf::Left(a) = &a.edits[0] {
                if let OneOf::Left(b) = &b.edits[0] {
                    let range_a = a.range;
                    let range_b = b.range;
                    range_b.start.cmp(&range_a.start)
                } else {
                    Ordering::Equal
                }
            } else {
                Ordering::Equal
            }
        });

        Ok(Some(WorkspaceEdit {
            document_changes: Some(DocumentChanges::Edits(text_document_edits)),
            changes: None,
            change_annotations: None,
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;

        let Some(tree) = self.ast_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No AST built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };
        let Some(rope) = self.document_map.get(uri) else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("No document built for URI: {:?}", *uri),
                )
                .await;
            return Ok(None);
        };

        let mut position = params.text_document_position.position;
        if position.character > 0 {
            position.character -= 1;
        }
        let point = lsp_position_to_ts_point(position, &rope);
        let query = Query::new(&QUERY_LANGUAGE, "(capture) @cap").unwrap();
        let mut cursor = QueryCursor::new();
        let contents = Cow::from(rope.slice(..));
        let contents = contents.as_bytes();
        let current_node = tree
            .root_node()
            .named_descendant_for_point_range(point, point)
            .unwrap();

        let mut completion_items = vec![];

        // Node and field name completions
        let cursor_after_at_sign = lsp_position_to_byte_offset(position, &rope)
            .and_then(|b| rope.try_byte_to_char(b))
            .map_or(false, |c| rope.char(c) == '@');
        let in_capture = cursor_after_at_sign
            || node_is_or_has_ancestor(tree.root_node(), current_node, "capture");
        if !in_capture && !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate") {
            let in_anon = node_is_or_has_ancestor(tree.root_node(), current_node, "anonymous_node");
            if let Some(symbols) = self.symbols_vec_map.get(uri) {
                for symbol in symbols.iter() {
                    if (in_anon && !symbol.named) || (!in_anon && symbol.named) {
                        completion_items.push(CompletionItem {
                            label: symbol.label.clone(),
                            kind: if symbol.named {
                                Some(CompletionItemKind::CLASS)
                            } else {
                                Some(CompletionItemKind::CONSTANT)
                            },
                            ..Default::default()
                        });
                    }
                }
            }
            if !in_anon {
                if let Some(fields) = self.fields_vec_map.get(uri) {
                    for field in fields.iter() {
                        completion_items.push(CompletionItem {
                            label: [field, ": "].concat(),
                            kind: Some(CompletionItemKind::FIELD),
                            ..Default::default()
                        });
                    }
                }
            }
        }

        // Capture completions
        if !node_is_or_has_ancestor(tree.root_node(), current_node, "predicate")
            || node_is_or_has_ancestor(tree.root_node(), current_node, "string")
        {
            return Ok(Some(CompletionResponse::Array(completion_items)));
        }
        let node = match tree.root_node().child_with_descendant(current_node) {
            None => return Ok(Some(CompletionResponse::Array(completion_items))),
            Some(value) => value,
        };
        let mut iter = cursor.matches(&query, node, contents);
        let mut seen = HashSet::new();
        while let Some(match_) = iter.next() {
            for capture in match_.captures {
                let node_text = get_node_text(capture.node, &rope);
                let parent_params = match capture.node.parent() {
                    None => true,
                    Some(value) => value.grammar_name() != "parameters",
                };
                if parent_params && !seen.contains(&node_text) {
                    seen.insert(node_text.clone());
                    completion_items.push(CompletionItem {
                        label: node_text.to_owned(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        ..Default::default()
                    });
                }
            }
        }

        Ok(Some(CompletionResponse::Array(completion_items)))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let tree = match self.ast_map.get(&uri) {
            None => return Ok(None),
            Some(val) => val,
        };
        let rope = match self.document_map.get(&uri) {
            None => return Ok(None),
            Some(val) => val,
        };
        let root = tree.root_node();
        let mut cursor = QueryCursor::new();
        let contents = Cow::from(rope.clone());
        let contents = contents.as_bytes();
        let mut matches = cursor.matches(&FORMAT_QUERY, root, contents);

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

        let mut edits = vec!["".to_owned()];

        format_iter(&rope, &tree.root_node(), &mut edits, &map, 0);

        Ok(Some(util::diff(
            rope.to_string().as_str(),
            edits.join("\n").as_str(),
            &rope,
        )))
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let options = Arc::new(RwLock::new(Options {
        parser_install_directories: None,
        parser_aliases: None,
        language_retrieval_patterns: None,
    }));
    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
        ast_map: DashMap::new(),
        symbols_set_map: DashMap::new(),
        symbols_vec_map: DashMap::new(),
        fields_set_map: DashMap::new(),
        fields_vec_map: DashMap::new(),
        options,
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
