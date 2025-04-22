use std::collections::{BTreeMap, HashMap};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default, JsonSchema)]
pub struct Options {
    /// A list of strings representing directories to search for parsers, of the form
    /// `<lang>.(so|dll|dylib)` or `tree-sitter-<lang>.wasm`.
    ///
    /// Supports environment variable expansion of the form `${VAR}`.
    pub parser_install_directories: Option<Vec<String>>,
    /// A map of parser aliases.
    pub parser_aliases: Option<BTreeMap<String, String>>,
    /// A list of patterns to aid the LSP in finding a language, given a file path.
    /// Patterns must have one capture group which represents the language name. Ordered
    /// from highest to lowest precedence.
    pub language_retrieval_patterns: Option<Vec<String>>,
    /// A map from query file name to valid captures. Valid captures are represented as a map from
    /// capture name (sans `@`) to a short (markdown format) description. Note that captures
    /// prefixed with an underscore are always permissible.
    #[serde(default)]
    pub valid_captures: HashMap<String, BTreeMap<String, String>>,
}
