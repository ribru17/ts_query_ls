use std::{
    collections::{BTreeMap, HashMap},
    env,
};

use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default, JsonSchema)]
pub struct Options {
    /// A list of strings representing directories to search for parsers, of the form
    /// `<lang>.(so|dll|dylib)` or `tree-sitter-<lang>.wasm`.
    ///
    /// Supports environment variable expansion of the form `${VAR}`.
    #[serde(default, deserialize_with = "deserialize_and_expand")]
    pub parser_install_directories: Vec<String>,
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

fn expand_env_vars(input: &str) -> String {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '$' && chars.peek() == Some(&'{') {
            chars.next(); // consume '{'
            let mut var_name = String::new();

            while let Some(&ch) = chars.peek() {
                if ch == '}' {
                    chars.next(); // consume '}'
                    break;
                }
                var_name.push(ch);
                chars.next();
            }

            // Lookup the env var
            if let Ok(val) = env::var(&var_name) {
                result.push_str(&val);
            } else {
                // Leave untouched if not found
                result.push_str(&format!("${{{}}}", var_name));
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn deserialize_and_expand<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    let raw = Vec::<String>::deserialize(deserializer)?;
    Ok(raw.into_iter().map(|s| expand_env_vars(&s)).collect())
}
