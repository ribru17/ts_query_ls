use std::collections::{BTreeMap, BTreeSet, HashMap};

use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default, JsonSchema)]
pub struct Options {
    /// A list of strings representing directories to search for parsers, of the form
    /// `<lang>.(so|dll|dylib)` or `tree-sitter-<lang>.wasm`.
    pub parser_install_directories: Option<Vec<String>>,
    /// A map of parser aliases.
    pub parser_aliases: Option<BTreeMap<String, String>>,
    /// A list of patterns to aid the LSP in finding a language, given a file path.
    /// Patterns must have one capture group which represents the language name. Ordered
    /// from highest to lowest precedence.
    pub language_retrieval_patterns: Option<Vec<String>>,
    /// A map from query file names to allowable captures. Captures are represented as an object
    /// containing their name and an optional description. Note that captures prefixed with an
    /// underscore are always permissible.
    #[serde(default, deserialize_with = "vecmap_to_setmap")]
    pub allowable_captures: HashMap<String, BTreeSet<SerializableCapture>>,
}

#[derive(Serialize, Deserialize, Debug, Default, JsonSchema, Clone)]
pub struct SerializableCapture {
    pub name: String,
    pub description: Option<String>,
}

impl PartialEq for SerializableCapture {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for SerializableCapture {}

fn vecmap_to_setmap<'de, D>(
    deserializer: D,
) -> Result<HashMap<String, BTreeSet<SerializableCapture>>, D::Error>
where
    D: Deserializer<'de>,
{
    let vec_map = HashMap::<String, Vec<SerializableCapture>>::deserialize(deserializer)?;
    Ok(vec_map
        .into_iter()
        .map(|(k, v)| (k, v.into_iter().collect()))
        .collect())
}

impl Ord for SerializableCapture {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for SerializableCapture {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
