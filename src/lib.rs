use std::{
    collections::{BTreeMap, HashMap},
    env,
    fmt::Display,
    sync::LazyLock,
};

use regex::Regex;
#[cfg(feature = "schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

static LANGUAGE_REGEX_1: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"queries/([^/]+)/[^/]+\.scm$").unwrap());
static LANGUAGE_REGEX_2: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"tree-sitter-([^/]+)/queries/[^/]+\.scm$").unwrap());

/// A type specification for a predicate.
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
struct PredicateAux {
    /// A short description of the predicate (in Markdown format).
    description: String,
    /// The list of valid parameter types.
    #[cfg_attr(feature = "schema", schemars(length(min = 1)))]
    parameters: Vec<PredicateParameter>,
    /// Whether this predicate supports a `not-` prefixed variant. Defaults to `true`.
    #[serde(default = "default_true")]
    not: bool,
    /// Whether this predicate supports a `any-` prefixed variant. Defaults to `false`.
    #[serde(default)]
    any: bool,
}

fn default_true() -> bool {
    true
}

fn add_prefixes<'de, D>(deserializer: D) -> Result<BTreeMap<String, Predicate>, D::Error>
where
    D: Deserializer<'de>,
{
    let raw = BTreeMap::<String, PredicateAux>::deserialize(deserializer)?;
    let mut valid_predicates = BTreeMap::new();
    for (name, pred) in raw {
        valid_predicates.insert(
            name.clone(),
            PredicateAux {
                description: pred.description.clone(),
                parameters: pred.parameters.clone(),
                not: pred.not,
                any: false,
            },
        );
        if pred.any {
            let description = format!(
                "Like `#{name}?`, but for quantified patterns only one captured node must match. `#{name}?` is defined as follows:\n\n{}",
                pred.description
            );
            valid_predicates.insert(
                format!("any-{name}"),
                PredicateAux {
                    description,
                    parameters: pred.parameters,
                    not: pred.not,
                    any: false,
                },
            );
        }
    }
    Ok(valid_predicates
        .into_iter()
        .flat_map(|(name, pred)| {
            let it = if !pred.not {
                vec![(
                    name,
                    Predicate {
                        description: pred.description,
                        parameters: pred.parameters,
                    },
                )]
            } else {
                let pref_name = format!("not-{name}");
                let pref_pred = Predicate {
                    parameters: pred.parameters.clone(),
                    description: format!(
                        "The inverse of `#{name}?`, which is defined as follows:\n\n{}",
                        pred.description
                    ),
                };
                vec![
                    (
                        name,
                        Predicate {
                            description: pred.description,
                            parameters: pred.parameters,
                        },
                    ),
                    (pref_name, pref_pred),
                ]
            };
            it.into_iter()
        })
        .collect())
}

/// Configuration options for the language server.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
pub struct Options {
    /// A list of strings representing directories to search for parsers, of the form
    /// `<lang>.(so|dll|dylib)` or `tree-sitter-<lang>.wasm`.
    ///
    /// Supports environment variable expansion of the form `${VAR}`.
    #[serde(default, deserialize_with = "deserialize_and_expand")]
    pub parser_install_directories: Vec<String>,

    /// A map of parser aliases.
    #[serde(default)]
    pub parser_aliases: BTreeMap<String, String>,

    /// A list of patterns to aid the LSP in finding a language, given a file path.
    /// Patterns must have one capture group which represents the language name. Ordered
    /// from highest to lowest precedence.
    #[serde(default = "default_regexes", deserialize_with = "add_default_regexes")]
    pub language_retrieval_patterns: Vec<SerializableRegex>,

    /// A map from query file name to valid captures. Valid captures are represented as a map from
    /// capture name (sans `@`) to a short (markdown format) description. Note that captures
    /// prefixed with an underscore are always permissible.
    #[serde(default)]
    pub valid_captures: HashMap<String, BTreeMap<String, String>>,

    /// A map of predicate names (sans `#` and `?`) to parameter specifications.
    #[serde(default, deserialize_with = "add_prefixes")]
    #[cfg_attr(feature = "schema", schemars(schema_with = "prefixes_schema"))]
    pub valid_predicates: BTreeMap<String, Predicate>,

    /// A map of directive names (sans `#` and `!`) to parameter specifications.
    #[serde(default)]
    pub valid_directives: BTreeMap<String, Predicate>,

    /// Options related to diagnostics
    #[serde(default)]
    pub diagnostic_options: DiagnosticOptions,

    /// An inclusive range of ABI versions supported by your tool. The end of the range must be
    /// greater than or equal to the start.
    pub supported_abi_versions: Option<std::ops::RangeInclusive<u32>>,
}

#[cfg(feature = "schema")]
fn prefixes_schema(gen_: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
    let raw = <BTreeMap<String, PredicateAux>>::json_schema(gen_).into_object();
    raw.into()
}

/// Options related to diagnostics
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
pub struct DiagnosticOptions {
    /// The style for predicate string arguments
    #[serde(default)]
    pub string_argument_style: StringArgumentStyle,
    /// Whether to warn on `_`-prefixed captures which are not referenced by a predicate or directive
    /// (default `true`)
    #[serde(default = "default_true")]
    pub warn_unused_underscore_captures: bool,
}

impl Default for DiagnosticOptions {
    fn default() -> Self {
        Self {
            string_argument_style: Default::default(),
            warn_unused_underscore_captures: true,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum StringArgumentStyle {
    /// String arguments can be quoted or unquoted (default)
    #[default]
    None,
    /// String arguments must be quoted
    PreferQuoted,
    /// String arguments should be unquoted, when possible
    PreferUnquoted,
}

/// A type specification for a directive.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
pub struct Predicate {
    /// A short description of the predicate (in Markdown format).
    pub description: String,
    /// The list of valid parameter types.
    #[cfg_attr(feature = "schema", schemars(length(min = 1)))]
    pub parameters: Vec<PredicateParameter>,
}

/// A parameter type reference.
///
/// Parameters can be one or both of two types (a capture or a string), and can be required,
/// optional, or "variadic" (there can be zero-to-many of them).
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
pub struct PredicateParameter {
    /// An optional description of this parameter.
    pub description: Option<String>,
    /// The type of this parameter. Can be `capture`, `string`, or `any` (either a capture or a
    /// string).
    #[serde(rename = "type")]
    pub type_: PredicateParameterType,
    /// The arity of the predicate parameter. Must be `"required"`, `"optional"`, or `"variadic"`.
    #[serde(default)]
    pub arity: PredicateParameterArity,
}

/// The type of the predicate parameter.
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
#[serde(rename_all = "lowercase")]
pub enum PredicateParameterType {
    /// Must be a capture (e.g. `@variable`).
    Capture,
    /// Must be a string (e.g. `foo`).
    String,
    /// Can be either a capture or a string.
    Any,
}

impl Display for PredicateParameterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Capture => write!(f, "capture"),
            Self::String => write!(f, "string"),
            Self::Any => write!(f, "any"),
        }
    }
}

/// The arity of the predicate parameter.
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "schema", derive(JsonSchema))]
#[serde(rename_all = "lowercase")]
pub enum PredicateParameterArity {
    /// A regular, required parameter.
    Required,
    /// A parameter which can be omitted. Must only be followed by other optional parameters.
    Optional,
    /// A parameter which can appear zero-to-many times. Must be the last parameter if present.
    Variadic,
}

impl Display for PredicateParameterArity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Required => write!(f, "required"),
            Self::Optional => write!(f, "optional"),
            Self::Variadic => write!(f, "variadic"),
        }
    }
}

impl Default for PredicateParameterArity {
    fn default() -> Self {
        Self::Required
    }
}

/// Expand environment variables written in `${VAR}` syntax
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
                result.push_str(&format!("${{{var_name}}}"));
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

fn default_regexes() -> Vec<SerializableRegex> {
    vec![
        LANGUAGE_REGEX_1.clone().into(),
        LANGUAGE_REGEX_2.clone().into(),
    ]
}

fn add_default_regexes<'de, D>(deserializer: D) -> Result<Vec<SerializableRegex>, D::Error>
where
    D: Deserializer<'de>,
{
    let mut raw = Vec::<SerializableRegex>::deserialize(deserializer)?;

    // Always provide these defaults
    raw.append(&mut default_regexes());
    Ok(raw)
}

#[derive(Debug, Clone)]
pub struct SerializableRegex(Regex);

impl SerializableRegex {
    pub fn captures<'h>(&self, haystack: &'h str) -> Option<regex::Captures<'h>> {
        self.0.captures_at(haystack, 0)
    }
}

impl From<Regex> for SerializableRegex {
    fn from(value: Regex) -> Self {
        Self(value)
    }
}

impl Serialize for SerializableRegex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.0.as_str())
    }
}

impl<'de> Deserialize<'de> for SerializableRegex {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Regex::new(&s)
            .map(SerializableRegex)
            .map_err(serde::de::Error::custom)
    }
}

impl PartialEq for SerializableRegex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl Eq for SerializableRegex {}

#[cfg(feature = "schema")]
impl JsonSchema for SerializableRegex {
    fn json_schema(_: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        use schemars::schema::{InstanceType, Schema, SchemaObject, StringValidation};

        Schema::Object(SchemaObject {
            instance_type: Some(InstanceType::String.into()),
            format: Some(String::from("regex")),
            string: Some(Box::new(StringValidation::default())),
            metadata: Some(Box::new(schemars::schema::Metadata {
                description: Some(String::from(
                    "A regular expression string (compiled at deserialization time)",
                )),
                ..Default::default()
            })),
            ..Default::default()
        })
    }

    fn schema_name() -> String {
        String::from("Regex")
    }
}
