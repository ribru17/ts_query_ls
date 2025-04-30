use clap::{Parser, Subcommand};
use core::fmt;
use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator as _};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    env,
    fs::{self},
    path::{Path, PathBuf},
    str,
    sync::{Arc, LazyLock, RwLock, atomic::AtomicI32},
};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use ts_query_ls::Options;
use walkdir::WalkDir;

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::Result,
    lsp_types::{
        CodeActionParams, CodeActionProviderCapability, CodeActionResponse, CompletionOptions,
        CompletionParams, CompletionResponse, DiagnosticSeverity, DidChangeConfigurationParams,
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentFormattingParams,
        DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams, DocumentSymbolResponse,
        ExecuteCommandOptions, ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse,
        Hover, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, Location,
        OneOf, ReferenceParams, RenameParams, SemanticTokenModifier, SemanticTokenType,
        SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
        WorkspaceEdit,
    },
};
use tree_sitter::{Language, Query, QueryErrorKind, Tree, wasmtime::Engine};

use handlers::{diagnostic::get_diagnostics, *};
use logging::LspLogLayer;

mod handlers;
mod logging;
mod test_helpers;
mod util;

static SERVER_CAPABILITIES: LazyLock<ServerCapabilities> = LazyLock::new(|| {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(
                ["@", "\"", "\\", "(", "/", ".", "#"]
                    .map(ToOwned::to_owned)
                    .into(),
            ),
            ..CompletionOptions::default()
        }),
        document_highlight_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![SemanticTokenType::INTERFACE, SemanticTokenType::VARIABLE],
                    token_modifiers: vec![SemanticTokenModifier::DEFAULT_LIBRARY],
                },
                // TODO: Support range and delta semantic token requests.
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..Default::default()
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec!["ts-query-ls.checkImpossiblePatterns".to_string()],
            ..Default::default()
        }),
        ..Default::default()
    }
});
static ENGINE: LazyLock<Engine> = LazyLock::new(Engine::default);
static QUERY_LANGUAGE: LazyLock<Language> = LazyLock::new(|| tree_sitter_query::LANGUAGE.into());

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
struct SymbolInfo {
    label: String,
    named: bool,
}

impl fmt::Display for SymbolInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let delims = if self.named { ('(', ')') } else { ('"', '"') };
        write!(f, "{}{}{}", delims.0, self.label, delims.1)
    }
}

struct DocumentData {
    symbols_set: HashSet<SymbolInfo>,
    symbols_vec: Vec<SymbolInfo>,
    fields_set: HashSet<String>,
    fields_vec: Vec<String>,
    supertype_map: HashMap<SymbolInfo, BTreeSet<SymbolInfo>>,
    rope: Rope,
    tree: Tree,
}

struct Backend {
    client: Client,
    document_map: DashMap<Url, DocumentData>,
    options: Arc<tokio::sync::RwLock<Options>>,
    workspace_uris: Arc<RwLock<Vec<Url>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        initialize::initialize(self, params).await
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        did_change_configuration::did_change_configuration(self, params).await
    }

    async fn shutdown(&self) -> Result<()> {
        shutdown::shutdown(self).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        goto_definition::goto_definition(self, params).await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        did_open::did_open(self, params).await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        did_change::did_change(self, params).await
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        references::references(self, params).await
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        document_highlight::document_highlight(self, params).await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        rename::rename(self, params).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        completion::completion(self, params).await
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        formatting::formatting(self, params).await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        semantic_tokens_full::semantic_tokens_full(self, params).await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        hover::hover(self, params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        document_symbol::document_symbol(self, params).await
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        code_action::code_action(self, params).await
    }

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        execute_command::execute_command(self, params).await
    }
}

#[derive(Parser)]
#[command(
    name = "ts_query_ls",
    version = env!("CARGO_PKG_VERSION"),
    about = "LSP implementation for Tree-sitter's query files"
)]
struct Arguments {
    #[command(subcommand)]
    commands: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Format the query files in the given directories
    Format {
        /// List of directories to format
        directories: Vec<PathBuf>,

        /// Only check that formatting is valid, do not write
        #[arg(long, short)]
        check: bool,
    },
    /// Check the query files in the given directories for errors. This will only check for query
    /// parsing errors. Pass the `--lint` flag to also check for lint warnings. See the `--lint`
    /// flag description for more details.
    Check {
        /// List of directories to check
        directories: Vec<PathBuf>,

        /// String representing server's JSON configuration
        #[arg(long, short)]
        config: Option<String>,

        /// Check for valid formatting
        #[arg(long, short)]
        format: bool,

        /// Check for lint warnings in addition to query errors. This catches things like invalid
        /// capture names or predicate signatures (as defined by the configuration options).
        #[arg(long, short)]
        lint: bool,
    },
    /// Lint the query files in the given directories for errors. This differs from `check` because
    /// it does not perform a full semantic analysis (e.g. analyzing for impossible patterns), but
    /// it does validate that there are no invalid captures or predicates as specified by the
    /// configuration options. Useful when you don't (yet?) have access to the parser objects.
    Lint {
        /// List of directories to lint
        directories: Vec<PathBuf>,

        /// String representing server's JSON configuration
        #[arg(long, short)]
        config: Option<String>,
    },
}

fn get_scm_files(directories: &[PathBuf]) -> Vec<PathBuf> {
    directories
        .iter()
        .flat_map(|directory| {
            WalkDir::new(directory)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| {
                    e.file_type().is_file() && e.path().extension().is_some_and(|ext| ext == "scm")
                })
                .map(|e| e.path().to_owned())
        })
        .collect()
}

fn format_directories(directories: &[PathBuf], check: bool) -> i32 {
    if directories.is_empty() {
        eprintln!("No directories were specified to be formatted. No work was done.");
        return 1;
    }

    let scm_files = get_scm_files(directories);
    let exit_code = AtomicI32::new(0);

    scm_files.par_iter().for_each(|path| {
        if let Ok(contents) = fs::read_to_string(path) {
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&QUERY_LANGUAGE)
                .expect("Error loading Query grammar");
            let tree = parser.parse(contents.as_str(), None).unwrap();
            let rope = Rope::from(contents.as_str());
            if let Some(formatted) = formatting::format_document(&rope, &tree) {
                if check {
                    let edits = formatting::diff(&contents, &formatted, &rope);
                    if !edits.is_empty() {
                        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                        eprintln!(
                            "Improper formatting detected for {:?}",
                            path.canonicalize().unwrap()
                        );
                    }
                } else if fs::write(path, formatted).is_err() {
                    exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                    eprint!("Failed to write to {:?}", path.canonicalize().unwrap())
                }
            }
        } else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
        }
    });
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

fn check_directories(directories: &[PathBuf], config: String, format: bool, lint: bool) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let exit_code = AtomicI32::new(0);
    // If directories are not specified, check all files in the current directory
    let scm_files = if directories.is_empty() {
        get_scm_files(&[env::current_dir().expect("Failed to get current directory")])
    } else {
        get_scm_files(directories)
    };
    scm_files.par_iter().for_each(|path| {
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        if let Some(lang) = util::get_language(&uri, &options) {
            if let Ok(source) = fs::read_to_string(path) {
                if let Err(err) = Query::new(&lang, source.as_str()) {
                    match err.kind {
                        QueryErrorKind::Predicate => {
                            // Ignore predicate errors, which depend on the implementation.
                        }
                        _ => {
                            eprintln!("In {:?}:\n{}\n", path.canonicalize().unwrap(), err);
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                        }
                    }
                }
                if lint {
                    lint_file(path, &uri, &source, &options, &exit_code);
                }
            } else {
                eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
                exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            }
        } else {
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            eprintln!(
                "Could not retrieve language for {:?}",
                path.canonicalize().unwrap()
            )
        };
    });
    if format && format_directories(directories, true) != 0 {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
    }
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

fn lint_file(path: &Path, uri: &Url, source: &str, options: &Options, exit_code: &AtomicI32) {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");
    let tree = parser.parse(source, None).unwrap();
    let rope = Rope::from(source);
    let doc = DocumentData {
        tree,
        rope,
        // The query construction already validates node names, fields, supertypes,
        // etc.
        symbols_set: Default::default(),
        symbols_vec: Default::default(),
        fields_set: Default::default(),
        fields_vec: Default::default(),
        supertype_map: Default::default(),
    };
    let provider = &util::TextProviderRope(&doc.rope);
    let diagnostics = get_diagnostics(uri, &doc, options, provider);
    if !diagnostics.is_empty() {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
        for diagnostic in diagnostics {
            let kind = match diagnostic.severity {
                Some(DiagnosticSeverity::ERROR) => "Error",
                Some(DiagnosticSeverity::WARNING) => "Warning",
                Some(DiagnosticSeverity::INFORMATION) => "Info",
                Some(DiagnosticSeverity::HINT) => "Hint",
                _ => "Diagnostic",
            };
            eprintln!(
                "{} in \"{}\" on line {}, col {}:\n  {}",
                kind,
                path.to_str().unwrap(),
                diagnostic.range.start.line,
                diagnostic.range.start.character,
                diagnostic.message
            );
        }
    }
}

/// Lint all the given directories according to the given configuration. Linting covers things like
/// invalid capture names or predicate signatures, but not errors like invalid node names or
/// impossible patterns.
fn lint_directories(directories: &[PathBuf], config: String) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let exit_code = AtomicI32::new(0);
    // If directories are not specified, lint all files in the current directory
    let scm_files = if directories.is_empty() {
        get_scm_files(&[env::current_dir().expect("Failed to get current directory")])
    } else {
        get_scm_files(directories)
    };
    scm_files.par_iter().for_each(|path| {
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        if let Ok(source) = fs::read_to_string(path) {
            lint_file(path, &uri, &source, &options, &exit_code);
        } else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
        }
    });
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

/// Return the given config string, or read it from a config file if not given. This function can
/// exit the program.
fn get_config_str(config: Option<String>) -> String {
    match config {
        Some(config_str) => config_str,
        None => {
            let config_file_path = Path::new(".tsqueryrc.json");
            fs::read_to_string(config_file_path).unwrap_or_else(|_| {
                eprintln!("No config parameter given, and no .tsqueryrc.json found");
                std::process::exit(1);
            })
        }
    }
}

#[tokio::main]
async fn main() {
    let args = Arguments::parse();
    match args.commands {
        Some(Commands::Format { directories, check }) => {
            std::process::exit(format_directories(&directories, check));
        }
        Some(Commands::Check {
            directories,
            config,
            format,
            lint,
        }) => {
            let config_str = get_config_str(config);
            std::process::exit(check_directories(&directories, config_str, format, lint));
        }
        Some(Commands::Lint {
            directories,
            config,
        }) => {
            let config_str = get_config_str(config);
            std::process::exit(lint_directories(&directories, config_str))
        }
        None => {}
    }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let options = Arc::new(tokio::sync::RwLock::new(Options::default()));
    let (service, socket) = LspService::build(|client| {
        let lsp_layer = LspLogLayer::new(client.clone());
        tracing_subscriber::registry().with(lsp_layer).init();

        Backend {
            client,
            document_map: Default::default(),
            workspace_uris: Default::default(),
            options,
        }
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
