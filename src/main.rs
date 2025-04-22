use clap::{Parser, Subcommand};
use core::fmt;
use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator as _};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    env, fs,
    path::{Path, PathBuf},
    sync::{Arc, LazyLock, RwLock, atomic::AtomicI32},
};
use ts_query_ls::Options;
use walkdir::WalkDir;

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::Result,
    lsp_types::{
        CompletionOptions, CompletionParams, CompletionResponse, DidChangeConfigurationParams,
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentFormattingParams,
        DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams, DocumentSymbolResponse,
        GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
        InitializeParams, InitializeResult, Location, OneOf, ReferenceParams, RenameParams,
        SemanticTokenModifier, SemanticTokenType, SemanticTokensFullOptions, SemanticTokensLegend,
        SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
        SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
        TextDocumentSyncKind, TextEdit, Url, WorkspaceEdit,
    },
};
use tree_sitter::{Language, Query, QueryErrorKind, Tree, wasmtime::Engine};

use handlers::*;

mod handlers;
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
            trigger_characters: Some(["@", "\"", "\\", "(", "/"].map(ToOwned::to_owned).into()),
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

struct Backend {
    client: Client,
    document_map: DashMap<Url, Rope>,
    cst_map: DashMap<Url, Tree>,
    symbols_set_map: DashMap<Url, HashSet<SymbolInfo>>,
    symbols_vec_map: DashMap<Url, Vec<SymbolInfo>>,
    fields_set_map: DashMap<Url, HashSet<String>>,
    fields_vec_map: DashMap<Url, Vec<String>>,
    supertype_map_map: DashMap<Url, HashMap<SymbolInfo, BTreeSet<SymbolInfo>>>,
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

#[derive(clap::ValueEnum, Clone, Debug)]
enum Mode {
    Check,
    Write,
}

#[derive(Subcommand)]
enum Commands {
    /// Format the query files in the given directories
    Format {
        /// List of directories to format
        directories: Vec<PathBuf>,

        /// Operation to perform on files
        #[arg(long, short)]
        mode: Mode,
    },
    /// Check the query files in the given directories for errors
    Check {
        /// List of directories to check
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

fn format_directories(directories: &[PathBuf], mode: Mode) -> i32 {
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
                // Add newline at EOF
                let formatted = formatted + "\n";
                match mode {
                    Mode::Check => {
                        let edits = formatting::diff(&contents, &formatted, &rope);
                        if !edits.is_empty() {
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                            eprintln!(
                                "Improper formatting detected for {:?}",
                                path.canonicalize().unwrap()
                            );
                        }
                    }
                    Mode::Write => {
                        if fs::write(path, formatted).is_err() {
                            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                            eprint!("Failed to write to {:?}", path.canonicalize().unwrap())
                        };
                    }
                }
            }
        } else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
        }
    });
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

fn check_directories(directories: &[PathBuf], config: String) -> i32 {
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
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    let args = Arguments::parse();
    match args.commands {
        Some(Commands::Format { directories, mode }) => {
            std::process::exit(format_directories(&directories, mode));
        }
        Some(Commands::Check {
            directories,
            config,
        }) => {
            let config_str = match config {
                Some(config_str) => config_str,
                None => {
                    let config_file_path = Path::new(".tsqueryrc.json");
                    fs::read_to_string(config_file_path).unwrap_or_else(|_| {
                        eprintln!("No config parameter given, and no .tsqueryrc.json found");
                        std::process::exit(1);
                    })
                }
            };
            std::process::exit(check_directories(&directories, config_str));
        }
        _ => {}
    }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let options = Arc::new(tokio::sync::RwLock::new(Options {
        parser_install_directories: None,
        parser_aliases: None,
        language_retrieval_patterns: None,
        allowable_captures: HashMap::default(),
    }));
    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: Default::default(),
        cst_map: Default::default(),
        symbols_set_map: Default::default(),
        symbols_vec_map: Default::default(),
        fields_set_map: Default::default(),
        fields_vec_map: Default::default(),
        supertype_map_map: Default::default(),
        workspace_uris: Default::default(),
        options,
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
