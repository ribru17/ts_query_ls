use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::atomic::AtomicI32,
};

use rayon::iter::{IntoParallelRefIterator as _, ParallelIterator as _};
use ropey::Rope;
use tower_lsp::lsp_types::{DiagnosticSeverity, Url};
use ts_query_ls::Options;

use crate::{
    DocumentData, QUERY_LANGUAGE,
    handlers::diagnostic::get_diagnostics,
    util::{self, get_language_name},
};

use super::get_scm_files;

pub(super) fn lint_file(
    path: &Path,
    uri: &Url,
    source: &str,
    options: &Options,
    exit_code: &AtomicI32,
) {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");
    let tree = parser.parse(source, None).unwrap();
    let rope = Rope::from(source);
    let language_name = get_language_name(uri, options);
    let doc = DocumentData {
        tree,
        rope,
        language_name,
        version: Default::default(),
    };
    let provider = &util::TextProviderRope(&doc.rope);
    // The query construction already validates node names, fields, supertypes,
    // etc.
    let diagnostics = get_diagnostics(uri, &doc, None, options, provider);
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
pub fn lint_directories(directories: &[PathBuf], config: String) -> i32 {
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
