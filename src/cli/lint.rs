use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::{Arc, atomic::AtomicI32},
};

use futures::future::join_all;
use ropey::Rope;
use tower_lsp::lsp_types::{CodeAction, CodeActionOrCommand, DiagnosticSeverity, Url};
use ts_query_ls::Options;

use crate::{
    DocumentData, LanguageData, QUERY_LANGUAGE,
    handlers::{code_action::diag_to_code_action, diagnostic::get_diagnostics},
    util::{edit_rope, get_language_name, get_scm_files},
};

pub(super) async fn lint_file(
    path: &Path,
    uri: &Url,
    source: &str,
    options: Arc<tokio::sync::RwLock<Options>>,
    fix: bool,
    language_data: Option<Arc<LanguageData>>,
    exit_code: &AtomicI32,
) -> Option<String> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&QUERY_LANGUAGE)
        .expect("Error loading Query grammar");
    let tree = parser.parse(source, None).unwrap();
    let rope = Rope::from(source);
    let language_name = get_language_name(uri, &*options.read().await);
    let doc = DocumentData {
        tree,
        rope,
        language_name,
        version: Default::default(),
        // TODO: Calculate these here?
        imported_uris: Default::default(),
    };
    // The query construction already validates node names, fields, supertypes,
    // etc.
    let diagnostics = get_diagnostics(uri, doc.clone(), language_data, options, false).await;
    if diagnostics.is_empty() {
        return None;
    }

    let path_str = path.to_str().unwrap();
    let mut edits = Vec::new();
    if !fix {
        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
    }
    let mut unfixed_issues = 0;
    for diagnostic in diagnostics {
        if !fix {
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
                path_str,
                diagnostic.range.start.line,
                diagnostic.range.start.character,
                diagnostic.message
            );
        } else {
            let Some(action) = diag_to_code_action(&doc.tree, &doc.rope, diagnostic, uri) else {
                unfixed_issues += 1;
                continue;
            };
            let CodeActionOrCommand::CodeAction(CodeAction {
                edit: Some(edit), ..
            }) = action
            else {
                continue;
            };
            let Some(mut changes) = edit.changes.and_then(|mut changes| changes.remove(uri)) else {
                continue;
            };
            edits.append(&mut changes);
        }
    }
    if unfixed_issues > 0 {
        let plurality = if unfixed_issues > 1 { "s" } else { "" };
        println!("{path_str}: {unfixed_issues} issue{plurality} could not be fixed automatically");
    }
    if !fix || edits.is_empty() {
        return None;
    }
    edits.sort_unstable_by(|a, b| b.range.start.cmp(&a.range.start));
    let mut rope = doc.rope;
    for edit in edits {
        let range = edit.range;
        let new_text = edit.new_text;
        edit_rope(&mut rope, range, &new_text);
    }
    Some(rope.to_string())
}

/// Lint all the given directories according to the given configuration. Linting covers things like
/// invalid capture names or predicate signatures, but not errors like invalid node names or
/// impossible patterns.
pub async fn lint_directories(directories: &[PathBuf], config: String, fix: bool) -> i32 {
    let Ok(options) = serde_json::from_str::<Options>(&config) else {
        eprintln!("Could not parse the provided configuration");
        return 1;
    };
    let options: Arc<tokio::sync::RwLock<Options>> = Arc::new(options.into());
    let exit_code = Arc::new(AtomicI32::new(0));
    // If directories are not specified, lint all files in the current directory
    let directories = if directories.is_empty() {
        &[env::current_dir().expect("Failed to get current directory")]
    } else {
        directories
    };
    let scm_files = get_scm_files(directories);
    let tasks = scm_files.into_iter().filter_map(|path| {
        let uri = Url::from_file_path(path.canonicalize().unwrap()).unwrap();
        let exit_code = exit_code.clone();
        let options = options.clone();
        if let Ok(source) = fs::read_to_string(&path) {
            Some(tokio::spawn(async move {
                if let Some(new_source) =
                    lint_file(&path, &uri, &source, options, fix, None, &exit_code).await
                {
                    if fs::write(&path, new_source).is_err() {
                        eprintln!("Failed to write {:?}", path.canonicalize().unwrap());
                        exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
                    }
                };
            }))
        } else {
            eprintln!("Failed to read {:?}", path.canonicalize().unwrap());
            exit_code.store(1, std::sync::atomic::Ordering::Relaxed);
            None
        }
    });
    join_all(tasks).await;
    exit_code.load(std::sync::atomic::Ordering::Relaxed)
}
