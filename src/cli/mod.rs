use std::path::PathBuf;

use walkdir::WalkDir;

pub(super) mod check;
pub(super) mod format;
pub(super) mod lint;

pub(in crate::cli) fn get_scm_files(directories: &[PathBuf]) -> Vec<PathBuf> {
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
