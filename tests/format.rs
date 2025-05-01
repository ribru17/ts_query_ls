#[cfg(test)]
mod test {
    use rstest::{Context, rstest};
    use std::{env::temp_dir, fs, process::Command};

    #[rstest]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_trailing_whitespace.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_trailing_whitespace.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_predicates.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_predicates.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_missing.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_missing.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_syntax_error.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_syntax_error.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/before_complex.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/queries/formatting_test_files/after_complex.scm")),
    )]
    fn cli_formatting(#[context] ctx: Context, #[case] before: &str, #[case] after: &str) {
        // Arrange
        let path = temp_dir()
            .join("ts-query-ls")
            .join(ctx.case.unwrap().to_string())
            .join("test.scm");
        fs::create_dir_all(path.parent().unwrap()).expect("Failed to create test case directory");
        fs::write(&path, before).expect("Failed to write test file");

        // Act
        Command::new(env!("CARGO_BIN_EXE_ts_query_ls"))
            .arg("format")
            .arg(&path)
            .output()
            .expect("Failed to wait on ts-query-ls format command");

        // Assert
        let formatted = fs::read_to_string(&path).expect("Failed to read test file");
        _ = fs::remove_file(path); // ignore cleanup errors
        assert_eq!(after, formatted);
    }
}
