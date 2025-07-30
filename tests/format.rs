#[cfg(test)]
mod test {
    use rstest::{Context, rstest};
    use std::{env::temp_dir, fs, path::Path, process::Command};

    #[rstest]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_trailing_whitespace.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_trailing_whitespace.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_predicates.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_predicates.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_missing.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_missing.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_syntax_error.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_syntax_error.scm")),
    )]
    #[case(
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_complex.scm")),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_complex.scm")),
    )]
    fn cli_format_write(#[context] ctx: Context, #[case] before: &str, #[case] after: &str) {
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

    enum FormatValidity {
        Valid,
        Invalid,
        SyntaxError,
    }

    #[rstest]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_trailing_whitespace.scm"),
        FormatValidity::Invalid
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_trailing_whitespace.scm"),
        FormatValidity::Valid
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_predicates.scm"),
        FormatValidity::Invalid
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_missing.scm"),
        FormatValidity::Invalid
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_syntax_error.scm"),
        FormatValidity::SyntaxError // NOTE: Files containing syntax errors cannot be formatted
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_complex.scm"),
        FormatValidity::Valid
    )]
    fn cli_format_validate(#[case] path_str: &str, #[case] valid: FormatValidity) {
        // Arrange
        let path = Path::new(path_str);

        // Act
        let output = Command::new(env!("CARGO_BIN_EXE_ts_query_ls"))
            .arg("format")
            .arg("--check")
            .arg(path)
            .output()
            .expect("Failed to wait on ts-query-ls format command");

        // Assert
        match valid {
            FormatValidity::Valid => {
                assert!(output.stderr.is_empty());
                assert_eq!(output.status.code(), Some(0));
            }
            FormatValidity::Invalid => {
                assert!(String::from_utf8(output.stderr).unwrap().contains(path_str));
                assert_eq!(output.status.code(), Some(1));
            }
            FormatValidity::SyntaxError => {
                assert!(
                    String::from_utf8(output.stderr)
                        .unwrap()
                        .contains("No formatting performed -- invalid syntax detected at")
                );
                assert_eq!(output.status.code(), Some(1));
            }
        }
    }
}
