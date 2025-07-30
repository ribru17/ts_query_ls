#[cfg(test)]
mod test {
    use rstest::rstest;
    use std::{
        collections::{BTreeMap, HashMap},
        path::Path,
        process::Command,
        sync::LazyLock,
    };
    use ts_query_ls::{Options, Predicate, PredicateParameter};

    static CONFIG: LazyLock<Options> = LazyLock::new(|| Options {
        valid_predicates: BTreeMap::from([
            (
                String::from("pred-name"),
                Predicate {
                    description: String::from("A predicate"),
                    parameters: vec![PredicateParameter {
                        description: None,
                        type_: ts_query_ls::PredicateParameterType::Any,
                        arity: ts_query_ls::PredicateParameterArity::Variadic,
                        ..Default::default()
                    }],
                },
            ),
            (
                String::from("match"),
                Predicate {
                    description: String::from("Check match"),
                    parameters: vec![PredicateParameter {
                        description: None,
                        type_: ts_query_ls::PredicateParameterType::Any,
                        arity: ts_query_ls::PredicateParameterArity::Variadic,
                        ..Default::default()
                    }],
                },
            ),
        ]),
        valid_captures: HashMap::from([
            (
                String::from("after_trailing_whitespace"),
                BTreeMap::from([(String::from("capture"), String::from("A capture."))]),
            ),
            (
                String::from("before_predicates"),
                BTreeMap::from([(String::from("type"), String::from("A type."))]),
            ),
        ]),
        ..Default::default()
    });

    #[rstest]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/after_trailing_whitespace.scm"),
        Some(["Invalid capture name \"@cap\" (fix available)"].as_slice())
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_predicates.scm"),
        Some(["Unrecognized predicate \"lua-match\""].as_slice())
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_missing.scm"),
        Some(["This pattern has no captures, and will not be processed"].as_slice())
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/formatting_test_files/before_syntax_error.scm"),
        Some(["Invalid syntax"].as_slice())
    )]
    #[case(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/example_test_files/simple.scm"),
        None
    )]
    fn cli_lint(#[case] path_str: &str, #[case] warning_messages: Option<&[&str]>) {
        // Arrange
        let path = Path::new(path_str);

        // Act
        let output = Command::new(env!("CARGO_BIN_EXE_ts_query_ls"))
            .arg("lint")
            .arg(path)
            .arg("--config")
            .arg(serde_json::to_string::<Options>(&CONFIG).unwrap())
            .output()
            .expect("Failed to wait on ts-query-ls format command");

        // Assert
        let string_output = String::from_utf8(output.stderr).unwrap();
        if let Some(messages) = warning_messages {
            for message in messages {
                assert!(string_output.contains(message));
            }
            assert_eq!(output.status.code(), Some(1));
        } else {
            assert_eq!(string_output, "");
            assert_eq!(output.status.code(), Some(0));
        }
    }
}
