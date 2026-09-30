#[test]
fn parses_every_repository_source_with_chumsky() {
    fn visit(path: &std::path::Path, failures: &mut Vec<String>) {
        if path.is_dir() {
            for entry in std::fs::read_dir(path).unwrap() {
                visit(&entry.unwrap().path(), failures);
            }
        } else if path.extension().is_some_and(|ext| ext == "cx") {
            let source = std::fs::read_to_string(path).unwrap();
            let result = crux::chumsky_parser::parse_module(&path.display().to_string(), &source);
            if result.output.is_none() || !result.diagnostics.is_empty() {
                failures.push(format!(
                    "{}: {:?}",
                    path.display(),
                    result.diagnostics.first()
                ));
            }
        }
    }

    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut failures = vec![];
    for directory in ["lib", "test-data", "examples", "tests"] {
        visit(&root.join(directory), &mut failures);
    }
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
