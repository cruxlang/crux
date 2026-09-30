#[test]
fn typechecks_the_existing_single_module_corpus() {
    fn visit(path: &std::path::Path, failures: &mut Vec<String>, successes: &mut usize) {
        if path.is_dir() {
            for entry in std::fs::read_dir(path).unwrap() {
                visit(&entry.unwrap().path(), failures, successes);
            }
        } else if path.file_name().is_some_and(|name| name == "main.cx") {
            // Import-cycle validation belongs to `compiler::compile_path` and
            // has its own project-loader test.
            if path
                .to_string_lossy()
                .contains("errors/circular-imports/main.cx")
            {
                return;
            }
            let source = std::fs::read_to_string(path).unwrap();
            let parsed = crux::parse(&path.display().to_string(), &source).unwrap();
            let expects_error = path.parent().unwrap().join("error.yaml").exists();
            match (expects_error, crux::typecheck::check(&parsed)) {
                (false, Ok(_)) | (true, Err(_)) => *successes += 1,
                (false, Err(error)) => failures.push(format!(
                    "unexpected rejection {}: {}",
                    path.display(),
                    error
                )),
                (true, Ok(_)) => failures.push(format!("unexpected acceptance {}", path.display())),
            }
        }
    }
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut failures = vec![];
    let mut successes = 0;
    for directory in ["test-data", "examples"] {
        visit(&root.join(directory), &mut failures, &mut successes);
    }
    assert!(
        failures.is_empty(),
        "{successes} successes, {} failures\n{}",
        failures.len(),
        failures.join("\n")
    );
}
