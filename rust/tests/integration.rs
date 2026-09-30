use crux::compiler::{compile_path, compile_path_executable, CompileError};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug)]
struct ErrorExpectation {
    error_name: Option<String>,
    type_error_name: Option<String>,
}

#[test]
fn runs_every_integration_fixture() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/integration");
    assert!(
        root.is_dir(),
        "missing integration directory: {}",
        root.display()
    );
    assert!(
        Command::new("node")
            .arg("--version")
            .output()
            .is_ok_and(|output| output.status.success()),
        "unable to launch node; install Node.js to run integration tests"
    );

    let mut fixtures = Vec::new();
    discover_fixtures(&root, &mut fixtures).unwrap();
    fixtures.sort();
    assert!(!fixtures.is_empty(), "no integration fixtures found");

    let mut failures = Vec::new();
    for fixture in &fixtures {
        let name = fixture.strip_prefix(&root).unwrap_or(fixture);
        println!("testing program {}", name.join("main.cx").display());
        if let Err(message) = run_fixture(fixture) {
            failures.push(format!("{}: {message}", name.display()));
        }
    }

    assert!(
        failures.is_empty(),
        "{} of {} integration fixtures failed:\n{}",
        failures.len(),
        fixtures.len(),
        failures.join("\n\n")
    );
}

fn discover_fixtures(path: &Path, fixtures: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = std::fs::read_dir(path)
        .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("cannot read directory entry: {error}"))?;
        let entry_path = entry.path();
        if entry_path.is_dir() {
            discover_fixtures(&entry_path, fixtures)?;
        } else if entry_path.file_name().is_some_and(|name| name == "main.cx") {
            fixtures.push(path.to_owned());
        }
    }
    Ok(())
}

fn run_fixture(fixture: &Path) -> Result<(), String> {
    let main = fixture.join("main.cx");
    let stdout_path = fixture.join("stdout.txt");
    let error_path = fixture.join("error.yaml");
    match (stdout_path.is_file(), error_path.is_file()) {
        (true, false) => run_success_fixture(&main, &stdout_path),
        (false, true) => run_error_fixture(&main, &error_path),
        (true, true) => Err("contains both stdout.txt and error.yaml".into()),
        (false, false) => Err("needs either stdout.txt or error.yaml".into()),
    }
}

fn run_success_fixture(main: &Path, stdout_path: &Path) -> Result<(), String> {
    let expected = std::fs::read(stdout_path)
        .map_err(|error| format!("cannot read {}: {error}", stdout_path.display()))?;
    let javascript = compile_path_executable(main).map_err(|error| format!("compile: {error}"))?;
    let output = Command::new("node")
        .arg("-e")
        .arg(javascript)
        .output()
        .map_err(|error| format!("cannot execute node: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "node exited with {}:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    if output.stdout != expected {
        return Err(format!(
            "stdout mismatch\nexpected: {:?}\n  actual: {:?}",
            String::from_utf8_lossy(&expected),
            String::from_utf8_lossy(&output.stdout)
        ));
    }
    Ok(())
}

fn run_error_fixture(main: &Path, error_path: &Path) -> Result<(), String> {
    let expectation = parse_error_expectation(error_path)?;
    let error = compile_path(main)
        .err()
        .ok_or_else(|| format!("compiled successfully; expected {:?}", expectation))?;
    assert_error_matches(&expectation, &error)
}

fn parse_error_expectation(path: &Path) -> Result<ErrorExpectation, String> {
    let source = std::fs::read_to_string(path)
        .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
    let mut expectation = ErrorExpectation {
        error_name: None,
        type_error_name: None,
    };
    for (index, line) in source.lines().enumerate() {
        let line = line.split('#').next().unwrap_or_default().trim();
        if line.is_empty() {
            continue;
        }
        let (key, value) = line
            .split_once(':')
            .ok_or_else(|| format!("{}:{}: expected key: value", path.display(), index + 1))?;
        let value = value.trim().trim_matches(['\'', '"']).to_owned();
        match key.trim() {
            "error-name" => expectation.error_name = Some(value),
            "type-error-name" => expectation.type_error_name = Some(value),
            key => {
                return Err(format!(
                    "{}:{}: unknown key {key}",
                    path.display(),
                    index + 1
                ))
            }
        }
    }
    if expectation.error_name.is_none() && expectation.type_error_name.is_none() {
        return Err(format!("{}: empty error expectation", path.display()));
    }
    Ok(expectation)
}

fn assert_error_matches(
    expectation: &ErrorExpectation,
    error: &CompileError,
) -> Result<(), String> {
    if let Some(expected) = &expectation.error_name {
        let actual = error.error_name();
        if actual != expected {
            return Err(format!(
                "expected error-name {expected:?}, got {actual:?}: {error}"
            ));
        }
    }
    if let Some(expected) = &expectation.type_error_name {
        let actual = error.type_error_name().ok_or_else(|| {
            format!(
                "expected type-error-name {expected:?}, got {}: {error}",
                error.error_name()
            )
        })?;
        if actual != expected {
            return Err(format!(
                "expected type-error-name {expected:?}, got {actual:?}: {error}"
            ));
        }
    }
    Ok(())
}
