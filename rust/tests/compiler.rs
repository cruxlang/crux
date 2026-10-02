#[test]
fn compiles_through_the_chumsky_parser_typechecker_and_backend() {
    let javascript = crux::compiler::compile(
        "<test>",
        "data Boolean { True, False }\nfun select(flag: Boolean): Int { if flag then 1 else 2 }\nlet answer = select(True)",
    )
    .unwrap();
    assert!(javascript.contains("function select(flag)"));
    assert!(javascript.contains("var answer"));
}

#[test]
fn project_loader_rejects_circular_imports() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let path = root.join("tests/integration/errors/circular-imports/main.cx");
    let error = crux::compiler::compile_path(path).unwrap_err();
    assert!(matches!(
        error,
        crux::compiler::CompileError::CircularImport(_)
    ));
}

#[test]
fn compiles_an_in_memory_project_without_filesystem_access() {
    let sources = std::collections::BTreeMap::from([
        (
            "main.cx".into(),
            "import greet(...)\nfun main() { hello() }".into(),
        ),
        (
            "greet.cx".into(),
            "export fun hello() { print(\"hello\") }".into(),
        ),
    ]);
    let javascript = crux::compiler::compile_in_memory_executable("main.cx", &sources).unwrap();
    assert!(javascript.contains("$module_main.main();"));
    assert!(javascript.contains("function hello()"));
}
