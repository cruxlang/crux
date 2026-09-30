fn generate(source: &str) -> String {
    let module = crux::parse("<test>", source).unwrap();
    let checked = crux::typecheck::check(&module).unwrap();
    crux::codegen::generate(&checked).unwrap()
}

#[test]
fn emits_functions_and_returns() {
    assert_eq!(
        generate("fun f() { return \"1\" }"),
        "function f() {\n  return \"1\";\n}\n"
    );
}

#[test]
fn emits_jsffi_data() {
    assert_eq!(
        generate("data jsffi JST { A = undefined, B = null, C = true, D = false, E = 10, F = \"hi\", }"),
        "var A = void 0;\nvar B = null;\nvar C = true;\nvar D = false;\nvar E = 10;\nvar F = \"hi\";\n"
    );
}

#[test]
fn emits_value_producing_branches() {
    assert_eq!(
        generate("let x = if True then \"1\" else \"2\""),
        "var $0;\nif (True) {\n  $0 = \"1\";\n}\nelse {\n  $0 = \"2\";\n}\nvar x = $0;\n"
    );
}
