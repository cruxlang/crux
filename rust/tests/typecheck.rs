use crux::typecheck::{check, TypeErrorKind};

fn check_source(
    source: &str,
) -> Result<crux::typecheck::CheckedModule, crux::typecheck::TypeError> {
    let module = crux::parse("<test>", source).expect("source should parse");
    check(&module)
}

#[test]
fn checks_functions_applications_and_control_flow() {
    check_source(
        "data Boolean { True, False }\n\
         fun choose(x: Boolean, a: Int, b: Int): Int { if x then a else b }\n\
         let answer: Int = choose(True, 40, 2)",
    )
    .unwrap();
}

#[test]
fn rejects_incompatible_branches() {
    let error = check_source("data Boolean { True, False }\nlet bad = if True then 1 else \"no\"")
        .unwrap_err();
    assert!(matches!(error.kind, TypeErrorKind::CannotUnify(_, _)));
}

#[test]
fn rejects_infinite_function_types() {
    let error = check_source("fun f(x) { x(x) }").unwrap_err();
    assert!(matches!(error.kind, TypeErrorKind::OccursCheck(_, _)));
}

#[test]
fn record_lookup_uses_an_open_row() {
    check_source("fun getX(r) { r.x }\nlet x = getX({x: 1, y: \"kept\"})").unwrap();
}

#[test]
fn immutable_bindings_cannot_be_assigned() {
    let error = check_source("let x = 1\nlet _ = x = 2").unwrap_err();
    assert_eq!(error.kind, TypeErrorKind::ImmutableAssignment);
}

#[test]
fn immutable_functions_are_generalized() {
    check_source("fun identity(x) { x }\nlet number = identity(1)\nlet text = identity(\"ok\")")
        .unwrap();
}

#[test]
fn expands_parameterized_and_forwarding_aliases() {
    check_source(
        "type Scalar = Number\n\
         type Vector = Array\n\
         type Boxed<a> = Array<a>\n\
         let scalar: Scalar = 1\n\
         let first: Vector<String> = [\"a\"]\n\
         let second: Boxed<Number> = [2]",
    )
    .unwrap();
}
