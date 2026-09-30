use crux::ast::*;
use crux::parse;

fn parse_one(source: &str) -> DeclarationKind {
    let mut module = parse("<test>", source).unwrap();
    assert_eq!(module.declarations.len(), 1);
    module.declarations.remove(0).kind
}

#[test]
fn parses_let_and_operator_precedence() {
    let decl = parse_one("let a: Number = 1 + length(xs) * 2");
    let DeclarationKind::Let {
        pattern,
        annotation,
        value,
        ..
    } = decl
    else {
        panic!("not a let")
    };
    assert_eq!(pattern, Pattern::Binding("a".into()));
    assert_eq!(annotation, Some(TypeIdent::Named("Number".into(), vec![])));
    let ExpressionKind::Binary(BinaryOp::Add, left, right) = value.kind else {
        panic!("not addition")
    };
    assert_eq!(left.kind, ExpressionKind::Literal(Literal::Integer(1)));
    assert!(matches!(
        right.kind,
        ExpressionKind::Binary(BinaryOp::Multiply, _, _)
    ));
}

#[test]
fn parses_data_and_type_variable_position() {
    let decl = parse_one("data Maybe<a> { Some(a), None, };");
    let DeclarationKind::Data {
        name,
        type_vars,
        variants,
    } = decl
    else {
        panic!("not data")
    };
    assert_eq!(name, "Maybe");
    assert_eq!(type_vars[0].name, "a");
    assert_eq!((type_vars[0].pos.line, type_vars[0].pos.column), (1, 12));
    assert_eq!(variants.len(), 2);
    assert_eq!(variants[0].name, "Some");
}

#[test]
fn parses_multiline_match() {
    let decl = parse_one("let x = match hoot {\n  Nil => 0\n  Cons(a, b) => a\n}");
    let DeclarationKind::Let { value, .. } = decl else {
        panic!("not let")
    };
    let ExpressionKind::Match(_, cases) = value.kind else {
        panic!("not match")
    };
    assert_eq!(cases.len(), 2);
    assert_eq!(cases[0].pattern, Pattern::Constructor("Nil".into(), vec![]));
}

#[test]
fn parses_function_and_trailing_commas() {
    let decl = parse_one("fun f<T>(x: T): [T] {\n  [x,]\n}");
    let DeclarationKind::Function {
        name,
        type_vars,
        function,
    } = decl
    else {
        panic!("not function")
    };
    assert_eq!(name, "f");
    assert_eq!(type_vars.len(), 1);
    assert_eq!(function.params.len(), 1);
    assert_eq!(
        function.return_type,
        Some(TypeIdent::Array(
            Mutability::Immutable,
            Box::new(TypeIdent::Named("T".into(), vec![]))
        ))
    );
}

#[test]
fn parses_import_forms() {
    let module = parse(
        "x.cx",
        "import foo.bar(...)\nimport x.y(A, B,)\nimport z as _\nlet value = 1",
    )
    .unwrap();
    assert_eq!(module.imports.len(), 3);
    assert_eq!(module.imports[0].kind, ImportType::Unqualified);
    assert_eq!(
        module.imports[1].kind,
        ImportType::Selective(vec!["A".into(), "B".into()])
    );
    assert_eq!(module.imports[2].kind, ImportType::Qualified(None));
}

#[test]
fn parses_repository_sources() {
    fn visit(path: &std::path::Path, failures: &mut Vec<String>) {
        if path.is_dir() {
            for entry in std::fs::read_dir(path).unwrap() {
                visit(&entry.unwrap().path(), failures);
            }
        } else if path.extension().is_some_and(|ext| ext == "cx") {
            let source = std::fs::read_to_string(path).unwrap();
            if let Err(error) = parse(&path.display().to_string(), &source) {
                failures.push(error.to_string());
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
        "failed to parse:\n{}",
        failures.join("\n")
    );
}
