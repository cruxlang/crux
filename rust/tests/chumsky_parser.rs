use crux::ast::{BinaryOp, DeclarationKind, Expression, ExpressionKind, Pattern, Pragma};
use crux::chumsky_parser::parse_expression;

fn ok(source: &str) -> Expression {
    let parsed = parse_expression("<chumsky-test>", source);
    assert!(
        parsed.diagnostics.is_empty(),
        "unexpected diagnostics for {source:?}: {:#?}",
        parsed.diagnostics
    );
    parsed.output.expect("parser did not produce an AST")
}

#[test]
fn precedence_and_application_are_not_ambiguous() {
    let expression = ok("1 + length(xs) * 2");
    let ExpressionKind::Binary(BinaryOp::Add, _, right) = expression.kind else {
        panic!("expected addition: {expression:#?}");
    };
    assert!(matches!(
        right.kind,
        ExpressionKind::Binary(BinaryOp::Multiply, _, _)
    ));
}

#[test]
fn parses_adjacent_layout_separated_match_arms() {
    let expression = ok("match value {\n  None => 0\n  Some(x) => x\n}");
    let ExpressionKind::Match(_, cases) = expression.kind else {
        panic!("expected match expression");
    };
    assert_eq!(cases.len(), 2);
    assert_eq!(
        cases[0].pattern,
        Pattern::Constructor("None".into(), vec![])
    );
    assert_eq!(
        cases[1].pattern,
        Pattern::Constructor("Some".into(), vec![Pattern::Binding("x".into())])
    );
}

#[test]
fn reports_misaligned_match_arms_with_a_partial_match() {
    let parsed = parse_expression(
        "layout.cx",
        "match value {\n  None => 0\n   Some(x) => x\n  Other => 2\n}",
    );
    assert!(matches!(
        parsed.output.as_ref().map(|expression| &expression.kind),
        Some(ExpressionKind::Match(_, cases)) if cases.len() == 3
    ));
    assert!(parsed
        .diagnostics
        .iter()
        .any(|error| error.message.contains("match case must begin")));
}

#[test]
fn parses_multiline_if_then_else() {
    let expression = ok("if\n True\nthen\n 1\nelse\n 2");
    assert!(matches!(expression.kind, ExpressionKind::If(_, _, _)));
}

#[test]
fn parses_nested_aligned_blocks() {
    let expression = ok("fun() {\n  if True {\n    a()\n    b()\n  }\n  c()\n}");
    let ExpressionKind::Function(function) = expression.kind else {
        panic!("expected function");
    };
    assert!(matches!(function.body.kind, ExpressionKind::Sequence(_, _)));
}

#[test]
fn reports_misaligned_block_line_but_retains_ast() {
    let parsed = parse_expression("layout.cx", "fun() {\n  first()\n   second()\n  third()\n}");
    assert!(
        parsed.output.is_some(),
        "layout validation discarded the AST"
    );
    assert!(
        parsed
            .diagnostics
            .iter()
            .any(|error| error.message.contains("block line must begin")),
        "missing alignment diagnostic: {:#?}",
        parsed.diagnostics
    );
}

#[test]
fn rejects_dedented_if_condition_without_discarding_it() {
    let parsed = parse_expression("layout.cx", "if\nTrue\nthen\n  1\nelse\n  2");
    assert!(parsed.output.is_some());
    assert!(parsed
        .diagnostics
        .iter()
        .any(|error| error.message.contains("if condition must be indented")));
}

#[test]
fn validates_multiline_application_indentation() {
    let valid = parse_expression("layout.cx", "apply(\n  first,\n  second,\n)");
    assert!(valid.diagnostics.is_empty(), "{:#?}", valid.diagnostics);

    let invalid = parse_expression("layout.cx", "apply(\nfirst,\nsecond,\n)");
    assert!(invalid.output.is_some());
    assert!(invalid.diagnostics.iter().any(|error| error
        .message
        .contains("parenthesized content must be indented")));
}

#[test]
fn distinguishes_parentheses_lambdas_and_calls() {
    let expression = ok("apply((x) => x, fun(y) { y })");
    let ExpressionKind::Apply(_, arguments) = expression.kind else {
        panic!("expected outer application");
    };
    assert_eq!(arguments.len(), 2);
    assert!(arguments
        .iter()
        .all(|argument| matches!(argument.kind, ExpressionKind::Function(_))));
}

#[test]
fn recovers_a_bad_block_line_and_continues() {
    let parsed = parse_expression(
        "recovery.cx",
        "fun() {\n  before()\n  let = 1\n  after()\n}",
    );
    assert!(
        parsed.output.is_some(),
        "recovery did not produce a partial AST"
    );
    assert!(
        !parsed.diagnostics.is_empty(),
        "recovery hid the syntax error"
    );
    assert!(contains_error(parsed.output.as_ref().unwrap()));
    assert!(contains_identifier(
        parsed.output.as_ref().unwrap(),
        "after"
    ));
}

#[test]
fn deeply_nested_parentheses_do_not_overflow() {
    // Far beyond normal source, but below the thread stack's platform-specific
    // hard limit so this remains deterministic under the test harness.
    let depth = 128;
    let source = format!("{}1{}", "(".repeat(depth), ")".repeat(depth));
    let expression = ok(&source);
    assert!(matches!(
        expression.kind,
        ExpressionKind::Literal(crux::ast::Literal::Integer(1))
    ));
}

#[test]
fn rejects_nesting_over_the_configured_bound_before_parsing() {
    let depth = crux::chumsky_parser::MAX_SYNTAX_NESTING + 1;
    let source = format!("{}0{}", "(".repeat(depth), ")".repeat(depth));
    let result = parse_expression("<test>", &source);
    assert!(result.output.is_none());
    assert!(result.diagnostics[0].message.contains("nesting exceeds"));
}

#[test]
fn parses_arrays_records_postfix_and_assignment() {
    let result = parse_expression("<test>", "target.value = f([1, 2], {x: 3}).answer");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    let output = result.output.unwrap();
    assert!(matches!(output.kind, ExpressionKind::Assign(_, _)));
}

#[test]
fn parses_loops_bindings_and_returns_in_blocks() {
    let result = parse_expression(
        "<test>",
        "fun(xs) {\n  let mutable last = 0\n  for x in xs { last = x }\n  while False { last = 1 }\n  return last\n}",
    );
    assert!(result.output.is_some(), "{:?}", result.diagnostics);
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn parses_a_complete_module_with_chumsky() {
    let source = concat!(
        "pragma { NoBuiltin }\n",
        "import support.values(Thing,)\n",
        "export data Maybe<a> { Some(a), None, }\n",
        "type Pair<a, b> = (a, b)\n",
        "declare external<T>: fun(T) -> T\n",
        "export fun mapOne<T>(f: fun(T) -> T, value: T): T {\n",
        "  f(value)\n",
        "}\n",
        "let answer: Int = 42",
    );
    let result = crux::chumsky_parser::parse_module("<test>", source);
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    let module = result.output.unwrap();
    assert_eq!(module.pragmas, vec![Pragma::NoBuiltin]);
    assert_eq!(module.imports.len(), 1);
    assert_eq!(module.declarations.len(), 5);
    assert!(module.declarations[0].exported);
}

#[test]
fn parses_trait_constraints_and_nominal_impls() {
    let source = concat!(
        "trait Display {\n",
        "  display(self): String\n",
        "}\n",
        "impl Display Number {\n",
        "  display(value) { value as String }\n",
        "}\n",
        "fun show<a: Display>(value: a): String { display(value) }",
    );
    let result = crux::chumsky_parser::parse_module("<test>", source);
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    let module = result.output.unwrap();
    assert!(matches!(
        module.declarations[0].kind,
        DeclarationKind::Trait { .. }
    ));
    assert!(matches!(
        module.declarations[1].kind,
        DeclarationKind::Impl { .. }
    ));
}

fn contains_error(expression: &Expression) -> bool {
    match &expression.kind {
        ExpressionKind::Error => true,
        ExpressionKind::Sequence(left, right) => contains_error(left) || contains_error(right),
        ExpressionKind::Function(function) => contains_error(&function.body),
        _ => false,
    }
}

fn contains_identifier(expression: &Expression, expected: &str) -> bool {
    match &expression.kind {
        ExpressionKind::Identifier(reference) => {
            matches!(reference, crux::ast::Reference::Unqualified(name) if name == expected)
        }
        ExpressionKind::Apply(function, arguments) => {
            contains_identifier(function, expected)
                || arguments
                    .iter()
                    .any(|argument| contains_identifier(argument, expected))
        }
        ExpressionKind::Sequence(left, right) => {
            contains_identifier(left, expected) || contains_identifier(right, expected)
        }
        ExpressionKind::Function(function) => contains_identifier(&function.body, expected),
        _ => false,
    }
}
