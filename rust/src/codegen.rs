//! JavaScript emission from the checked source AST.

use crate::ast::*;
use crate::typecheck::CheckedModule;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodegenError {
    pub pos: Pos,
    pub message: String,
}

pub fn generate(module: &CheckedModule) -> Result<String, CodegenError> {
    Generator::default().module(&module.module)
}

pub fn generate_checked_namespaced(
    module: &CheckedModule,
    namespace: &str,
) -> Result<String, CodegenError> {
    generate_namespaced(&module.module, namespace)
}

pub fn generate_checked_linked(
    module: &CheckedModule,
    namespace: &str,
    imported_traits: BTreeMap<String, String>,
) -> Result<String, CodegenError> {
    generate_linked(&module.module, namespace, imported_traits)
}

pub fn generate_unchecked(module: &Module) -> Result<String, CodegenError> {
    Generator::default().module(module)
}

pub fn generate_namespaced(module: &Module, namespace: &str) -> Result<String, CodegenError> {
    generate_linked(module, namespace, BTreeMap::new())
}

pub fn generate_linked(
    module: &Module,
    namespace: &str,
    imported_traits: BTreeMap<String, String>,
) -> Result<String, CodegenError> {
    Generator {
        trait_namespace: namespace.into(),
        imported_traits,
        ..Generator::default()
    }
    .module(module)
}

#[derive(Default)]
struct Generator {
    next_temp: usize,
    nominal_variants: BTreeMap<Name, Vec<Name>>,
    module_aliases: BTreeSet<Name>,
    value_trait_methods: BTreeSet<Name>,
    declared_traits: BTreeSet<Name>,
    trait_namespace: String,
    imported_traits: BTreeMap<String, String>,
}

struct Value {
    statements: Vec<String>,
    value: Option<String>,
}

impl Generator {
    fn trait_key(&self, name: &str) -> String {
        if self.declared_traits.contains(name) && !self.trait_namespace.is_empty() {
            format!("{}::{name}", self.trait_namespace)
        } else {
            name.into()
        }
    }

    fn trait_reference_key(&self, reference: &Reference) -> String {
        let imported_name = match reference {
            Reference::Unqualified(name) => name.clone(),
            Reference::Qualified(module, name) => format!("{module}.{name}"),
        };
        self.imported_traits
            .get(&imported_name)
            .cloned()
            .unwrap_or_else(|| self.trait_key(reference_name(reference)))
    }

    fn temp(&mut self) -> String {
        let result = format!("${}", self.next_temp);
        self.next_temp += 1;
        result
    }

    fn module(&mut self, module: &Module) -> Result<String, CodegenError> {
        for import in &module.imports {
            match &import.kind {
                ImportType::Qualified(Some(alias)) => {
                    self.module_aliases.insert(alias.clone());
                }
                ImportType::Qualified(None)
                | ImportType::Unqualified
                | ImportType::Selective(_) => {}
            }
        }
        for declaration in &module.declarations {
            if let DeclarationKind::Data { name, variants, .. } = &declaration.kind {
                self.nominal_variants.insert(
                    name.clone(),
                    variants
                        .iter()
                        .map(|variant| variant.name.clone())
                        .collect(),
                );
            } else if let DeclarationKind::Trait { methods, .. } = &declaration.kind {
                for method in methods {
                    if !matches!(method.ty, TypeIdent::Function(_, _)) {
                        self.value_trait_methods.insert(method.name.clone());
                    }
                }
                if let DeclarationKind::Trait { name, .. } = &declaration.kind {
                    self.declared_traits.insert(name.clone());
                }
            }
        }
        let mut output = String::new();
        for declaration in &module.declarations {
            output.push_str(&self.declaration(declaration)?);
        }
        Ok(output)
    }

    fn declaration(&mut self, declaration: &Declaration) -> Result<String, CodegenError> {
        match &declaration.kind {
            DeclarationKind::Declare { .. }
            | DeclarationKind::TypeAlias { .. }
            | DeclarationKind::ExportImport(_) => Ok(String::new()),
            DeclarationKind::Let {
                pattern,
                annotation,
                value,
                ..
            } => {
                let generated = self.expression(value)?;
                let mut out = generated.statements.concat();
                if let Some(mut value) = generated.value {
                    if let Some(key) = annotation_runtime_key(annotation.as_ref()) {
                        value = format!("_rts_resolve_trait_value({value}, {})", js_string(key));
                    }
                    let binding = self.bind_pattern(pattern, &value, true, 0)?;
                    if binding.is_empty() {
                        out.push_str(&format!("{value};\n"));
                    } else {
                        out.push_str(&binding);
                    }
                }
                Ok(out)
            }
            DeclarationKind::Function { name, function, .. } => {
                let (params, prefix) = self.parameters(&function.params)?;
                let body = self.returning(&function.body, 2)?;
                Ok(format!(
                    "function {}({}) {{\n{}{}}}\n",
                    js_name(name),
                    params.join(", "),
                    indent_lines(&prefix, 2),
                    body
                ))
            }
            DeclarationKind::Data { variants, .. } => {
                let mut out = String::new();
                for variant in variants {
                    if variant.fields.is_empty() {
                        out.push_str(&format!(
                            "var {} = [{}];\n",
                            js_name(&variant.name),
                            js_string(&variant.name)
                        ));
                    } else {
                        let args = (0..variant.fields.len())
                            .map(|i| format!("a{i}"))
                            .collect::<Vec<_>>();
                        let values = std::iter::once(js_string(&variant.name))
                            .chain(args.iter().cloned())
                            .collect::<Vec<_>>();
                        out.push_str(&format!(
                            "function {}({}) {{\n  return [{}];\n}}\n",
                            js_name(&variant.name),
                            args.join(", "),
                            values.join(", ")
                        ));
                    }
                }
                Ok(out)
            }
            DeclarationKind::JsData { variants, .. } => Ok(variants
                .iter()
                .map(|v| format!("var {} = {};\n", js_name(&v.name), js_literal(&v.value)))
                .collect()),
            DeclarationKind::Exception { name, .. } => Ok(format!(
                "var {}$ = _rts_new_exception({});\n",
                js_name(name),
                js_string(name)
            )),
            DeclarationKind::Trait { name, methods } => {
                let trait_key = self.trait_key(name);
                let mut defaults = Vec::new();
                for method in methods {
                    if let Some(value) = &method.default {
                        let value = self.expression(value)?;
                        if !value.statements.is_empty() {
                            return self
                                .error(&method.pos, "trait default needs statement lifting");
                        }
                        defaults.push(format!(
                            "{}: {}",
                            method.name,
                            value.value.unwrap_or_else(|| "void 0".into())
                        ));
                    }
                }
                let mut out = format!(
                    "_rts_trait_defaults[{}] = {{{}}};\n",
                    js_string(&trait_key),
                    defaults.join(", ")
                );
                for method in methods {
                    if self.value_trait_methods.contains(&method.name) {
                        out.push_str(&format!(
                            "var {} = _rts_new_trait_value({}, {});\n",
                            js_name(&method.name),
                            js_string(&trait_key),
                            js_string(&method.name)
                        ));
                    } else {
                        out.push_str(&format!(
                            "function {}() {{ return _rts_trait_call({}, {}, arguments); }}\n",
                            js_name(&method.name),
                            js_string(&trait_key),
                            js_string(&method.name)
                        ));
                    }
                }
                Ok(out)
            }
            DeclarationKind::Impl {
                trait_name,
                impl_type,
                methods,
                ..
            } => {
                let mut entries = Vec::new();
                for (name, expression) in methods {
                    let value = self.expression(expression)?;
                    if !value.statements.is_empty() {
                        return self.error(&expression.pos, "trait method needs statement lifting");
                    }
                    entries.push(format!(
                        "{}: {}",
                        property_name(name),
                        value.value.unwrap_or_else(|| "void 0".into())
                    ));
                }
                let trait_name = self.trait_reference_key(trait_name);
                let mut out = String::new();
                if let ImplType::Record { field_function } = impl_type {
                    let transformer = self.expression(field_function)?;
                    if !transformer.statements.is_empty() {
                        return self.error(
                            &field_function.pos,
                            "record field transformer needs statement lifting",
                        );
                    }
                    out.push_str(&format!(
                        "var fieldMap = function(record) {{ var result = {{}}; Object.keys(record).forEach(function(key) {{ result[key] = ({})((record)[key]); }}); return result; }};\n",
                        transformer.value.unwrap()
                    ));
                }
                let keys = match impl_type {
                    ImplType::Nominal { name, .. } => {
                        let nominal = reference_name(name);
                        match nominal {
                            "Option" => vec!["Some".into(), "None".into()],
                            "Array" | "MutableArray" => vec!["Array".into()],
                            _ => self
                                .nominal_variants
                                .get(nominal)
                                .cloned()
                                .unwrap_or_else(|| vec![nominal.into()]),
                        }
                    }
                    ImplType::Function { arity } => vec![format!("Function/{arity}")],
                    ImplType::Record { .. } => vec!["Record".into()],
                };
                for key in keys {
                    out.push_str(&format!(
                        "_rts_register_trait({}, {}, {{{}}});\n",
                        js_string(&trait_name),
                        js_string(&key),
                        entries.join(", ")
                    ));
                    for (name, expression) in methods {
                        if self.value_trait_methods.contains(name) {
                            let value = self
                                .expression(expression)?
                                .value
                                .unwrap_or_else(|| "void 0".into());
                            out.push_str(&format!(
                                "_rts_set_trait_value({}, {}, {});\n",
                                js_name(name),
                                js_string(&key),
                                value
                            ));
                        }
                    }
                }
                Ok(out)
            }
        }
    }

    fn expression(&mut self, expression: &Expression) -> Result<Value, CodegenError> {
        let pos = &expression.pos;
        match &expression.kind {
            ExpressionKind::Error => self.error(pos, "cannot generate a recovered parse error"),
            ExpressionKind::Literal(literal) => Ok(pure(match literal {
                Literal::Integer(value) => value.to_string(),
                Literal::String(value) => js_string(value),
                Literal::Unit => "void 0".into(),
            })),
            ExpressionKind::Identifier(reference) => Ok(pure(js_reference(reference))),
            ExpressionKind::Array(_, values) | ExpressionKind::Tuple(values) => {
                let values = self.values(values)?;
                Ok(Value {
                    statements: values.0,
                    value: Some(format!("[{}]", values.1.join(", "))),
                })
            }
            ExpressionKind::Record(fields) => {
                let mut statements = Vec::new();
                let mut values = Vec::new();
                for (name, (_, expression)) in fields {
                    let value = self.expression(expression)?;
                    statements.extend(value.statements);
                    values.push(format!(
                        "{}: {}",
                        property_name(name),
                        value.value.unwrap_or_else(|| "void 0".into())
                    ));
                }
                Ok(Value {
                    statements,
                    value: Some(format!("{{{}}}", values.join(", "))),
                })
            }
            ExpressionKind::Function(function) => {
                let (params, prefix) = self.parameters(&function.params)?;
                let body = self.returning(&function.body, 2)?;
                Ok(pure(format!(
                    "(function({}) {{\n{}{}  }})",
                    params.join(", "),
                    indent_lines(&prefix, 2),
                    body
                )))
            }
            ExpressionKind::Lookup(value, name) => {
                let mut value = self.expression(value)?;
                value.value = value
                    .value
                    .map(|v| format!("({v}).{}", property_name(name)));
                Ok(value)
            }
            ExpressionKind::Apply(function, arguments) => {
                // The unsafe escape is intentionally syntactic: it must remain a raw JS expression.
                if matches!(&function.kind, ExpressionKind::Identifier(Reference::Unqualified(n)) if n == "_unsafe_js")
                {
                    if let [Expression {
                        kind: ExpressionKind::Literal(Literal::String(raw)),
                        ..
                    }] = arguments.as_slice()
                    {
                        return Ok(pure(raw.clone()));
                    }
                }
                let function = self.expression(function)?;
                let args = self.values(arguments)?;
                let mut statements = function.statements;
                statements.extend(args.0);
                Ok(Value {
                    statements,
                    value: Some(format!(
                        "{}({})",
                        function.value.unwrap_or_else(|| "void 0".into()),
                        args.1.join(", ")
                    )),
                })
            }
            ExpressionKind::MethodApply(receiver, name, arguments) => {
                let module_call = matches!(&receiver.kind,
                    ExpressionKind::Identifier(Reference::Unqualified(module)) if self.module_aliases.contains(module));
                let receiver = self.expression(receiver)?;
                let args = self.values(arguments)?;
                let mut statements = receiver.statements;
                statements.extend(args.0);
                let receiver_value = receiver.value.unwrap();
                let value = if module_call {
                    format!(
                        "({receiver_value}).{}({})",
                        property_name(name),
                        args.1.join(", ")
                    )
                } else {
                    let mut values = vec![receiver_value];
                    values.extend(args.1);
                    format!("{}({})", js_name(name), values.join(", "))
                };
                Ok(Value {
                    statements,
                    value: Some(value),
                })
            }
            ExpressionKind::Binary(op, left, right) => {
                let left = self.expression(left)?;
                let right = self.expression(right)?;
                let mut statements = left.statements;
                statements.extend(right.statements);
                Ok(Value {
                    statements,
                    value: Some(format!(
                        "({}{}{})",
                        left.value.unwrap(),
                        binary_op(*op),
                        right.value.unwrap()
                    )),
                })
            }
            ExpressionKind::Unary(UnaryOp::Negate, value) => {
                let mut value = self.expression(value)?;
                value.value = value.value.map(|v| format!("(-{v})"));
                Ok(value)
            }
            ExpressionKind::As(value, _) => self.expression(value),
            ExpressionKind::Sequence(left, right) => {
                let left = self.expression(left)?;
                let right = self.expression(right)?;
                let mut statements = left.statements;
                if let Some(value) = left.value {
                    statements.push(format!("{value};\n"));
                }
                statements.extend(right.statements);
                Ok(Value {
                    statements,
                    value: right.value,
                })
            }
            ExpressionKind::Let {
                pattern,
                annotation,
                value,
                ..
            } => {
                let value = self.expression(value)?;
                let mut statements = value.statements;
                if let Some(mut value) = value.value {
                    if let Some(key) = annotation_runtime_key(annotation.as_ref()) {
                        value = format!("_rts_resolve_trait_value({value}, {})", js_string(key));
                    }
                    let binding = self.bind_pattern(pattern, &value, true, 0)?;
                    statements.push(if binding.is_empty() {
                        format!("{value};\n")
                    } else {
                        binding
                    });
                }
                Ok(Value {
                    statements,
                    value: Some("void 0".into()),
                })
            }
            ExpressionKind::Assign(target, value) => {
                let target = self.lvalue(target)?;
                let value = self.expression(value)?;
                let mut statements = value.statements;
                statements.push(format!("{target} = {};\n", value.value.unwrap()));
                Ok(Value {
                    statements,
                    value: Some("void 0".into()),
                })
            }
            ExpressionKind::If(condition, yes, no) => {
                let condition = self.expression(condition)?;
                let temporary = self.temp();
                let mut statements = condition.statements;
                statements.push(format!("var {temporary};\n"));
                let yes = self.assigning(yes, &temporary, 2)?;
                let no = self.assigning(no, &temporary, 2)?;
                statements.push(format!(
                    "if ({}) {{\n{yes}}}\nelse {{\n{no}}}\n",
                    condition.value.unwrap()
                ));
                Ok(Value {
                    statements,
                    value: Some(temporary),
                })
            }
            ExpressionKind::While(condition, body) => {
                let condition = self.expression(condition)?;
                if !condition.statements.is_empty() {
                    return self.error(pos, "effectful while conditions are not implemented");
                }
                let body = self.discarding(body, 2)?;
                Ok(Value {
                    statements: vec![format!(
                        "while ({}) {{\n{body}}}\n",
                        condition.value.unwrap()
                    )],
                    value: Some("void 0".into()),
                })
            }
            ExpressionKind::For(pattern, over, body) => {
                let over = self.expression(over)?;
                let array = self.temp();
                let index = self.temp();
                let mut statements = over.statements;
                statements.push(format!("var {array} = {};\n", over.value.unwrap()));
                let binding = self.bind_pattern(pattern, &format!("{array}[{index}]"), true, 2)?;
                let body = self.discarding(body, 2)?;
                statements.push(format!("for (var {index} = 0; {index} < {array}.length; ++{index}) {{\n{binding}{body}}}\n"));
                Ok(Value {
                    statements,
                    value: Some("void 0".into()),
                })
            }
            ExpressionKind::Return(value) => {
                let value = self.expression(value)?;
                let mut statements = value.statements;
                statements.push(format!(
                    "return {};\n",
                    value.value.unwrap_or_else(|| "void 0".into())
                ));
                Ok(Value {
                    statements,
                    value: None,
                })
            }
            ExpressionKind::Throw(reference, value) => {
                let value = self.expression(value)?;
                let mut statements = value.statements;
                statements.push(format!(
                    "{}$.throw({}, new Error());\n",
                    js_reference(reference),
                    value.value.unwrap()
                ));
                Ok(Value {
                    statements,
                    value: None,
                })
            }
            ExpressionKind::TryCatch(body, binding, handler) => {
                let temporary = self.temp();
                let body = self.assigning(body, &temporary, 2)?;
                let exception = self.temp();
                let mut catch_prefix = String::new();
                match binding {
                    CatchBinding::Wildcard => {}
                    CatchBinding::CruxException(reference, pattern) => {
                        catch_prefix.push_str(&format!(
                            "  if (!{}$.check({exception})) {{ throw {exception}; }}\n",
                            js_reference(reference)
                        ));
                        catch_prefix.push_str(&self.bind_pattern(
                            pattern,
                            &format!("{exception}.message"),
                            true,
                            2,
                        )?);
                    }
                }
                let handler = self.assigning(handler, &temporary, 2)?;
                Ok(Value {
                    statements: vec![format!(
                        "var {temporary};\ntry {{\n{body}}} catch ({exception}) {{\n{catch_prefix}{handler}}}\n"
                    )],
                    value: Some(temporary),
                })
            }
            ExpressionKind::Match(subject, cases) => {
                let subject = self.expression(subject)?;
                let subject_name = self.temp();
                let result = self.temp();
                let mut statements = subject.statements;
                statements.push(format!(
                    "var {subject_name} = {};\nvar {result};\n",
                    subject.value.unwrap()
                ));
                let mut cascade = String::new();
                for case in cases.iter().rev() {
                    let binding = self.bind_pattern(&case.pattern, &subject_name, true, 2)?;
                    let body = self.assigning(&case.body, &result, 2)?;
                    if let Some(condition) = pattern_condition(&case.pattern, &subject_name) {
                        cascade = format!(
                            "if ({condition}) {{\n{binding}{body}}}\nelse {{\n{}{}}}\n",
                            indent_lines(&cascade, 2),
                            ""
                        );
                    } else {
                        cascade = format!("{binding}{body}");
                    }
                }
                statements.push(cascade);
                Ok(Value {
                    statements,
                    value: Some(result),
                })
            }
            ExpressionKind::TypeLookup(_, name) => Ok(pure(js_name(name))),
        }
    }

    fn values(
        &mut self,
        expressions: &[Expression],
    ) -> Result<(Vec<String>, Vec<String>), CodegenError> {
        let mut statements = Vec::new();
        let mut values = Vec::new();
        for expression in expressions {
            let value = self.expression(expression)?;
            statements.extend(value.statements);
            values.push(value.value.unwrap_or_else(|| "void 0".into()));
        }
        Ok((statements, values))
    }

    fn parameters(
        &mut self,
        params: &[FunctionParam],
    ) -> Result<(Vec<String>, String), CodegenError> {
        let mut names = Vec::new();
        let mut prefix = String::new();
        for param in params {
            match &param.pattern {
                Pattern::Binding(name) => names.push(js_name(name)),
                Pattern::Wildcard => names.push(self.temp()),
                pattern => {
                    let arg = self.temp();
                    prefix.push_str(&self.bind_pattern(pattern, &arg, true, 0)?);
                    names.push(arg);
                }
            }
        }
        Ok((names, prefix))
    }

    fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        value: &str,
        declare: bool,
        indent: usize,
    ) -> Result<String, CodegenError> {
        let pad = " ".repeat(indent);
        let prefix = if declare { "var " } else { "" };
        Ok(match pattern {
            Pattern::Wildcard => String::new(),
            Pattern::Binding(name) => format!("{pad}{prefix}{} = {value};\n", js_name(name)),
            Pattern::Tuple(patterns) | Pattern::Constructor(_, patterns) => {
                let offset = usize::from(matches!(pattern, Pattern::Constructor(_, _)));
                let mut out = String::new();
                for (index, pattern) in patterns.iter().enumerate() {
                    out.push_str(&self.bind_pattern(
                        pattern,
                        &format!("{value}[{}]", index + offset),
                        declare,
                        indent,
                    )?);
                }
                out
            }
        })
    }

    fn returning(
        &mut self,
        expression: &Expression,
        indent: usize,
    ) -> Result<String, CodegenError> {
        let value = self.expression(expression)?;
        let mut out = indent_lines(&value.statements.concat(), indent);
        if let Some(value) = value.value {
            out.push_str(&format!("{}return {value};\n", " ".repeat(indent)));
        }
        Ok(out)
    }

    fn assigning(
        &mut self,
        expression: &Expression,
        target: &str,
        indent: usize,
    ) -> Result<String, CodegenError> {
        let value = self.expression(expression)?;
        let mut out = indent_lines(&value.statements.concat(), indent);
        if let Some(value) = value.value {
            out.push_str(&format!("{}{target} = {value};\n", " ".repeat(indent)));
        }
        Ok(out)
    }

    fn discarding(
        &mut self,
        expression: &Expression,
        indent: usize,
    ) -> Result<String, CodegenError> {
        let value = self.expression(expression)?;
        let mut out = indent_lines(&value.statements.concat(), indent);
        if let Some(value) = value.value {
            out.push_str(&format!("{}{value};\n", " ".repeat(indent)));
        }
        Ok(out)
    }

    fn lvalue(&mut self, expression: &Expression) -> Result<String, CodegenError> {
        match &expression.kind {
            ExpressionKind::Identifier(reference) => Ok(js_reference(reference)),
            ExpressionKind::Lookup(value, name) => {
                let value = self.expression(value)?;
                if !value.statements.is_empty() {
                    return self.error(&expression.pos, "effectful assignment target");
                }
                Ok(format!(
                    "({}).{}",
                    value.value.unwrap(),
                    property_name(name)
                ))
            }
            _ => self.error(&expression.pos, "invalid assignment target"),
        }
    }

    fn error<T>(&self, pos: &Pos, message: impl Into<String>) -> Result<T, CodegenError> {
        Err(CodegenError {
            pos: pos.clone(),
            message: message.into(),
        })
    }
}

fn pure(value: String) -> Value {
    Value {
        statements: vec![],
        value: Some(value),
    }
}

fn js_reference(reference: &Reference) -> String {
    match reference {
        Reference::Unqualified(name) => js_name(name),
        Reference::Qualified(module, name) => {
            format!("({}).{}", js_name(module), property_name(name))
        }
    }
}

fn reference_name(reference: &Reference) -> &str {
    match reference {
        Reference::Unqualified(name) | Reference::Qualified(_, name) => name,
    }
}

fn annotation_runtime_key(annotation: Option<&TypeIdent>) -> Option<&str> {
    match annotation {
        Some(TypeIdent::Named(reference, _)) => Some(reference_name(reference)),
        Some(TypeIdent::Array(_, _)) => Some("Array"),
        _ => None,
    }
}

fn js_name(name: &str) -> String {
    const KEYWORDS: &[&str] = &[
        "break",
        "case",
        "catch",
        "class",
        "const",
        "continue",
        "debugger",
        "default",
        "delete",
        "do",
        "else",
        "export",
        "extends",
        "finally",
        "for",
        "function",
        "if",
        "import",
        "in",
        "instanceof",
        "let",
        "new",
        "return",
        "super",
        "switch",
        "this",
        "throw",
        "try",
        "typeof",
        "var",
        "void",
        "while",
        "with",
        "yield",
        "enum",
        "implements",
        "interface",
        "package",
        "private",
        "protected",
        "public",
        "static",
        "await",
        "null",
        "true",
        "false",
        "undefined",
    ];
    if KEYWORDS.contains(&name) {
        format!("{name}$")
    } else {
        name.into()
    }
}

fn property_name(name: &str) -> String {
    if name
        .chars()
        .all(|c| c == '_' || c == '$' || c.is_ascii_alphanumeric())
    {
        name.into()
    } else {
        js_string(name)
    }
}

fn js_string(value: &str) -> String {
    let mut result = String::from("\"");
    for ch in value.chars() {
        match ch {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => result.push_str(&format!("\\u{:04x}", c as u32)),
            c => result.push(c),
        }
    }
    result.push('"');
    result
}

fn js_literal(value: &JsLiteral) -> String {
    match value {
        JsLiteral::Undefined => "void 0".into(),
        JsLiteral::Null => "null".into(),
        JsLiteral::True => "true".into(),
        JsLiteral::False => "false".into(),
        JsLiteral::Integer(v) => v.to_string(),
        JsLiteral::String(v) => js_string(v),
    }
}

fn binary_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Less => "<",
        BinaryOp::Greater => ">",
        BinaryOp::LessEqual => "<=",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::Equal => "===",
        BinaryOp::NotEqual => "!==",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
    }
}

fn pattern_condition(pattern: &Pattern, value: &str) -> Option<String> {
    match pattern {
        Pattern::Wildcard | Pattern::Binding(_) | Pattern::Tuple(_) => None,
        Pattern::Constructor(reference, children) => {
            let constructor = match reference {
                Reference::Unqualified(name) | Reference::Qualified(_, name) => name,
            };
            let mut conditions = vec![if constructor == "None" {
                format!("({value} === void 0 || {value}[0] === \"None\")")
            } else {
                format!(
                    "(Array.isArray({value}) ? {value}[0] === {} : {value} === {})",
                    js_string(constructor),
                    js_reference(reference)
                )
            }];
            for (index, child) in children.iter().enumerate() {
                if let Some(condition) =
                    pattern_condition(child, &format!("{value}[{}]", index + 1))
                {
                    conditions.push(condition);
                }
            }
            Some(conditions.join(" && "))
        }
    }
}

fn indent_lines(value: &str, amount: usize) -> String {
    let pad = " ".repeat(amount);
    value.lines().map(|line| format!("{pad}{line}\n")).collect()
}
