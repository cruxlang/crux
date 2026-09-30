//! End-to-end compiler entry point.

use crate::{ast::Module, chumsky_parser, codegen, typecheck};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub enum CompileError {
    Parse(Vec<chumsky_parser::Diagnostic>),
    Type(typecheck::TypeError),
    Codegen(codegen::CodegenError),
    Io { path: PathBuf, message: String },
    CircularImport(Vec<PathBuf>),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(errors) => {
                for (index, error) in errors.iter().enumerate() {
                    if index != 0 {
                        writeln!(f)?;
                    }
                    write!(
                        f,
                        "{}:{}:{}: {}",
                        error.pos.file, error.pos.line, error.pos.column, error.message
                    )?;
                }
                Ok(())
            }
            Self::Type(error) => error.fmt(f),
            Self::Codegen(error) => write!(
                f,
                "{}:{}:{}: {}",
                error.pos.file, error.pos.line, error.pos.column, error.message
            ),
            Self::Io { path, message } => write!(f, "{}: {message}", path.display()),
            Self::CircularImport(paths) => write!(
                f,
                "circular import: {}",
                paths
                    .iter()
                    .map(|p| p.display().to_string())
                    .collect::<Vec<_>>()
                    .join(" -> ")
            ),
        }
    }
}

impl CompileError {
    /// Stable top-level diagnostic category used by integration fixtures.
    pub fn error_name(&self) -> &'static str {
        match self {
            Self::Parse(_) => "parse-error",
            Self::Type(_) => "type-error",
            Self::Codegen(_) => "codegen-error",
            Self::Io { .. } => "io-error",
            Self::CircularImport(_) => "circular-import",
        }
    }

    pub fn type_error_name(&self) -> Option<&'static str> {
        match self {
            Self::Type(error) => Some(error.name()),
            _ => None,
        }
    }
}

/// Compile a program rooted at a source file. Imports are loaded from the
/// program directory first and then from the repository's `lib` directory.
/// Loading is deliberately separate from type inference so cycles are
/// diagnosed deterministically before any module is checked.
pub fn compile_path(path: impl AsRef<Path>) -> Result<String, CompileError> {
    let path = path.as_ref();
    let project_root = path.parent().unwrap_or_else(|| Path::new("."));
    let base_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("lib");
    let mut modules = BTreeMap::new();
    let mut visiting = Vec::new();
    let mut complete = BTreeSet::new();
    load_module_graph(
        path,
        project_root,
        &base_root,
        &mut visiting,
        &mut complete,
        &mut modules,
    )?;
    bundle_modules(path, project_root, &base_root, &modules)
}

pub fn compile_path_executable(path: impl AsRef<Path>) -> Result<String, CompileError> {
    let path = path.as_ref();
    let body = compile_path(path)?;
    Ok(format!(
        "{}\n{}\n{body}\n$module_main.main();\n",
        include_str!("../../rts/rts.js"),
        runtime_prelude()
    ))
}

fn bundle_modules(
    main: &Path,
    project_root: &Path,
    base_root: &Path,
    modules: &BTreeMap<PathBuf, Module>,
) -> Result<String, CompileError> {
    let names = modules
        .keys()
        .enumerate()
        .map(|(index, path)| {
            (
                path.clone(),
                if path == main {
                    "$module_main".into()
                } else {
                    format!("$module_{index}")
                },
            )
        })
        .collect::<BTreeMap<_, String>>();
    let mut emitted = BTreeSet::new();
    let mut output = String::new();
    emit_module(
        main,
        project_root,
        base_root,
        modules,
        &names,
        &mut emitted,
        &mut output,
    )?;
    Ok(output)
}

fn emit_module(
    path: &Path,
    project_root: &Path,
    base_root: &Path,
    modules: &BTreeMap<PathBuf, Module>,
    names: &BTreeMap<PathBuf, String>,
    emitted: &mut BTreeSet<PathBuf>,
    output: &mut String,
) -> Result<(), CompileError> {
    if !emitted.insert(path.to_owned()) {
        return Ok(());
    }
    let module = modules.get(path).ok_or_else(|| CompileError::Io {
        path: path.to_owned(),
        message: "loaded module missing from graph".into(),
    })?;
    let mut dependencies = Vec::new();
    for import in &module.imports {
        let dependency = resolve_import(import, project_root, base_root);
        emit_module(
            &dependency,
            project_root,
            base_root,
            modules,
            names,
            emitted,
            output,
        )?;
        dependencies.push((import, dependency));
    }
    let module_name = &names[path];
    let imported_traits =
        imported_trait_keys(&dependencies, project_root, base_root, modules, names);
    let generated = if module_name == "$module_main" {
        let checked = typecheck::check(module).map_err(CompileError::Type)?;
        codegen::generate_checked_linked(&checked, module_name, imported_traits)
    } else {
        codegen::generate_linked(module, module_name, imported_traits)
    }
    .map_err(CompileError::Codegen)?;
    output.push_str(&format!("var {module_name} = (function() {{\n"));
    for (import, dependency) in &dependencies {
        let dependency_name = &names[dependency];
        match &import.kind {
            crate::ast::ImportType::Qualified(Some(alias)) => {
                output.push_str(&format!(
                    "var {} = {dependency_name};\n",
                    js_identifier(alias)
                ));
                // Type-directed method syntax can select a function from a
                // qualified module even when the source call does not repeat
                // the module name (`value->method()`). Make those exported
                // candidates available to the generated dispatcher call.
                for name in exported_names(
                    dependency,
                    project_root,
                    base_root,
                    modules,
                    &mut BTreeSet::new(),
                ) {
                    if matches!(
                        name.as_str(),
                        "toString" | "print" | "range" | "replicate" | "each" | "sorted"
                    ) {
                        continue;
                    }
                    output.push_str(&format!(
                        "var {} = {dependency_name}[{}];\n",
                        js_identifier(&name),
                        js_string(&name)
                    ));
                }
            }
            crate::ast::ImportType::Selective(selected) => {
                for name in selected {
                    output.push_str(&format!(
                        "var {} = {dependency_name}[{}];\n",
                        js_identifier(name),
                        js_string(name)
                    ));
                }
            }
            crate::ast::ImportType::Unqualified | crate::ast::ImportType::Qualified(None) => {
                for name in exported_names(
                    dependency,
                    project_root,
                    base_root,
                    modules,
                    &mut BTreeSet::new(),
                ) {
                    output.push_str(&format!(
                        "var {} = {dependency_name}[{}];\n",
                        js_identifier(&name),
                        js_string(&name)
                    ));
                }
            }
        }
    }
    output.push_str(&generated);
    output.push_str("var $exports = {");
    let mut exports = exported_bindings(module);
    if module_name == "$module_main" {
        if module.declarations.iter().any(|declaration| matches!(&declaration.kind, crate::ast::DeclarationKind::Function { name, .. } if name == "main")) {
            exports.push(("main".into(), "main".into()));
        }
    }
    output.push_str(
        &exports
            .into_iter()
            .map(|(name, local)| format!("{}: {}", js_string(&name), local))
            .collect::<Vec<_>>()
            .join(", "),
    );
    output.push_str("};\n");
    for declaration in &module.declarations {
        if let crate::ast::DeclarationKind::ExportImport(imported) = &declaration.kind {
            if let Some((_, dependency)) = dependencies
                .iter()
                .find(|(import, _)| import.module.last() == Some(imported))
            {
                output.push_str(&format!(
                    "Object.assign($exports, {});\n",
                    names[dependency]
                ));
            }
        }
    }
    output.push_str("return $exports;\n})();\n");
    Ok(())
}

fn imported_trait_keys(
    dependencies: &[(&crate::ast::Import, PathBuf)],
    project_root: &Path,
    base_root: &Path,
    modules: &BTreeMap<PathBuf, Module>,
    names: &BTreeMap<PathBuf, String>,
) -> BTreeMap<String, String> {
    let mut result = BTreeMap::new();
    for (import, dependency) in dependencies {
        let exported = exported_trait_keys(
            dependency,
            project_root,
            base_root,
            modules,
            names,
            &mut BTreeSet::new(),
        );
        match &import.kind {
            crate::ast::ImportType::Qualified(Some(alias)) => {
                for (trait_name, key) in exported {
                    result.insert(format!("{alias}.{trait_name}"), key);
                }
            }
            crate::ast::ImportType::Selective(selected) => {
                for name in selected {
                    if let Some(key) = exported.get(name) {
                        result.insert(name.clone(), key.clone());
                    }
                }
            }
            crate::ast::ImportType::Unqualified | crate::ast::ImportType::Qualified(None) => {
                result.extend(exported);
            }
        }
    }
    result
}

fn exported_trait_keys(
    path: &Path,
    project_root: &Path,
    base_root: &Path,
    modules: &BTreeMap<PathBuf, Module>,
    names: &BTreeMap<PathBuf, String>,
    seen: &mut BTreeSet<PathBuf>,
) -> BTreeMap<String, String> {
    if !seen.insert(path.to_owned()) {
        return BTreeMap::new();
    }
    let Some(module) = modules.get(path) else {
        return BTreeMap::new();
    };
    let mut result = module
        .declarations
        .iter()
        .filter_map(|declaration| match &declaration.kind {
            crate::ast::DeclarationKind::Trait { name, .. } if declaration.exported => {
                Some((name.clone(), format!("{}::{name}", names[path])))
            }
            _ => None,
        })
        .collect::<BTreeMap<_, _>>();
    for declaration in &module.declarations {
        if let crate::ast::DeclarationKind::ExportImport(name) = &declaration.kind {
            if let Some(import) = module
                .imports
                .iter()
                .find(|import| import.module.last() == Some(name))
            {
                result.extend(exported_trait_keys(
                    &resolve_import(import, project_root, base_root),
                    project_root,
                    base_root,
                    modules,
                    names,
                    seen,
                ));
            }
        }
    }
    result
}

fn resolve_import(import: &crate::ast::Import, project_root: &Path, base_root: &Path) -> PathBuf {
    let relative = import
        .module
        .iter()
        .collect::<PathBuf>()
        .with_extension("cx");
    let project_path = project_root.join(&relative);
    if project_path.is_file() {
        project_path
    } else {
        base_root.join(relative)
    }
}

fn exported_bindings(module: &Module) -> Vec<(String, String)> {
    let mut result = Vec::new();
    for declaration in &module.declarations {
        if !declaration.exported {
            continue;
        }
        match &declaration.kind {
            crate::ast::DeclarationKind::Function { name, .. }
            | crate::ast::DeclarationKind::Declare { name, .. } => {
                result.push((name.clone(), js_identifier(name)))
            }
            crate::ast::DeclarationKind::Exception { name, .. } => {
                result.push((format!("{name}$"), format!("{}$", js_identifier(name))))
            }
            crate::ast::DeclarationKind::Let { pattern, .. } => {
                pattern_bindings(pattern, &mut result)
            }
            crate::ast::DeclarationKind::Data { variants, .. } => {
                for variant in variants {
                    result.push((variant.name.clone(), js_identifier(&variant.name)));
                }
            }
            crate::ast::DeclarationKind::JsData { variants, .. } => {
                for variant in variants {
                    result.push((variant.name.clone(), js_identifier(&variant.name)));
                }
            }
            crate::ast::DeclarationKind::Trait { methods, .. } => {
                for method in methods {
                    result.push((method.name.clone(), js_identifier(&method.name)));
                }
            }
            _ => {}
        }
    }
    result
}

fn exported_names(
    path: &Path,
    project_root: &Path,
    base_root: &Path,
    modules: &BTreeMap<PathBuf, Module>,
    seen: &mut BTreeSet<PathBuf>,
) -> BTreeSet<String> {
    if !seen.insert(path.to_owned()) {
        return BTreeSet::new();
    }
    let Some(module) = modules.get(path) else {
        return BTreeSet::new();
    };
    let mut result = exported_bindings(module)
        .into_iter()
        .map(|(name, _)| name)
        .collect::<BTreeSet<_>>();
    for declaration in &module.declarations {
        if let crate::ast::DeclarationKind::ExportImport(name) = &declaration.kind {
            if let Some(import) = module
                .imports
                .iter()
                .find(|import| import.module.last() == Some(name))
            {
                result.extend(exported_names(
                    &resolve_import(import, project_root, base_root),
                    project_root,
                    base_root,
                    modules,
                    seen,
                ));
            }
        }
    }
    result
}

fn pattern_bindings(pattern: &crate::ast::Pattern, result: &mut Vec<(String, String)>) {
    match pattern {
        crate::ast::Pattern::Binding(name) => result.push((name.clone(), js_identifier(name))),
        crate::ast::Pattern::Constructor(crate::ast::Reference::Unqualified(name), values)
            if values.is_empty() =>
        {
            result.push((name.clone(), js_identifier(name)))
        }
        crate::ast::Pattern::Tuple(values) | crate::ast::Pattern::Constructor(_, values) => {
            for value in values {
                pattern_bindings(value, result);
            }
        }
        crate::ast::Pattern::Wildcard => {}
    }
}

fn js_identifier(name: &str) -> String {
    const KEYWORDS: &[&str] = &[
        "break", "case", "catch", "class", "const", "continue", "default", "delete", "do", "else",
        "export", "for", "function", "if", "import", "in", "let", "new", "return", "switch",
        "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield",
    ];
    if KEYWORDS.contains(&name) {
        format!("{name}$")
    } else {
        name.into()
    }
}

fn js_string(value: &str) -> String {
    format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""))
}

fn load_module_graph(
    path: &Path,
    project_root: &Path,
    base_root: &Path,
    visiting: &mut Vec<PathBuf>,
    complete: &mut BTreeSet<PathBuf>,
    modules: &mut BTreeMap<PathBuf, Module>,
) -> Result<(), CompileError> {
    let path = path.to_owned();
    if complete.contains(&path) {
        return Ok(());
    }
    if let Some(index) = visiting.iter().position(|candidate| candidate == &path) {
        let mut cycle = visiting[index..].to_vec();
        cycle.push(path);
        return Err(CompileError::CircularImport(cycle));
    }
    let source = std::fs::read_to_string(&path).map_err(|error| CompileError::Io {
        path: path.clone(),
        message: error.to_string(),
    })?;
    let parsed = chumsky_parser::parse_module(&path.display().to_string(), &source);
    if !parsed.diagnostics.is_empty() {
        return Err(CompileError::Parse(parsed.diagnostics));
    }
    let module = parsed.output.ok_or_else(|| CompileError::Parse(vec![]))?;
    visiting.push(path.clone());
    for import in &module.imports {
        let relative = import
            .module
            .iter()
            .collect::<PathBuf>()
            .with_extension("cx");
        let project_path = project_root.join(&relative);
        let dependency = if project_path.is_file() {
            project_path
        } else {
            base_root.join(relative)
        };
        load_module_graph(
            &dependency,
            project_root,
            base_root,
            visiting,
            complete,
            modules,
        )?;
    }
    visiting.pop();
    complete.insert(path.clone());
    modules.insert(path, module);
    Ok(())
}

impl std::error::Error for CompileError {}

pub fn compile(file: &str, source: &str) -> Result<String, CompileError> {
    let parsed = chumsky_parser::parse_module(file, source);
    if !parsed.diagnostics.is_empty() {
        return Err(CompileError::Parse(parsed.diagnostics));
    }
    let module = parsed.output.ok_or_else(|| CompileError::Parse(vec![]))?;
    let checked = typecheck::check(&module).map_err(CompileError::Type)?;
    codegen::generate(&checked).map_err(CompileError::Codegen)
}

/// Compile a source module as a self-running JavaScript program.
pub fn compile_executable(file: &str, source: &str) -> Result<String, CompileError> {
    let body = compile(file, source)?;
    Ok(format!(
        "{}\n{}\n{body}\nmain();\n",
        include_str!("../../rts/rts.js"),
        runtime_prelude()
    ))
}

fn runtime_prelude() -> &'static str {
    r#""use strict";
var True = true;
var False = false;
var None = ["None"];
function Some(value) { return ["Some", value]; }
function Ok(value) { return ["Ok", value]; }
function Err(value) { return ["Err", value]; }
var _rts_traits = Object.create(null);
var _rts_trait_defaults = Object.create(null);
function _rts_type_key(value) {
  if (Array.isArray(value)) return typeof value[0] === "string" ? value[0] : "Array";
  if (value === null || value === void 0) return "Void";
  if (typeof value === "number") return "Number";
  if (typeof value === "string") return "String";
  if (typeof value === "boolean") return "Boolean";
  if (typeof value === "function") return "Function/" + value.length;
  return "Record";
}
function _rts_register_trait(traitName, key, methods) {
  (_rts_traits[traitName] || (_rts_traits[traitName] = Object.create(null)))[key] = methods;
}
function _rts_new_trait_value(traitName, methodName) {
  return {_rts_trait_value: true, traitName: traitName, methodName: methodName, values: Object.create(null)};
}
function _rts_set_trait_value(value, key, implementation) { value.values[key] = implementation; }
function _rts_resolve_trait_value(value, key) {
  return value && value._rts_trait_value ? value.values[key] : value;
}
function _rts_trait_call(traitName, methodName, args) {
  var table = _rts_traits[traitName] || Object.create(null);
  var key;
  for (var i = 0; i < args.length; ++i) if (!args[i] || !args[i]._rts_trait_value) { key = _rts_type_key(args[i]); break; }
  var implementation = table[key] || table.Record;
  if (!implementation) {
    var keys = Object.keys(table);
    if (keys.length === 1) { key = keys[0]; implementation = table[key]; }
  }
  implementation = implementation || Object.create(null);
  var method = implementation[methodName] || (_rts_trait_defaults[traitName] || Object.create(null))[methodName];
  if (!method) throw new TypeError("missing " + traitName + "." + methodName + " implementation");
  return method.apply(null, Array.prototype.map.call(args, function(arg) {
    return arg && arg._rts_trait_value ? arg.values[key] : arg;
  }));
}
function toString(value) {
  var implementation = (_rts_traits.ToString || Object.create(null))[_rts_type_key(value)];
  return implementation && implementation.toString ? implementation.toString(value) : String(value);
}
function print(value) { console.log(value === void 0 ? "()" : toString(value)); }
function range(length) { return length <= 0 ? [] : Array.from({length: length}, function (_, i) { return i; }); }
function replicate(value, length) { return Array.from({length: length}, function () { return value; }); }
function each(array, callback) { array.forEach(callback); }
function append(array, value) { array.push(value); }
function freeze(array) { return array; }
function endsWith(value, suffix) { return value.endsWith(suffix); }
function startsWith(value, prefix) { return value.startsWith(prefix); }
function sorted(array) { return array.slice().sort(); }
function maybe(option, callback, fallback) {
  if (option === void 0 || option === null || (Array.isArray(option) && option[0] === "None")) return fallback;
  return callback(Array.isArray(option) && option[0] === "Some" ? option[1] : option);
}
function _unsafe_coerce(value) { return value; }
"#
}
