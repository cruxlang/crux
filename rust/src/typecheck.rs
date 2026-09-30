//! Hindley-Milner type inference for the source AST.
//!
//! Types live in an arena and are referred to by small indices.  Keeping the
//! mutable graph out of the Rust call stack also makes it possible to put hard
//! limits on all graph walks.

use crate::ast::*;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

pub type TypeId = usize;

const MAX_TYPE_NODES: usize = 1_000_000;
const MAX_TYPE_WALK: usize = 2_000_000;
const MAX_UNIFY_DEPTH: usize = 256;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var,
    Rigid(Name),
    IntegerLiteral,
    Link(TypeId),
    Named(Name, Vec<TypeId>),
    Function(Vec<TypeId>, TypeId),
    Record(BTreeMap<Name, FieldType>, Option<TypeId>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldType {
    pub mutability: Option<Mutability>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub quantified: Vec<TypeId>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
    pub pos: Pos,
    pub kind: TypeErrorKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeErrorKind {
    UnboundValue(Name),
    UnboundType(Name),
    DuplicateBinding(Name),
    CannotUnify(String, String),
    OccursCheck(String, String),
    Arity { expected: usize, actual: usize },
    ImmutableAssignment,
    InvalidAssignmentTarget,
    ReturnOutsideFunction,
    BreakOutsideLoop,
    ResourceLimit(&'static str),
    Unsupported(&'static str),
    TraitError(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: ",
            self.pos.file, self.pos.line, self.pos.column
        )?;
        match &self.kind {
            TypeErrorKind::UnboundValue(n) => write!(f, "unbound value {n}"),
            TypeErrorKind::UnboundType(n) => write!(f, "unbound type {n}"),
            TypeErrorKind::DuplicateBinding(n) => write!(f, "duplicate binding {n}"),
            TypeErrorKind::CannotUnify(a, b) => write!(f, "cannot unify {a} with {b}"),
            TypeErrorKind::OccursCheck(a, b) => write!(f, "infinite type: {a} occurs in {b}"),
            TypeErrorKind::Arity { expected, actual } => {
                write!(f, "expected {expected} arguments, found {actual}")
            }
            TypeErrorKind::ImmutableAssignment => write!(f, "cannot assign to an immutable value"),
            TypeErrorKind::InvalidAssignmentTarget => write!(f, "invalid assignment target"),
            TypeErrorKind::ReturnOutsideFunction => write!(f, "return outside a function"),
            TypeErrorKind::BreakOutsideLoop => write!(f, "break outside a loop"),
            TypeErrorKind::ResourceLimit(s) => {
                write!(f, "type-checking resource limit exceeded: {s}")
            }
            TypeErrorKind::Unsupported(s) => write!(f, "unsupported language feature: {s}"),
            TypeErrorKind::TraitError(s) => write!(f, "trait error: {s}"),
        }
    }
}

impl std::error::Error for TypeError {}

impl TypeError {
    /// Stable diagnostic category used by integration fixtures and tooling.
    pub fn name(&self) -> &'static str {
        match &self.kind {
            TypeErrorKind::UnboundValue(_) => "unbound-value",
            TypeErrorKind::UnboundType(_) => "unbound-type",
            TypeErrorKind::DuplicateBinding(_) => "duplicate-symbol",
            TypeErrorKind::CannotUnify(left, right)
                if left.contains("mutable field") || right.contains("mutable field") =>
            {
                "record-mutability-unification"
            }
            TypeErrorKind::CannotUnify(_, _) => "unification",
            TypeErrorKind::OccursCheck(_, _) => "occurs-check",
            TypeErrorKind::Arity {
                expected: 0,
                actual,
            } if *actual > 0 => "illegal-type-application",
            TypeErrorKind::Arity {
                expected,
                actual: 0,
            } if *expected > 0 => "type-application-mismatch",
            TypeErrorKind::Arity { .. } => "unification",
            TypeErrorKind::ImmutableAssignment | TypeErrorKind::InvalidAssignmentTarget => {
                "immutable-assignment"
            }
            TypeErrorKind::ReturnOutsideFunction => "return-outside-function",
            TypeErrorKind::BreakOutsideLoop => "break-outside-loop",
            TypeErrorKind::ResourceLimit(_) => "resource-limit",
            TypeErrorKind::Unsupported(message) if message.contains("intrinsic") => "intrinsic",
            TypeErrorKind::Unsupported(_) => "unsupported",
            TypeErrorKind::TraitError(message) if message == "ambiguous polymorphism" => {
                "ambiguous-polymorphism"
            }
            TypeErrorKind::TraitError(message) if message.starts_with("unexpected impl method") => {
                "unexpected-impl-method"
            }
            TypeErrorKind::TraitError(message) if message.starts_with("missing impl method") => {
                "incomplete-impl"
            }
            TypeErrorKind::TraitError(_) => "no-trait-on-type",
        }
    }
}

#[derive(Clone, Debug)]
struct Binding {
    scheme: Scheme,
    mutability: Mutability,
}

#[derive(Clone, Debug)]
struct RecordRequirement {
    fields: BTreeMap<Name, TypeId>,
    /// When present, every field not explicitly listed has this type.
    rest: Option<TypeId>,
}

#[derive(Clone, Debug)]
struct TraitMethodSpec {
    scheme: Scheme,
    self_type: TypeId,
    has_default: bool,
}

#[derive(Clone, Debug, Default)]
struct TraitSpec {
    methods: BTreeMap<Name, TraitMethodSpec>,
}

#[derive(Clone, Debug)]
pub struct CheckedModule {
    pub module: Module,
    pub declarations: Vec<Option<TypeId>>,
    pub types: Vec<Type>,
}

pub fn check(module: &Module) -> Result<CheckedModule, TypeError> {
    let mut checker = Checker::new();
    if !module.pragmas.contains(&Pragma::NoBuiltin) {
        checker.install_prelude()?;
    }
    checker.module(module)
}

struct Checker {
    types: Vec<Type>,
    values: Vec<BTreeMap<Name, Binding>>,
    external_values: BTreeMap<Name, Binding>,
    allow_external_unqualified: bool,
    module_aliases: BTreeSet<Name>,
    named_types: BTreeMap<Name, usize>,
    aliases: BTreeMap<Name, (Vec<Name>, TypeIdent)>,
    resolving_aliases: Vec<Name>,
    return_type: Option<TypeId>,
    loop_depth: usize,
    unify_depth: usize,
    expression_depth: usize,
    weak_vars: BTreeSet<TypeId>,
    record_requirements: BTreeMap<TypeId, RecordRequirement>,
    trait_requirements: BTreeMap<TypeId, BTreeSet<Name>>,
    traits: BTreeMap<Name, TraitSpec>,
    trait_instances: BTreeMap<Name, BTreeSet<Name>>,
    record_trait_instances: BTreeMap<Name, Scheme>,
    function_trait_instances: BTreeMap<Name, BTreeSet<usize>>,
    allow_external_traits: bool,
}

impl Checker {
    fn new() -> Self {
        let mut this = Self {
            types: Vec::new(),
            values: vec![BTreeMap::new()],
            external_values: BTreeMap::new(),
            allow_external_unqualified: false,
            module_aliases: BTreeSet::new(),
            named_types: BTreeMap::new(),
            aliases: BTreeMap::new(),
            resolving_aliases: Vec::new(),
            return_type: None,
            loop_depth: 0,
            unify_depth: 0,
            expression_depth: 0,
            weak_vars: BTreeSet::new(),
            record_requirements: BTreeMap::new(),
            trait_requirements: BTreeMap::new(),
            traits: BTreeMap::new(),
            trait_instances: BTreeMap::new(),
            record_trait_instances: BTreeMap::new(),
            function_trait_instances: BTreeMap::new(),
            allow_external_traits: false,
        };
        for (name, arity) in [
            ("Int", 0),
            ("Number", 0),
            ("String", 0),
            ("Unit", 0),
            ("Void", 0),
            ("Boolean", 0),
            ("Array", 1),
            ("MutableArray", 1),
            ("Option", 1),
            ("JSOption", 1),
        ] {
            this.named_types.insert(name.into(), arity);
        }
        let bool_ty = this
            .named("Boolean", vec![], &Pos::new("<builtin>", 1, 1))
            .unwrap();
        this.bind_mono("True", bool_ty, Mutability::Immutable);
        this.bind_mono("False", bool_ty, Mutability::Immutable);
        this
    }

    fn install_prelude(&mut self) -> Result<(), TypeError> {
        let pos = Pos::new("<prelude>", 1, 1);
        for (name, arity) in [("Result", 2), ("Tuple0", 0)] {
            self.named_types.insert(name.into(), arity);
        }

        let a = self.fresh(&pos)?;
        let unit = self.named("Unit", vec![], &pos)?;
        let print = self.alloc(Type::Function(vec![a], unit), &pos)?;
        self.values[0].insert(
            "print".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a],
                    ty: print,
                },
                mutability: Mutability::Immutable,
            },
        );

        let a = self.fresh(&pos)?;
        let string = self.named("String", vec![], &pos)?;
        let to_string = self.alloc(Type::Function(vec![a], string), &pos)?;
        self.values[0].insert(
            "toString".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a],
                    ty: to_string,
                },
                mutability: Mutability::Immutable,
            },
        );

        let a = self.fresh(&pos)?;
        let option = self.named("Option", vec![a], &pos)?;
        let some = self.alloc(Type::Function(vec![a], option), &pos)?;
        self.values[0].insert(
            "Some".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a],
                    ty: some,
                },
                mutability: Mutability::Immutable,
            },
        );
        self.values[0].insert(
            "None".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a],
                    ty: option,
                },
                mutability: Mutability::Immutable,
            },
        );

        let a = self.fresh(&pos)?;
        let b = self.fresh(&pos)?;
        let result = self.named("Result", vec![a, b], &pos)?;
        let ok = self.alloc(Type::Function(vec![a], result), &pos)?;
        let err = self.alloc(Type::Function(vec![b], result), &pos)?;
        self.values[0].insert(
            "Ok".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a, b],
                    ty: ok,
                },
                mutability: Mutability::Immutable,
            },
        );
        self.values[0].insert(
            "Err".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a, b],
                    ty: err,
                },
                mutability: Mutability::Immutable,
            },
        );

        let number = self.named("Number", vec![], &pos)?;
        let array_number = self.named("Array", vec![number], &pos)?;
        let range = self.alloc(Type::Function(vec![number], array_number), &pos)?;
        self.bind_mono("range", range, Mutability::Immutable);

        let a = self.fresh(&pos)?;
        let array = self.named("Array", vec![a], &pos)?;
        let replicate = self.alloc(Type::Function(vec![a, number], array), &pos)?;
        self.values[0].insert(
            "replicate".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a],
                    ty: replicate,
                },
                mutability: Mutability::Immutable,
            },
        );

        let a = self.fresh(&pos)?;
        let b = self.fresh(&pos)?;
        let array = self.named("Array", vec![a], &pos)?;
        let callback = self.alloc(Type::Function(vec![a], b), &pos)?;
        let each = self.alloc(Type::Function(vec![array, callback], unit), &pos)?;
        self.values[0].insert(
            "each".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a, b],
                    ty: each,
                },
                mutability: Mutability::Immutable,
            },
        );

        let a = self.fresh(&pos)?;
        let array = self.named("Array", vec![a], &pos)?;
        let sorted = self.alloc(Type::Function(vec![array], array), &pos)?;
        self.values[0].insert(
            "sorted".into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![a],
                    ty: sorted,
                },
                mutability: Mutability::Immutable,
            },
        );
        Ok(())
    }

    fn err<T>(&self, pos: &Pos, kind: TypeErrorKind) -> Result<T, TypeError> {
        Err(TypeError {
            pos: pos.clone(),
            kind,
        })
    }

    fn alloc(&mut self, ty: Type, pos: &Pos) -> Result<TypeId, TypeError> {
        if self.types.len() >= MAX_TYPE_NODES {
            return self.err(pos, TypeErrorKind::ResourceLimit("type nodes"));
        }
        let id = self.types.len();
        self.types.push(ty);
        Ok(id)
    }

    fn fresh(&mut self, pos: &Pos) -> Result<TypeId, TypeError> {
        self.alloc(Type::Var, pos)
    }

    fn rigid(&mut self, name: &str, pos: &Pos) -> Result<TypeId, TypeError> {
        self.alloc(Type::Rigid(name.into()), pos)
    }

    fn named(&mut self, name: &str, args: Vec<TypeId>, pos: &Pos) -> Result<TypeId, TypeError> {
        self.alloc(Type::Named(name.into(), args), pos)
    }

    fn root(&mut self, mut id: TypeId, pos: &Pos) -> Result<TypeId, TypeError> {
        let mut path = Vec::new();
        for _ in 0..MAX_TYPE_WALK {
            match self.types.get(id) {
                Some(Type::Link(next)) => {
                    path.push(id);
                    id = *next;
                }
                Some(_) => {
                    for p in path {
                        self.types[p] = Type::Link(id);
                    }
                    return Ok(id);
                }
                None => {
                    return self.err(pos, TypeErrorKind::ResourceLimit("invalid type reference"))
                }
            }
        }
        self.err(pos, TypeErrorKind::ResourceLimit("type graph walk"))
    }

    fn occurs(&mut self, needle: TypeId, haystack: TypeId, pos: &Pos) -> Result<bool, TypeError> {
        let needle = self.root(needle, pos)?;
        let mut todo = vec![haystack];
        let mut seen = BTreeSet::new();
        for _ in 0..MAX_TYPE_WALK {
            let Some(candidate) = todo.pop() else {
                return Ok(false);
            };
            let candidate = self.root(candidate, pos)?;
            if candidate == needle {
                return Ok(true);
            }
            if !seen.insert(candidate) {
                continue;
            }
            match self.types[candidate].clone() {
                Type::Var | Type::Rigid(_) | Type::IntegerLiteral | Type::Link(_) => {}
                Type::Named(_, xs) | Type::Function(xs, _) => {
                    todo.extend(xs);
                    if let Type::Function(_, result) = self.types[candidate] {
                        todo.push(result);
                    }
                }
                Type::Record(fields, rest) => {
                    todo.extend(fields.values().map(|f| f.ty));
                    todo.extend(rest);
                }
            }
        }
        self.err(pos, TypeErrorKind::ResourceLimit("occurs check"))
    }

    fn unify(&mut self, a: TypeId, b: TypeId, pos: &Pos) -> Result<(), TypeError> {
        if self.unify_depth >= MAX_UNIFY_DEPTH {
            return self.err(pos, TypeErrorKind::ResourceLimit("unification depth"));
        }
        self.unify_depth += 1;
        let result = self.unify_inner(a, b, pos);
        self.unify_depth -= 1;
        result
    }

    fn unify_inner(&mut self, a: TypeId, b: TypeId, pos: &Pos) -> Result<(), TypeError> {
        let a = self.root(a, pos)?;
        let b = self.root(b, pos)?;
        if a == b {
            return Ok(());
        }
        let at = self.types[a].clone();
        let bt = self.types[b].clone();
        match (at, bt) {
            (Type::Var, _) => self.bind_var(a, b, pos),
            (_, Type::Var) => self.bind_var(b, a, pos),
            (Type::Rigid(_), Type::Record(_, Some(_))) => {
                if let Some(requirement) = self.record_requirements.get(&a).cloned() {
                    self.validate_record_requirement(&requirement, b, pos)
                } else {
                    let left = self.render(a, pos)?;
                    let right = self.render(b, pos)?;
                    self.err(pos, TypeErrorKind::CannotUnify(left, right))
                }
            }
            (Type::Record(_, Some(_)), Type::Rigid(_)) => {
                if let Some(requirement) = self.record_requirements.get(&b).cloned() {
                    self.validate_record_requirement(&requirement, a, pos)
                } else {
                    let left = self.render(a, pos)?;
                    let right = self.render(b, pos)?;
                    self.err(pos, TypeErrorKind::CannotUnify(left, right))
                }
            }
            (Type::IntegerLiteral, Type::IntegerLiteral) => {
                self.types[a] = Type::Link(b);
                Ok(())
            }
            (Type::IntegerLiteral, Type::Named(name, _)) if is_integer_literal_type(&name) => {
                self.types[a] = Type::Link(b);
                Ok(())
            }
            (Type::Named(name, _), Type::IntegerLiteral) if is_integer_literal_type(&name) => {
                self.types[b] = Type::Link(a);
                Ok(())
            }
            (Type::Named(an, aa), Type::Named(bn, ba)) if an == bn && aa.len() == ba.len() => {
                for (x, y) in aa.into_iter().zip(ba) {
                    self.unify(x, y, pos)?;
                }
                Ok(())
            }
            (Type::Function(aa, ar), Type::Function(ba, br)) if aa.len() == ba.len() => {
                for (x, y) in aa.into_iter().zip(ba) {
                    self.unify(x, y, pos)?;
                }
                self.unify(ar, br, pos)
            }
            (Type::Record(af, ar), Type::Record(bf, br)) => {
                for name in af.keys().filter(|name| bf.contains_key(*name)) {
                    match (af[name].mutability, bf[name].mutability) {
                        (None, Some(mutability)) => {
                            if let Type::Record(fields, _) = &mut self.types[a] {
                                fields.get_mut(name).unwrap().mutability = Some(mutability);
                            }
                        }
                        (Some(mutability), None) => {
                            if let Type::Record(fields, _) = &mut self.types[b] {
                                fields.get_mut(name).unwrap().mutability = Some(mutability);
                            }
                        }
                        _ => {}
                    }
                }
                self.unify_records(af, ar, bf, br, pos)
            }
            _ => {
                let left = self.render(a, pos)?;
                let right = self.render(b, pos)?;
                self.err(pos, TypeErrorKind::CannotUnify(left, right))
            }
        }
    }

    fn bind_var(&mut self, var: TypeId, value: TypeId, pos: &Pos) -> Result<(), TypeError> {
        if self.occurs(var, value, pos)? {
            let a = self.render(var, pos)?;
            let b = self.render(value, pos)?;
            return self.err(pos, TypeErrorKind::OccursCheck(a, b));
        }
        if let Some(requirement) = self.record_requirements.remove(&var) {
            self.validate_record_requirement(&requirement, value, pos)?;
            let value = self.root(value, pos)?;
            if matches!(self.types[value], Type::Var) {
                self.merge_record_requirement(value, requirement, pos)?;
            }
        }
        if let Some(required_traits) = self.trait_requirements.remove(&var) {
            let value_root = self.root(value, pos)?;
            if matches!(self.types[value_root], Type::Var) {
                let existing = self
                    .trait_requirements
                    .get(&value_root)
                    .cloned()
                    .unwrap_or_default();
                if !existing.is_empty()
                    && required_traits.iter().any(|name| !existing.contains(name))
                {
                    return self.err(
                        pos,
                        TypeErrorKind::TraitError("ambiguous polymorphism".into()),
                    );
                }
            }
            self.validate_trait_requirements(&required_traits, value, pos)?;
            let value = self.root(value, pos)?;
            if matches!(self.types[value], Type::Var | Type::Rigid(_)) {
                self.trait_requirements
                    .entry(value)
                    .or_default()
                    .extend(required_traits);
            }
        }
        self.types[var] = Type::Link(value);
        Ok(())
    }

    fn validate_trait_requirements(
        &mut self,
        required: &BTreeSet<Name>,
        value: TypeId,
        pos: &Pos,
    ) -> Result<(), TypeError> {
        let value = self.root(value, pos)?;
        match self.types[value].clone() {
            Type::Var => Ok(()),
            Type::Rigid(_) => {
                let available = self
                    .trait_requirements
                    .get(&value)
                    .cloned()
                    .unwrap_or_default();
                if let Some(missing) = required.iter().find(|name| !available.contains(*name)) {
                    return self.err(
                        pos,
                        TypeErrorKind::TraitError(format!(
                            "type variable does not have trait {missing}"
                        )),
                    );
                }
                Ok(())
            }
            Type::Named(name, _) => {
                if let Some(missing) = required.iter().find(|trait_name| {
                    self.traits.contains_key(*trait_name)
                        && !self
                            .trait_instances
                            .get(*trait_name)
                            .is_some_and(|types| types.contains(&name))
                }) {
                    return self.err(
                        pos,
                        TypeErrorKind::TraitError(format!("{name} does not implement {missing}")),
                    );
                }
                Ok(())
            }
            Type::Function(args, _) => {
                if let Some(missing) = required.iter().find(|trait_name| {
                    self.traits.contains_key(*trait_name)
                        && !self
                            .function_trait_instances
                            .get(*trait_name)
                            .is_some_and(|arities| arities.contains(&args.len()))
                }) {
                    return self.err(
                        pos,
                        TypeErrorKind::TraitError(format!("function does not implement {missing}")),
                    );
                }
                Ok(())
            }
            Type::Record(fields, _) => {
                for trait_name in required {
                    let Some(scheme) = self.record_trait_instances.get(trait_name).cloned() else {
                        return self.err(
                            pos,
                            TypeErrorKind::TraitError(format!(
                                "record does not implement {trait_name}"
                            )),
                        );
                    };
                    let shared_result = self.fresh(pos)?;
                    for field in fields.values() {
                        let transformer = self.instantiate(&scheme, pos)?;
                        let expected =
                            self.alloc(Type::Function(vec![field.ty], shared_result), pos)?;
                        self.unify(transformer, expected, pos)?;
                    }
                }
                Ok(())
            }
            _ => self.err(
                pos,
                TypeErrorKind::TraitError("trait used with an unsupported type".into()),
            ),
        }
    }

    fn merge_record_requirement(
        &mut self,
        id: TypeId,
        requirement: RecordRequirement,
        pos: &Pos,
    ) -> Result<(), TypeError> {
        if let Some(mut existing) = self.record_requirements.remove(&id) {
            for (name, ty) in requirement.fields {
                if let Some(other) = existing.fields.get(&name).copied() {
                    self.unify(ty, other, pos)?;
                } else {
                    existing.fields.insert(name, ty);
                }
            }
            match (existing.rest, requirement.rest) {
                (Some(a), Some(b)) => {
                    self.unify(a, b, pos)?;
                    existing.rest = Some(a);
                }
                (None, Some(b)) => existing.rest = Some(b),
                _ => {}
            }
            self.record_requirements.insert(id, existing);
        } else {
            self.record_requirements.insert(id, requirement);
        }
        Ok(())
    }

    fn validate_record_requirement(
        &mut self,
        requirement: &RecordRequirement,
        value: TypeId,
        pos: &Pos,
    ) -> Result<(), TypeError> {
        let value = self.root(value, pos)?;
        match self.types[value].clone() {
            Type::Var => Ok(()),
            Type::Record(fields, tail) => {
                for (name, required) in &requirement.fields {
                    if let Some(actual) = fields.get(name) {
                        self.unify(*required, actual.ty, pos)?;
                    } else if let Some(tail) = tail {
                        let open_tail = self.fresh(pos)?;
                        let row = self.alloc(
                            Type::Record(
                                BTreeMap::from([(
                                    name.clone(),
                                    FieldType {
                                        mutability: None,
                                        ty: *required,
                                    },
                                )]),
                                Some(open_tail),
                            ),
                            pos,
                        )?;
                        self.unify(tail, row, pos)?;
                    } else {
                        self.require_optional_field(*required, pos)?;
                    }
                }
                if let Some(rest) = requirement.rest {
                    for (name, field) in fields {
                        if !requirement.fields.contains_key(&name) {
                            self.unify(rest, field.ty, pos)?;
                        }
                    }
                }
                Ok(())
            }
            _ => {
                let actual = self.render(value, pos)?;
                self.err(
                    pos,
                    TypeErrorKind::CannotUnify("record constraint".into(), actual),
                )
            }
        }
    }

    fn unify_records(
        &mut self,
        af: BTreeMap<Name, FieldType>,
        ar: Option<TypeId>,
        bf: BTreeMap<Name, FieldType>,
        br: Option<TypeId>,
        pos: &Pos,
    ) -> Result<(), TypeError> {
        for (name, a) in &af {
            if let Some(b) = bf.get(name) {
                if matches!(
                    (a.mutability, b.mutability),
                    (Some(Mutability::Mutable), Some(Mutability::Immutable))
                        | (Some(Mutability::Immutable), Some(Mutability::Mutable))
                ) {
                    return self.err(
                        pos,
                        TypeErrorKind::CannotUnify(
                            format!("mutable field {name}"),
                            format!("immutable field {name}"),
                        ),
                    );
                }
                self.unify(a.ty, b.ty, pos)?;
            }
        }
        let a_only = af
            .iter()
            .filter(|(n, _)| !bf.contains_key(*n))
            .map(|(n, f)| (n.clone(), f.clone()))
            .collect::<BTreeMap<_, _>>();
        let b_only = bf
            .iter()
            .filter(|(n, _)| !af.contains_key(*n))
            .map(|(n, f)| (n.clone(), f.clone()))
            .collect::<BTreeMap<_, _>>();
        match (ar, br) {
            (None, None) if !a_only.is_empty() || !b_only.is_empty() => self.err(
                pos,
                TypeErrorKind::CannotUnify(
                    "record fields".into(),
                    "different record fields".into(),
                ),
            ),
            (Some(a_rest), Some(b_rest)) => {
                let shared = self.fresh(pos)?;
                let a_tail = self.alloc(Type::Record(b_only, Some(shared)), pos)?;
                let b_tail = self.alloc(Type::Record(a_only, Some(shared)), pos)?;
                self.unify(a_rest, a_tail, pos)?;
                self.unify(b_rest, b_tail, pos)
            }
            (Some(a_rest), None) => {
                for field in a_only.values() {
                    self.require_optional_field(field.ty, pos)?;
                }
                let tail = self.alloc(Type::Record(b_only, None), pos)?;
                self.unify(a_rest, tail, pos)
            }
            (None, Some(b_rest)) => {
                for field in b_only.values() {
                    self.require_optional_field(field.ty, pos)?;
                }
                let tail = self.alloc(Type::Record(a_only, None), pos)?;
                self.unify(b_rest, tail, pos)
            }
            (None, None) => Ok(()),
        }
    }

    fn require_optional_field(&mut self, ty: TypeId, pos: &Pos) -> Result<(), TypeError> {
        let ty = self.root(ty, pos)?;
        match self.types[ty].clone() {
            Type::Var => {
                let element = self.fresh(pos)?;
                let option = self.named("Option", vec![element], pos)?;
                self.bind_var(ty, option, pos)
            }
            Type::Named(name, args)
                if (name == "Option" || name == "JSOption") && args.len() == 1 =>
            {
                Ok(())
            }
            _ => {
                let actual = self.render(ty, pos)?;
                self.err(
                    pos,
                    TypeErrorKind::CannotUnify(actual, "optional field".into()),
                )
            }
        }
    }

    fn bind_mono(&mut self, name: &str, ty: TypeId, mutability: Mutability) {
        self.values.last_mut().unwrap().insert(
            name.into(),
            Binding {
                scheme: Scheme {
                    quantified: vec![],
                    ty,
                },
                mutability,
            },
        );
    }

    fn lookup(&mut self, name: &str, pos: &Pos) -> Result<Binding, TypeError> {
        let binding = self
            .values
            .iter()
            .rev()
            .find_map(|s| s.get(name))
            .cloned()
            .or_else(|| self.external_values.get(name).cloned());
        match binding {
            Some(mut b) => {
                b.scheme.ty = self.instantiate(&b.scheme, pos)?;
                Ok(b)
            }
            None if name == "_unsafe_js" || name == "_unsafe_coerce" => {
                let a = self.fresh(pos)?;
                let b = self.fresh(pos)?;
                let string = self.named("String", vec![], pos)?;
                let args = if name == "_unsafe_js" {
                    vec![string]
                } else {
                    vec![a]
                };
                let ty = self.alloc(Type::Function(args, b), pos)?;
                Ok(Binding {
                    scheme: Scheme {
                        quantified: vec![],
                        ty,
                    },
                    mutability: Mutability::Immutable,
                })
            }
            None if self.allow_external_unqualified => self.external(name, pos),
            None => self.err(pos, TypeErrorKind::UnboundValue(name.into())),
        }
    }

    fn external(&mut self, name: &str, pos: &Pos) -> Result<Binding, TypeError> {
        if let Some(binding) = self.external_values.get(name).cloned() {
            return Ok(binding);
        }
        let ty = self.fresh(pos)?;
        let binding = Binding {
            scheme: Scheme {
                quantified: vec![],
                ty,
            },
            mutability: Mutability::Immutable,
        };
        self.external_values.insert(name.into(), binding.clone());
        Ok(binding)
    }

    fn lookup_reference(&mut self, reference: &Reference, pos: &Pos) -> Result<Binding, TypeError> {
        match reference {
            Reference::Unqualified(name) => self.lookup(name, pos),
            Reference::Qualified(module, name) => self.external(&format!("{module}.{name}"), pos),
        }
    }

    fn instantiate(&mut self, scheme: &Scheme, pos: &Pos) -> Result<TypeId, TypeError> {
        // Monomorphic bindings share their inference variable. Copying them is
        // both unnecessary and incorrect: row unification can make their type
        // graph recursive, and a copy would turn that graph into a fresh
        // self-referential type on every lookup.
        if scheme.quantified.is_empty() {
            return Ok(scheme.ty);
        }
        let mut replacements = BTreeMap::new();
        for id in &scheme.quantified {
            replacements.insert(*id, self.fresh(pos)?);
        }
        for (old, new) in replacements.clone() {
            if let Some(requirement) = self.record_requirements.get(&old).cloned() {
                let mut fields = BTreeMap::new();
                for (name, ty) in requirement.fields {
                    fields.insert(name, self.copy_type(ty, &replacements, pos)?);
                }
                let rest = requirement
                    .rest
                    .map(|ty| self.copy_type(ty, &replacements, pos))
                    .transpose()?;
                self.record_requirements
                    .insert(new, RecordRequirement { fields, rest });
            }
            if let Some(required) = self.trait_requirements.get(&old).cloned() {
                self.trait_requirements.insert(new, required);
            }
        }
        self.copy_type(scheme.ty, &replacements, pos)
    }

    fn instantiate_trait_method(
        &mut self,
        method: &TraitMethodSpec,
        self_ty: TypeId,
        pos: &Pos,
    ) -> Result<TypeId, TypeError> {
        let self_id = self.root(method.self_type, pos)?;
        let mut replacements = BTreeMap::from([(self_id, self_ty)]);
        for id in &method.scheme.quantified {
            let id = self.root(*id, pos)?;
            if id != self_id {
                replacements.insert(id, self.fresh(pos)?);
            }
        }
        for (old, new) in replacements.clone() {
            if old == self_id {
                continue;
            }
            if let Some(required) = self.trait_requirements.get(&old).cloned() {
                self.trait_requirements.insert(new, required);
            }
        }
        self.copy_type(method.scheme.ty, &replacements, pos)
    }

    fn copy_type(
        &mut self,
        id: TypeId,
        replacements: &BTreeMap<TypeId, TypeId>,
        pos: &Pos,
    ) -> Result<TypeId, TypeError> {
        self.copy_type_inner(id, replacements, pos, &mut BTreeMap::new())
    }

    fn copy_type_inner(
        &mut self,
        id: TypeId,
        replacements: &BTreeMap<TypeId, TypeId>,
        pos: &Pos,
        copied: &mut BTreeMap<TypeId, TypeId>,
    ) -> Result<TypeId, TypeError> {
        let id = self.root(id, pos)?;
        if let Some(copy) = replacements.get(&id) {
            return Ok(*copy);
        }
        if let Some(copy) = copied.get(&id) {
            return Ok(*copy);
        }
        match self.types[id].clone() {
            Type::Var | Type::Rigid(_) | Type::IntegerLiteral => Ok(id),
            Type::Link(_) => unreachable!(),
            Type::Named(n, xs) => {
                let placeholder = self.fresh(pos)?;
                copied.insert(id, placeholder);
                let xs = xs
                    .into_iter()
                    .map(|x| self.copy_type_inner(x, replacements, pos, copied))
                    .collect::<Result<_, _>>()?;
                self.types[placeholder] = Type::Named(n, xs);
                Ok(placeholder)
            }
            Type::Function(xs, r) => {
                let placeholder = self.fresh(pos)?;
                copied.insert(id, placeholder);
                let xs = xs
                    .into_iter()
                    .map(|x| self.copy_type_inner(x, replacements, pos, copied))
                    .collect::<Result<_, _>>()?;
                let r = self.copy_type_inner(r, replacements, pos, copied)?;
                self.types[placeholder] = Type::Function(xs, r);
                Ok(placeholder)
            }
            Type::Record(fields, rest) => {
                let placeholder = self.fresh(pos)?;
                copied.insert(id, placeholder);
                let mut copied_fields = BTreeMap::new();
                for (n, f) in fields {
                    copied_fields.insert(
                        n,
                        FieldType {
                            mutability: f.mutability,
                            ty: self.copy_type_inner(f.ty, replacements, pos, copied)?,
                        },
                    );
                }
                let rest = rest
                    .map(|r| self.copy_type_inner(r, replacements, pos, copied))
                    .transpose()?;
                self.types[placeholder] = Type::Record(copied_fields, rest);
                Ok(placeholder)
            }
        }
    }

    fn module(mut self, module: &Module) -> Result<CheckedModule, TypeError> {
        self.allow_external_traits = !module.imports.is_empty();
        for import in &module.imports {
            match &import.kind {
                ImportType::Unqualified => self.allow_external_unqualified = true,
                ImportType::Selective(names) => {
                    for name in names {
                        self.external(name, &import.pos)?;
                    }
                }
                ImportType::Qualified(Some(alias)) => {
                    self.module_aliases.insert(alias.clone());
                }
                ImportType::Qualified(None) => self.allow_external_unqualified = true,
            }
        }
        // Register nominal arities before resolving annotations and constructors.
        for declaration in &module.declarations {
            match &declaration.kind {
                DeclarationKind::Data {
                    name, type_vars, ..
                } => {
                    self.named_types.insert(name.clone(), type_vars.len());
                }
                DeclarationKind::TypeAlias { name, params, ty } => {
                    self.named_types.insert(name.clone(), params.len());
                    self.aliases
                        .insert(name.clone(), (params.clone(), ty.clone()));
                }
                DeclarationKind::JsData { name, .. } => {
                    self.named_types.insert(name.clone(), 0);
                }
                _ => {}
            }
        }
        // Recursive functions need monomorphic placeholders in scope while bodies are checked.
        for declaration in &module.declarations {
            if let DeclarationKind::Function { name, .. } = &declaration.kind {
                let ty = self.fresh(&declaration.pos)?;
                self.bind_mono(name, ty, Mutability::Immutable);
            }
        }
        let mut declarations = Vec::with_capacity(module.declarations.len());
        for declaration in &module.declarations {
            declarations.push(self.declaration(declaration)?);
        }
        Ok(CheckedModule {
            module: module.clone(),
            declarations,
            types: self.types,
        })
    }

    fn declaration(&mut self, declaration: &Declaration) -> Result<Option<TypeId>, TypeError> {
        let pos = &declaration.pos;
        match &declaration.kind {
            DeclarationKind::Let {
                mutability,
                pattern,
                type_vars,
                annotation,
                value,
            } => {
                let value_ty = self.expression(value)?;
                if let Some(annotation) = annotation {
                    let mut vars = BTreeMap::new();
                    for type_var in type_vars {
                        vars.insert(
                            type_var.name.clone(),
                            self.rigid(&type_var.name, &type_var.pos)?,
                        );
                    }
                    let ann = self.annotation(annotation, pos, &mut vars)?;
                    self.unify(value_ty, ann, pos)?;
                }
                self.bind_let_pattern(pattern, value_ty, *mutability, pos)?;
                Ok(Some(value_ty))
            }
            DeclarationKind::Function {
                name,
                type_vars,
                function,
            } => {
                let expected = self.lookup(name, pos)?.scheme.ty;
                let actual = self.function_with_vars(function, type_vars, pos)?;
                self.unify(expected, actual, pos)?;
                self.values[0].remove(name);
                let scheme = self.generalize(actual, pos)?;
                self.values[0].insert(
                    name.clone(),
                    Binding {
                        scheme,
                        mutability: Mutability::Immutable,
                    },
                );
                Ok(Some(actual))
            }
            DeclarationKind::Declare { name, ty, .. } => {
                let ty = self.annotation(ty, pos, &mut BTreeMap::new())?;
                self.bind_mono(name, ty, Mutability::Immutable);
                Ok(Some(ty))
            }
            DeclarationKind::Data {
                name,
                type_vars,
                variants,
            } => {
                let mut vars = BTreeMap::new();
                for var in type_vars {
                    let id = self.fresh(&var.pos)?;
                    vars.insert(var.name.clone(), id);
                }
                let args = type_vars.iter().map(|v| vars[&v.name]).collect();
                let result = self.named(name, args, pos)?;
                for variant in variants {
                    let params = variant
                        .fields
                        .iter()
                        .map(|t| self.annotation(t, &variant.pos, &mut vars))
                        .collect::<Result<Vec<_>, _>>()?;
                    let ty = if params.is_empty() {
                        result
                    } else {
                        self.alloc(Type::Function(params, result), &variant.pos)?
                    };
                    self.values[0].insert(
                        variant.name.clone(),
                        Binding {
                            scheme: Scheme {
                                quantified: vars.values().copied().collect(),
                                ty,
                            },
                            mutability: Mutability::Immutable,
                        },
                    );
                }
                Ok(None)
            }
            DeclarationKind::JsData { name, variants } => {
                let nominal = self.named(name, vec![], pos)?;
                for variant in variants {
                    self.bind_mono(&variant.name, nominal, Mutability::Immutable);
                }
                Ok(None)
            }
            DeclarationKind::Trait { name, methods } => {
                let mut trait_spec = TraitSpec::default();
                for method in methods {
                    if trait_spec.methods.contains_key(&method.name) {
                        return self.err(
                            &method.pos,
                            TypeErrorKind::DuplicateBinding(method.name.clone()),
                        );
                    }
                    let mut vars = BTreeMap::new();
                    let ty = self.annotation(&method.ty, &method.pos, &mut vars)?;
                    let Some(self_type) = vars.get("self").copied() else {
                        return self.err(
                            &method.pos,
                            TypeErrorKind::TraitError("trait method must mention self".into()),
                        );
                    };
                    self.trait_requirements
                        .entry(self_type)
                        .or_default()
                        .insert(name.clone());
                    if let Some(default) = &method.default {
                        let actual = self.expression(default)?;
                        self.unify(ty, actual, &method.pos)?;
                    }
                    let scheme = self.generalize(ty, &method.pos)?;
                    let self_type = self.root(self_type, &method.pos)?;
                    trait_spec.methods.insert(
                        method.name.clone(),
                        TraitMethodSpec {
                            scheme: scheme.clone(),
                            self_type,
                            has_default: method.default.is_some(),
                        },
                    );
                    self.values[0].insert(
                        method.name.clone(),
                        Binding {
                            scheme,
                            mutability: Mutability::Immutable,
                        },
                    );
                }
                self.traits.insert(name.clone(), trait_spec);
                Ok(None)
            }
            DeclarationKind::Impl {
                trait_name,
                impl_type,
                methods,
                ..
            } => {
                let trait_name = reference_leaf(trait_name).to_owned();
                let trait_spec = self.traits.get(&trait_name).cloned();
                if trait_spec.is_none() && !self.allow_external_traits {
                    return self.err(
                        pos,
                        TypeErrorKind::TraitError(format!("unknown trait {trait_name}")),
                    );
                }
                let mut seen = BTreeSet::new();
                for (name, _) in methods {
                    if !seen.insert(name.clone()) {
                        return self.err(pos, TypeErrorKind::DuplicateBinding(name.clone()));
                    }
                    if trait_spec
                        .as_ref()
                        .is_some_and(|spec| !spec.methods.contains_key(name))
                    {
                        return self.err(
                            pos,
                            TypeErrorKind::TraitError(format!("unexpected impl method {name}")),
                        );
                    }
                }
                if let Some(trait_spec) = &trait_spec {
                    for (name, method) in &trait_spec.methods {
                        if !method.has_default && !seen.contains(name) {
                            return self.err(
                                pos,
                                TypeErrorKind::TraitError(format!("missing impl method {name}")),
                            );
                        }
                    }
                }
                let record_impl = matches!(impl_type, ImplType::Record { .. });
                if record_impl {
                    self.values.push(BTreeMap::new());
                    let input = self.fresh(pos)?;
                    let output = self.fresh(pos)?;
                    let field_map = self.alloc(Type::Function(vec![input], output), pos)?;
                    self.bind_mono("fieldMap", field_map, Mutability::Immutable);
                }
                let self_ty = match impl_type {
                    ImplType::Nominal { name, type_vars } => {
                        let nominal = reference_leaf(name);
                        let expected = self.named_types.get(nominal).copied().unwrap_or(0);
                        if expected != type_vars.len() {
                            return self.err(
                                pos,
                                TypeErrorKind::Arity {
                                    expected,
                                    actual: type_vars.len(),
                                },
                            );
                        }
                        let args = type_vars
                            .iter()
                            .map(|var| self.fresh(&var.pos))
                            .collect::<Result<Vec<_>, _>>()?;
                        self.named(nominal, args, pos)?
                    }
                    ImplType::Function { arity } => {
                        let args = (0..*arity)
                            .map(|_| self.fresh(pos))
                            .collect::<Result<Vec<_>, _>>()?;
                        let result = self.fresh(pos)?;
                        self.alloc(Type::Function(args, result), pos)?
                    }
                    ImplType::Record { field_function } => {
                        let transformer = self.expression(field_function)?;
                        let scheme = self.generalize(transformer, pos)?;
                        self.record_trait_instances
                            .insert(trait_name.clone(), scheme);
                        let tail = self.fresh(pos)?;
                        self.alloc(Type::Record(BTreeMap::new(), Some(tail)), pos)?
                    }
                };
                let checked = methods.iter().try_for_each(|(name, value)| {
                    let actual = self.expression(value)?;
                    if let Some(method) =
                        trait_spec.as_ref().and_then(|spec| spec.methods.get(name))
                    {
                        let expected = self.instantiate_trait_method(method, self_ty, pos)?;
                        self.unify(actual, expected, pos)
                    } else {
                        Ok(())
                    }
                });
                if record_impl {
                    self.values.pop();
                }
                checked?;
                let self_root = self.root(self_ty, pos)?;
                if let Type::Named(name, _) = self.types[self_root].clone() {
                    self.trait_instances
                        .entry(trait_name)
                        .or_default()
                        .insert(name);
                } else if let Type::Function(args, _) = self.types[self_root].clone() {
                    self.function_trait_instances
                        .entry(trait_name)
                        .or_default()
                        .insert(args.len());
                }
                Ok(None)
            }
            DeclarationKind::Exception { name, ty } => {
                let payload = self.annotation(ty, pos, &mut BTreeMap::new())?;
                self.bind_mono(name, payload, Mutability::Immutable);
                Ok(None)
            }
            DeclarationKind::TypeAlias { .. } | DeclarationKind::ExportImport(_) => Ok(None),
        }
    }

    fn function(&mut self, function: &Function, pos: &Pos) -> Result<TypeId, TypeError> {
        self.function_with_vars(function, &[], pos)
    }

    fn function_with_vars(
        &mut self,
        function: &Function,
        type_vars: &[TypeVar],
        pos: &Pos,
    ) -> Result<TypeId, TypeError> {
        self.values.push(BTreeMap::new());
        let mut args = Vec::new();
        let mut annotation_vars = BTreeMap::new();
        for type_var in type_vars {
            let ty = self.rigid(&type_var.name, &type_var.pos)?;
            annotation_vars.insert(type_var.name.clone(), ty);
        }
        for type_var in type_vars {
            if let Some(constraint) = &type_var.constraints.record {
                let mut fields = BTreeMap::new();
                for (name, annotation) in &constraint.fields {
                    fields.insert(
                        name.clone(),
                        self.annotation(annotation, &type_var.pos, &mut annotation_vars)?,
                    );
                }
                let rest = constraint
                    .rest
                    .as_ref()
                    .map(|annotation| {
                        self.annotation(annotation, &type_var.pos, &mut annotation_vars)
                    })
                    .transpose()?;
                self.record_requirements.insert(
                    annotation_vars[&type_var.name],
                    RecordRequirement { fields, rest },
                );
            }
            if !type_var.constraints.traits.is_empty() {
                self.trait_requirements.insert(
                    annotation_vars[&type_var.name],
                    type_var
                        .constraints
                        .traits
                        .iter()
                        .map(|reference| reference_leaf(reference).to_owned())
                        .collect(),
                );
            }
        }
        for param in &function.params {
            let ty = if let Some((annotation, _)) = &param.annotation {
                self.annotation(annotation, pos, &mut annotation_vars)?
            } else {
                self.fresh(pos)?
            };
            self.pattern(&param.pattern, ty, Mutability::Immutable, pos)?;
            args.push(ty);
        }
        let result = if let Some(annotation) = &function.return_type {
            self.annotation(annotation, pos, &mut annotation_vars)?
        } else {
            self.fresh(pos)?
        };
        let old_return = self.return_type.replace(result);
        let body = self.expression(&function.body)?;
        self.unify(body, result, &function.body.pos)?;
        self.return_type = old_return;
        self.values.pop();
        self.alloc(Type::Function(args, result), pos)
    }

    fn expression(&mut self, expression: &Expression) -> Result<TypeId, TypeError> {
        if self.expression_depth >= MAX_UNIFY_DEPTH {
            return self.err(
                &expression.pos,
                TypeErrorKind::ResourceLimit("expression depth"),
            );
        }
        self.expression_depth += 1;
        let result = self.expression_inner(expression);
        self.expression_depth -= 1;
        result
    }

    fn expression_inner(&mut self, expression: &Expression) -> Result<TypeId, TypeError> {
        let pos = &expression.pos;
        match &expression.kind {
            ExpressionKind::Error => self.fresh(pos),
            ExpressionKind::Literal(Literal::Integer(_)) => self.alloc(Type::IntegerLiteral, pos),
            ExpressionKind::Literal(Literal::String(_)) => self.named("String", vec![], pos),
            ExpressionKind::Literal(Literal::Unit) => self.named("Unit", vec![], pos),
            ExpressionKind::Identifier(Reference::Unqualified(name))
                if name == "_unsafe_js" || name == "_unsafe_coerce" =>
            {
                self.err(
                    pos,
                    TypeErrorKind::Unsupported("intrinsic functions must be called directly"),
                )
            }
            ExpressionKind::Identifier(reference) => {
                self.lookup_reference(reference, pos).map(|b| b.scheme.ty)
            }
            ExpressionKind::Function(function) => self.function(function, pos),
            ExpressionKind::Apply(function, arguments) => {
                if matches!(&function.kind, ExpressionKind::Identifier(Reference::Unqualified(name)) if name == "_unsafe_js")
                {
                    if arguments.len() != 1 {
                        return self.err(
                            pos,
                            TypeErrorKind::Arity {
                                expected: 1,
                                actual: arguments.len(),
                            },
                        );
                    }
                    let argument = self.expression(&arguments[0])?;
                    let string = self.named("String", vec![], pos)?;
                    self.unify(argument, string, pos)?;
                    return self.fresh(pos);
                }
                if matches!(&function.kind, ExpressionKind::Identifier(Reference::Unqualified(name)) if name == "_unsafe_coerce")
                {
                    if arguments.len() != 1 {
                        return self.err(
                            pos,
                            TypeErrorKind::Arity {
                                expected: 1,
                                actual: arguments.len(),
                            },
                        );
                    }
                    self.expression(&arguments[0])?;
                    return self.fresh(pos);
                }
                let f = self.expression(function)?;
                let args = arguments
                    .iter()
                    .map(|a| self.expression(a))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.fresh(pos)?;
                let expected = self.alloc(Type::Function(args, result), pos)?;
                self.unify(f, expected, pos)?;
                Ok(result)
            }
            ExpressionKind::Binary(op, left, right) => {
                let a = self.expression(left)?;
                let b = self.expression(right)?;
                self.unify(a, b, pos)?;
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        Ok(a)
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        let boolean = self.named("Boolean", vec![], pos)?;
                        self.unify(a, boolean, pos)?;
                        Ok(boolean)
                    }
                    _ => self.named("Boolean", vec![], pos),
                }
            }
            ExpressionKind::Unary(_, value) => {
                let value = self.expression(value)?;
                Ok(value)
            }
            ExpressionKind::Let {
                mutability,
                pattern,
                annotation,
                value,
                ..
            } => {
                let value_ty = self.expression(value)?;
                if let Some(annotation) = annotation {
                    let ann = self.annotation(annotation, pos, &mut BTreeMap::new())?;
                    self.unify(value_ty, ann, pos)?;
                }
                self.bind_let_pattern(pattern, value_ty, *mutability, pos)?;
                self.named("Unit", vec![], pos)
            }
            ExpressionKind::Sequence(left, right) => {
                self.expression(left)?;
                self.expression(right)
            }
            ExpressionKind::If(condition, yes, no) => {
                let cond = self.expression(condition)?;
                let boolean = self.named("Boolean", vec![], pos)?;
                self.unify(cond, boolean, pos)?;
                self.values.push(BTreeMap::new());
                let yes = self.expression(yes);
                self.values.pop();
                let yes = yes?;
                self.values.push(BTreeMap::new());
                let no = self.expression(no);
                self.values.pop();
                let no = no?;
                self.unify(yes, no, pos)?;
                Ok(yes)
            }
            ExpressionKind::Array(mutability, elements) => {
                let element = self.fresh(pos)?;
                for value in elements {
                    let t = self.expression(value)?;
                    self.unify(element, t, &value.pos)?;
                }
                let array = self.named("Array", vec![element], pos)?;
                if *mutability == Mutability::Mutable {
                    let weak = self.free_vars(array, pos)?;
                    self.weak_vars.extend(weak);
                }
                Ok(array)
            }
            ExpressionKind::Tuple(elements) => {
                let values = elements
                    .iter()
                    .map(|e| self.expression(e))
                    .collect::<Result<Vec<_>, _>>()?;
                self.named(&format!("Tuple{}", values.len()), values, pos)
            }
            ExpressionKind::Record(fields) => {
                let mut record = BTreeMap::new();
                for (name, (mutability, value)) in fields {
                    let ty = self.expression(value)?;
                    if *mutability == Mutability::Mutable {
                        let weak = self.free_vars(ty, pos)?;
                        self.weak_vars.extend(weak);
                    }
                    record.insert(
                        name.clone(),
                        FieldType {
                            mutability: Some(*mutability),
                            ty,
                        },
                    );
                }
                self.alloc(Type::Record(record, None), pos)
            }
            ExpressionKind::Lookup(value, name) => {
                if let ExpressionKind::Identifier(Reference::Unqualified(module)) = &value.kind {
                    if self.module_aliases.contains(module) {
                        return self
                            .external(&format!("{module}.{name}"), pos)
                            .map(|binding| binding.scheme.ty);
                    }
                }
                let value = self.expression(value)?;
                let field = self.fresh(pos)?;
                let rest = self.fresh(pos)?;
                let expected = self.alloc(
                    Type::Record(
                        BTreeMap::from([(
                            name.clone(),
                            FieldType {
                                mutability: None,
                                ty: field,
                            },
                        )]),
                        Some(rest),
                    ),
                    pos,
                )?;
                self.unify(value, expected, pos)?;
                Ok(field)
            }
            ExpressionKind::Assign(target, value) => {
                let rhs = self.expression(value)?;
                let lhs = match &target.kind {
                    ExpressionKind::Identifier(reference) => {
                        let binding = self.lookup(reference_leaf(reference), &target.pos)?;
                        if binding.mutability != Mutability::Mutable {
                            return self.err(pos, TypeErrorKind::ImmutableAssignment);
                        }
                        binding.scheme.ty
                    }
                    ExpressionKind::Lookup(record, name) => {
                        let record_ty = self.expression(record)?;
                        let field = self.fresh(pos)?;
                        let rest = self.fresh(pos)?;
                        let expected = self.alloc(
                            Type::Record(
                                BTreeMap::from([(
                                    name.clone(),
                                    FieldType {
                                        mutability: Some(Mutability::Mutable),
                                        ty: field,
                                    },
                                )]),
                                Some(rest),
                            ),
                            pos,
                        )?;
                        self.unify(record_ty, expected, pos)?;
                        field
                    }
                    _ => return self.err(pos, TypeErrorKind::InvalidAssignmentTarget),
                };
                self.unify(lhs, rhs, pos)?;
                self.named("Unit", vec![], pos)
            }
            ExpressionKind::Match(subject, cases) => {
                let subject = self.expression(subject)?;
                let result = self.fresh(pos)?;
                for case in cases {
                    self.values.push(BTreeMap::new());
                    self.pattern(
                        &case.pattern,
                        subject,
                        Mutability::Immutable,
                        &case.body.pos,
                    )?;
                    let body = self.expression(&case.body)?;
                    self.unify(result, body, &case.body.pos)?;
                    self.values.pop();
                }
                Ok(result)
            }
            ExpressionKind::While(condition, body) => {
                let condition = self.expression(condition)?;
                let boolean = self.named("Boolean", vec![], pos)?;
                self.unify(condition, boolean, pos)?;
                self.values.push(BTreeMap::new());
                self.loop_depth += 1;
                let body = self.expression(body);
                self.loop_depth -= 1;
                self.values.pop();
                body?;
                self.named("Unit", vec![], pos)
            }
            ExpressionKind::For(pattern, over, body) => {
                let element = self.fresh(pos)?;
                let over_ty = self.expression(over)?;
                let array = self.named("Array", vec![element], pos)?;
                self.unify(over_ty, array, pos)?;
                self.values.push(BTreeMap::new());
                self.pattern(pattern, element, Mutability::Immutable, pos)?;
                self.loop_depth += 1;
                self.expression(body)?;
                self.loop_depth -= 1;
                self.values.pop();
                self.named("Unit", vec![], pos)
            }
            ExpressionKind::Return(value) => {
                let Some(expected) = self.return_type else {
                    return self.err(pos, TypeErrorKind::ReturnOutsideFunction);
                };
                let actual = self.expression(value)?;
                self.unify(expected, actual, pos)?;
                self.fresh(pos)
            }
            ExpressionKind::Throw(reference, value) => {
                let payload = self.lookup_reference(reference, pos)?.scheme.ty;
                let actual = self.expression(value)?;
                self.unify(payload, actual, pos)?;
                self.fresh(pos)
            }
            ExpressionKind::TryCatch(body, binding, handler) => {
                let a = self.expression(body)?;
                self.values.push(BTreeMap::new());
                let bound = match binding {
                    CatchBinding::Wildcard => Ok(()),
                    CatchBinding::CruxException(reference, pattern) => {
                        let payload = self.lookup_reference(reference, pos)?.scheme.ty;
                        self.pattern(pattern, payload, Mutability::Immutable, pos)
                    }
                };
                let b = match bound {
                    Ok(()) => self.expression(handler),
                    Err(error) => Err(error),
                };
                self.values.pop();
                let b = b?;
                self.unify(a, b, pos)?;
                Ok(a)
            }
            ExpressionKind::MethodApply(receiver, name, args)
                if name == "append" && args.len() == 1 =>
            {
                let receiver = self.expression(receiver)?;
                let element = self.expression(&args[0])?;
                let array = self.named("Array", vec![element], pos)?;
                self.unify(receiver, array, pos)?;
                self.named("Unit", vec![], pos)
            }
            ExpressionKind::MethodApply(receiver, _, args) => {
                self.expression(receiver)?;
                for arg in args {
                    self.expression(arg)?;
                }
                self.fresh(pos)
            }
            ExpressionKind::TypeLookup(_, _) => self.fresh(pos),
            ExpressionKind::As(value, annotation) => {
                let value = self.expression(value)?;
                let annotation = self.annotation(annotation, pos, &mut BTreeMap::new())?;
                self.unify(value, annotation, pos)?;
                Ok(annotation)
            }
        }
    }

    fn pattern(
        &mut self,
        pattern: &Pattern,
        ty: TypeId,
        mutability: Mutability,
        pos: &Pos,
    ) -> Result<(), TypeError> {
        match pattern {
            Pattern::Wildcard => Ok(()),
            Pattern::Binding(name) => {
                if self.values.last().unwrap().contains_key(name) {
                    return self.err(pos, TypeErrorKind::DuplicateBinding(name.clone()));
                }
                self.bind_mono(name, ty, mutability);
                Ok(())
            }
            Pattern::Tuple(patterns) => {
                if patterns.is_empty() {
                    let unit = self.named("Unit", vec![], pos)?;
                    return self.unify(ty, unit, pos);
                }
                let elements = patterns
                    .iter()
                    .map(|_| self.fresh(pos))
                    .collect::<Result<Vec<_>, _>>()?;
                let tuple =
                    self.named(&format!("Tuple{}", elements.len()), elements.clone(), pos)?;
                self.unify(ty, tuple, pos)?;
                for (p, t) in patterns.iter().zip(elements) {
                    self.pattern(p, t, mutability, pos)?;
                }
                Ok(())
            }
            Pattern::Constructor(reference, patterns) => {
                let constructor = self.lookup_reference(reference, pos)?.scheme.ty;
                if patterns.is_empty() {
                    self.unify(ty, constructor, pos)
                } else {
                    let params = patterns
                        .iter()
                        .map(|_| self.fresh(pos))
                        .collect::<Result<Vec<_>, _>>()?;
                    let function = self.alloc(Type::Function(params.clone(), ty), pos)?;
                    self.unify(constructor, function, pos)?;
                    for (p, t) in patterns.iter().zip(params) {
                        self.pattern(p, t, mutability, pos)?;
                    }
                    Ok(())
                }
            }
        }
    }

    fn bind_let_pattern(
        &mut self,
        pattern: &Pattern,
        ty: TypeId,
        mutability: Mutability,
        pos: &Pos,
    ) -> Result<(), TypeError> {
        let binding_name = match pattern {
            Pattern::Binding(name) => Some(name),
            // Declaration patterns introduce names. This also accommodates
            // conventional all-caps constants such as `let PORT = 8080`.
            Pattern::Constructor(Reference::Unqualified(name), args) if args.is_empty() => {
                Some(name)
            }
            _ => None,
        };
        if let Some(name) = binding_name {
            if self.values.last().unwrap().contains_key(name) {
                return self.err(pos, TypeErrorKind::DuplicateBinding(name.clone()));
            }
            let scheme = if mutability == Mutability::Immutable {
                self.generalize(ty, pos)?
            } else {
                Scheme {
                    quantified: vec![],
                    ty,
                }
            };
            self.values
                .last_mut()
                .unwrap()
                .insert(name.clone(), Binding { scheme, mutability });
            Ok(())
        } else {
            self.pattern(pattern, ty, mutability, pos)
        }
    }

    fn generalize(&mut self, ty: TypeId, pos: &Pos) -> Result<Scheme, TypeError> {
        let mut in_type = self.free_vars(ty, pos)?;
        let bindings = self
            .values
            .iter()
            .flat_map(|scope| scope.values())
            .map(|binding| binding.scheme.clone())
            .collect::<Vec<_>>();
        for binding in bindings {
            let free = self.free_vars(binding.ty, pos)?;
            for id in free {
                if !binding.quantified.contains(&id) {
                    in_type.remove(&id);
                }
            }
        }
        in_type.retain(|id| !self.weak_vars.contains(id));
        Ok(Scheme {
            quantified: in_type.into_iter().collect(),
            ty,
        })
    }

    fn free_vars(&mut self, ty: TypeId, pos: &Pos) -> Result<BTreeSet<TypeId>, TypeError> {
        let mut result = BTreeSet::new();
        let mut seen = BTreeSet::new();
        let mut todo = vec![ty];
        for _ in 0..MAX_TYPE_WALK {
            let Some(id) = todo.pop() else {
                return Ok(result);
            };
            let id = self.root(id, pos)?;
            if !seen.insert(id) {
                continue;
            }
            match self.types[id].clone() {
                Type::Var | Type::Rigid(_) => {
                    result.insert(id);
                }
                Type::IntegerLiteral => {}
                Type::Link(_) => unreachable!(),
                Type::Named(_, args) => todo.extend(args),
                Type::Function(args, result) => {
                    todo.extend(args);
                    todo.push(result);
                }
                Type::Record(fields, rest) => {
                    todo.extend(fields.values().map(|field| field.ty));
                    todo.extend(rest);
                }
            }
        }
        self.err(pos, TypeErrorKind::ResourceLimit("free-variable walk"))
    }

    fn annotation(
        &mut self,
        annotation: &TypeIdent,
        pos: &Pos,
        vars: &mut BTreeMap<Name, TypeId>,
    ) -> Result<TypeId, TypeError> {
        match annotation {
            TypeIdent::Wildcard => self.fresh(pos),
            TypeIdent::Named(reference, args) => {
                let name = reference_leaf(reference);
                if let Some(id) = vars.get(name) {
                    return Ok(*id);
                }
                if let Some((params, body)) = self.aliases.get(name).cloned() {
                    if self
                        .resolving_aliases
                        .iter()
                        .any(|resolving| resolving == name)
                    {
                        return self.err(pos, TypeErrorKind::ResourceLimit("cyclic type alias"));
                    }
                    let resolved_args = args
                        .iter()
                        .map(|arg| self.annotation(arg, pos, vars))
                        .collect::<Result<Vec<_>, _>>()?;
                    if params.is_empty() {
                        if let TypeIdent::Named(target, target_args) = &body {
                            if target_args.is_empty() && !resolved_args.is_empty() {
                                return self.named(reference_leaf(target), resolved_args, pos);
                            }
                        }
                        if !resolved_args.is_empty() {
                            return self.err(
                                pos,
                                TypeErrorKind::Arity {
                                    expected: 0,
                                    actual: resolved_args.len(),
                                },
                            );
                        }
                    } else if params.len() != resolved_args.len() {
                        return self.err(
                            pos,
                            TypeErrorKind::Arity {
                                expected: params.len(),
                                actual: resolved_args.len(),
                            },
                        );
                    }
                    let mut alias_vars = vars.clone();
                    for (param, arg) in params.into_iter().zip(resolved_args) {
                        alias_vars.insert(param, arg);
                    }
                    self.resolving_aliases.push(name.into());
                    let result = self.annotation(&body, pos, &mut alias_vars);
                    self.resolving_aliases.pop();
                    return result;
                }
                if !self.named_types.contains_key(name) {
                    let id = self.fresh(pos)?;
                    vars.insert(name.into(), id);
                    return Ok(id);
                }
                if let Some(arity) = self.named_types.get(name) {
                    if *arity != args.len() {
                        return self.err(
                            pos,
                            TypeErrorKind::Arity {
                                expected: *arity,
                                actual: args.len(),
                            },
                        );
                    }
                }
                let args = args
                    .iter()
                    .map(|a| self.annotation(a, pos, vars))
                    .collect::<Result<_, _>>()?;
                self.named(name, args, pos)
            }
            TypeIdent::Function(args, result) => {
                let args = args
                    .iter()
                    .map(|a| self.annotation(a, pos, vars))
                    .collect::<Result<_, _>>()?;
                let result = self.annotation(result, pos, vars)?;
                self.alloc(Type::Function(args, result), pos)
            }
            TypeIdent::Record(fields) => {
                let mut result = BTreeMap::new();
                for field in fields {
                    result.insert(
                        field.name.clone(),
                        FieldType {
                            mutability: field.mutability,
                            ty: self.annotation(&field.ty, pos, vars)?,
                        },
                    );
                }
                self.alloc(Type::Record(result, None), pos)
            }
            TypeIdent::Array(_, value) => {
                let value = self.annotation(value, pos, vars)?;
                self.named("Array", vec![value], pos)
            }
            TypeIdent::Tuple(values) if values.is_empty() => self.named("Unit", vec![], pos),
            TypeIdent::Tuple(values) => {
                let values = values
                    .iter()
                    .map(|v| self.annotation(v, pos, vars))
                    .collect::<Result<Vec<_>, _>>()?;
                self.named(&format!("Tuple{}", values.len()), values, pos)
            }
            TypeIdent::Option(value) => {
                let value = self.annotation(value, pos, vars)?;
                self.named("Option", vec![value], pos)
            }
        }
    }

    fn render(&mut self, id: TypeId, pos: &Pos) -> Result<String, TypeError> {
        self.render_inner(id, pos, &mut BTreeSet::new())
    }

    fn render_inner(
        &mut self,
        id: TypeId,
        pos: &Pos,
        seen: &mut BTreeSet<TypeId>,
    ) -> Result<String, TypeError> {
        let id = self.root(id, pos)?;
        if !seen.insert(id) {
            return Ok("...".into());
        }
        let rendered = match self.types[id].clone() {
            Type::Var => format!("_t{id}"),
            Type::Rigid(name) => name,
            Type::IntegerLiteral => "integer literal".into(),
            Type::Link(_) => unreachable!(),
            Type::Named(name, args) if args.is_empty() => name,
            Type::Named(name, args) => {
                let args = args
                    .into_iter()
                    .map(|x| self.render_inner(x, pos, seen))
                    .collect::<Result<Vec<_>, _>>()?;
                format!("{name}<{}>", args.join(", "))
            }
            Type::Function(args, result) => {
                let args = args
                    .into_iter()
                    .map(|x| self.render_inner(x, pos, seen))
                    .collect::<Result<Vec<_>, _>>()?;
                format!(
                    "fun({}) -> {}",
                    args.join(", "),
                    self.render_inner(result, pos, seen)?
                )
            }
            Type::Record(fields, rest) => {
                let mut fields = fields
                    .into_iter()
                    .map(|(n, f)| Ok(format!("{n}: {}", self.render_inner(f.ty, pos, seen)?)))
                    .collect::<Result<Vec<_>, TypeError>>()?;
                if rest.is_some() {
                    fields.push("...".into());
                }
                format!("{{{}}}", fields.join(", "))
            }
        };
        seen.remove(&id);
        Ok(rendered)
    }
}

fn reference_leaf(reference: &Reference) -> &str {
    match reference {
        Reference::Unqualified(name) | Reference::Qualified(_, name) => name,
    }
}

fn is_integer_literal_type(name: &str) -> bool {
    matches!(
        name,
        "Number"
            | "Int"
            | "Int8"
            | "UInt8"
            | "Int16"
            | "UInt16"
            | "Int32"
            | "UInt32"
            | "Int64"
            | "UInt64"
            | "Float32"
            | "Float64"
    )
}
