use std::collections::BTreeMap;

pub type Name = String;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pos {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub fn new(file: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            file: file.into(),
            line,
            column,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i128),
    String(String),
    Unit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JsLiteral {
    Undefined,
    Null,
    True,
    False,
    Integer(i128),
    String(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Reference {
    Unqualified(Name),
    Qualified(Name, Name),
}

impl From<&str> for Reference {
    fn from(value: &str) -> Self {
        Self::Unqualified(value.into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    Wildcard,
    Binding(Name),
    Constructor(Reference, Vec<Pattern>),
    Tuple(Vec<Pattern>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeIdent {
    Named(Reference, Vec<TypeIdent>),
    Record(Vec<RecordFieldType>),
    Function(Vec<TypeIdent>, Box<TypeIdent>),
    Array(Mutability, Box<TypeIdent>),
    Tuple(Vec<TypeIdent>),
    Option(Box<TypeIdent>),
    Wildcard,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecordFieldType {
    pub name: Name,
    /// `None` is `mutable?`, otherwise the concrete field mutability.
    pub mutability: Option<Mutability>,
    pub ty: TypeIdent,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecordConstraint {
    pub fields: Vec<(Name, TypeIdent)>,
    pub rest: Option<TypeIdent>,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ConstraintSet {
    pub record: Option<RecordConstraint>,
    pub traits: Vec<Reference>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeVar {
    pub name: Name,
    pub pos: Pos,
    pub constraints: ConstraintSet,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeIdent>,
    pub body: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParam {
    pub pattern: Pattern,
    pub annotation: Option<(TypeIdent, Option<Name>)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CatchBinding {
    CruxException(Reference, Pattern),
    Wildcard,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    /// Placeholder produced by an error-recovering parser.
    Error,
    Let {
        mutability: Mutability,
        pattern: Pattern,
        type_vars: Vec<TypeVar>,
        annotation: Option<TypeIdent>,
        value: Box<Expression>,
    },
    Lookup(Box<Expression>, Name),
    Apply(Box<Expression>, Vec<Expression>),
    Match(Box<Expression>, Vec<MatchCase>),
    Assign(Box<Expression>, Box<Expression>),
    Identifier(Reference),
    Sequence(Box<Expression>, Box<Expression>),
    MethodApply(Box<Expression>, Name, Vec<Expression>),
    TypeLookup(Reference, Name),
    As(Box<Expression>, TypeIdent),
    Function(Function),
    Record(BTreeMap<Name, (Mutability, Expression)>),
    Array(Mutability, Vec<Expression>),
    Tuple(Vec<Expression>),
    Literal(Literal),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    While(Box<Expression>, Box<Expression>),
    For(Pattern, Box<Expression>, Box<Expression>),
    Return(Box<Expression>),
    Throw(Reference, Box<Expression>),
    TryCatch(Box<Expression>, CatchBinding, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub pos: Pos,
    pub kind: ExpressionKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variant {
    pub pos: Pos,
    pub name: Name,
    pub fields: Vec<TypeIdent>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JsVariant {
    pub name: Name,
    pub value: JsLiteral,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImplType {
    Nominal {
        name: Reference,
        type_vars: Vec<TypeVar>,
    },
    Function {
        arity: usize,
    },
    Record {
        field_function: Expression,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclarationKind {
    ExportImport(Name),
    Declare {
        name: Name,
        type_vars: Vec<TypeVar>,
        ty: TypeIdent,
    },
    Let {
        mutability: Mutability,
        pattern: Pattern,
        type_vars: Vec<TypeVar>,
        annotation: Option<TypeIdent>,
        value: Expression,
    },
    Function {
        name: Name,
        type_vars: Vec<TypeVar>,
        function: Function,
    },
    Data {
        name: Name,
        type_vars: Vec<TypeVar>,
        variants: Vec<Variant>,
    },
    JsData {
        name: Name,
        variants: Vec<JsVariant>,
    },
    TypeAlias {
        name: Name,
        params: Vec<Name>,
        ty: TypeIdent,
    },
    Trait {
        name: Name,
        methods: Vec<TraitMethod>,
    },
    Impl {
        trait_name: Reference,
        impl_type: ImplType,
        context: Vec<Name>,
        methods: Vec<(Name, Expression)>,
    },
    Exception {
        name: Name,
        ty: TypeIdent,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitMethod {
    pub name: Name,
    pub pos: Pos,
    pub ty: TypeIdent,
    pub default: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub exported: bool,
    pub pos: Pos,
    pub kind: DeclarationKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportType {
    Unqualified,
    Selective(Vec<Name>),
    Qualified(Option<Name>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Import {
    pub pos: Pos,
    pub module: Vec<Name>,
    pub kind: ImportType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Pragma {
    NoBuiltin,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub pragmas: Vec<Pragma>,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
}
