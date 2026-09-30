use crate::ast::*;
use crate::lexer::{lex, LexError, Token, TokenKind};
use std::collections::BTreeMap;
use std::fmt;
use std::mem::discriminant;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    pub pos: Pos,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.pos.file, self.pos.line, self.pos.column, self.message
        )
    }
}
impl std::error::Error for ParseError {}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        Self {
            pos: value.pos,
            message: value.message,
        }
    }
}

/// Lex and parse an entire Crux module.
pub fn parse(file: &str, source: &str) -> Result<Module, ParseError> {
    parse_tokens(file, lex(file, source)?)
}

/// Parse an already-tokenized Crux module.
pub fn parse_tokens(file: &str, tokens: Vec<Token>) -> Result<Module, ParseError> {
    Parser::new(file, tokens).module()
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PatternContext {
    Refutable,
    Irrefutable,
}

type LetParts = (
    Pos,
    Mutability,
    Pattern,
    Vec<TypeVar>,
    Option<TypeIdent>,
    Expression,
);

struct Parser {
    file: String,
    tokens: Vec<Token>,
    at: usize,
    continuation_floor: Option<(usize, usize)>,
}

impl Parser {
    fn new(file: &str, tokens: Vec<Token>) -> Self {
        Self {
            file: file.into(),
            tokens,
            at: 0,
            continuation_floor: None,
        }
    }

    fn eof_pos(&self) -> Pos {
        self.tokens
            .last()
            .map(|t| t.pos.clone())
            .unwrap_or_else(|| Pos::new(&self.file, 1, 1))
    }
    fn error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            pos: self
                .peek()
                .map(|t| t.pos.clone())
                .unwrap_or_else(|| self.eof_pos()),
            message: message.into(),
        }
    }
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.at)
    }
    fn peek_n(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.at + n)
    }
    fn is(&self, kind: &TokenKind) -> bool {
        self.peek()
            .is_some_and(|t| discriminant(&t.kind) == discriminant(kind))
    }
    fn bump(&mut self) -> Result<Token, ParseError> {
        let token = self
            .peek()
            .cloned()
            .ok_or_else(|| self.error("unexpected end of input"))?;
        self.at += 1;
        Ok(token)
    }
    fn take(&mut self, kind: &TokenKind) -> Option<Token> {
        if self.is(kind) {
            self.at += 1;
            Some(self.tokens[self.at - 1].clone())
        } else {
            None
        }
    }
    fn expect(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        self.take(kind).ok_or_else(|| {
            self.error(format!(
                "expected {}, found {}",
                token_name(kind),
                self.peek()
                    .map(|t| token_name(&t.kind))
                    .unwrap_or("end of input")
            ))
        })
    }
    fn any_ident(&mut self) -> Result<(String, Pos), ParseError> {
        let token = self.bump()?;
        match token.kind {
            TokenKind::LowerIdent(s) | TokenKind::UpperIdent(s) => Ok((s, token.pos)),
            _ => Err(ParseError {
                pos: token.pos,
                message: "expected identifier".into(),
            }),
        }
    }

    fn module(mut self) -> Result<Module, ParseError> {
        let pragmas = if self.is(&TokenKind::Pragma) {
            self.pragmas()?
        } else {
            vec![]
        };
        let mut imports = vec![];
        while self.is(&TokenKind::Import) {
            imports.extend(self.imports()?);
        }
        let mut declarations = vec![];
        while self.peek().is_some() {
            if self.peek().unwrap().line_start != 1 {
                return Err(self.error("top-level declaration must begin at the left margin"));
            }
            declarations.push(self.declaration()?);
            self.take(&TokenKind::Semicolon);
        }
        Ok(Module {
            pragmas,
            imports,
            declarations,
        })
    }

    fn pragmas(&mut self) -> Result<Vec<Pragma>, ParseError> {
        let open = self.expect(&TokenKind::Pragma)?;
        self.expect(&TokenKind::OpenBrace)?;
        let mut result = vec![];
        while !self.is(&TokenKind::CloseBrace) {
            let (name, pos) = self.any_ident()?;
            match name.as_str() {
                "NoBuiltin" => result.push(Pragma::NoBuiltin),
                _ => {
                    return Err(ParseError {
                        pos,
                        message: format!("unknown pragma {name}"),
                    })
                }
            }
        }
        let close = self.expect(&TokenKind::CloseBrace)?;
        if close.pos.line > open.pos.line && close.line_start < open.line_start {
            return Err(ParseError {
                pos: close.pos,
                message: "pragma block is dedented too far".into(),
            });
        }
        Ok(result)
    }

    fn imports(&mut self) -> Result<Vec<Import>, ParseError> {
        let start = self.expect(&TokenKind::Import)?;
        if self.take(&TokenKind::OpenBrace).is_some() {
            let mut result = vec![];
            while !self.is(&TokenKind::CloseBrace) {
                result.push(self.import_after_keyword(start.pos.clone())?);
                self.take(&TokenKind::Comma);
            }
            self.expect(&TokenKind::CloseBrace)?;
            return Ok(result);
        }
        Ok(vec![self.import_after_keyword(start.pos)?])
    }

    fn import_after_keyword(&mut self, pos: Pos) -> Result<Import, ParseError> {
        let mut module = vec![self.any_ident()?.0];
        while self.is(&TokenKind::Dot)
            && self.peek_n(1).is_some_and(|t| {
                matches!(t.kind, TokenKind::LowerIdent(_) | TokenKind::UpperIdent(_))
            })
        {
            self.bump()?;
            module.push(self.any_ident()?.0);
        }
        let base = module.last().cloned().unwrap();
        let kind = if self.take(&TokenKind::OpenParen).is_some() {
            let kind = if self.take(&TokenKind::Ellipsis).is_some() {
                ImportType::Unqualified
            } else {
                ImportType::Selective(
                    self.comma_list(TokenKind::CloseParen, |p| Ok(p.any_ident()?.0))?,
                )
            };
            self.expect(&TokenKind::CloseParen)?;
            kind
        } else if self.take(&TokenKind::As).is_some() {
            if self.take(&TokenKind::Wildcard).is_some() {
                ImportType::Qualified(None)
            } else {
                ImportType::Qualified(Some(self.any_ident()?.0))
            }
        } else {
            ImportType::Qualified(Some(base))
        };
        Ok(Import { pos, module, kind })
    }

    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        let pos = self
            .peek()
            .ok_or_else(|| self.error("expected declaration"))?
            .pos
            .clone();
        let exported = self.take(&TokenKind::Export).is_some();
        let kind = match self.peek().map(|t| &t.kind) {
            Some(TokenKind::Declare) => self.declare_decl()?,
            Some(TokenKind::Data) => self.data_decl()?,
            Some(TokenKind::Type) => self.alias_decl()?,
            Some(TokenKind::Fun)
                if matches!(
                    self.peek_n(1).map(|t| &t.kind),
                    Some(TokenKind::LowerIdent(_) | TokenKind::UpperIdent(_))
                ) =>
            {
                self.fun_decl()?
            }
            Some(TokenKind::Let) => self.let_decl()?,
            Some(TokenKind::Trait) => self.trait_decl()?,
            Some(TokenKind::Impl) => self.impl_decl()?,
            Some(TokenKind::Exception) => self.exception_decl()?,
            Some(TokenKind::Import) if exported => {
                self.bump()?;
                DeclarationKind::ExportImport(self.any_ident()?.0)
            }
            _ if !exported => {
                let value = self.no_semi_expression()?;
                DeclarationKind::Let {
                    mutability: Mutability::Immutable,
                    pattern: Pattern::Wildcard,
                    type_vars: vec![],
                    annotation: None,
                    value,
                }
            }
            _ => return Err(self.error("expected declaration after export")),
        };
        Ok(Declaration {
            exported,
            pos,
            kind,
        })
    }

    fn declare_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        self.expect(&TokenKind::Declare)?;
        let name = self.any_ident()?.0;
        let type_vars = if self.is(&TokenKind::Less) {
            self.type_vars()?
        } else {
            vec![]
        };
        self.expect(&TokenKind::Colon)?;
        let ty = self.type_ident()?;
        Ok(DeclarationKind::Declare {
            name,
            type_vars,
            ty,
        })
    }

    fn let_parts(&mut self) -> Result<LetParts, ParseError> {
        let start = self.expect(&TokenKind::Let)?;
        let mutability = if self.take(&TokenKind::Mutable).is_some() {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let pattern = self.pattern(PatternContext::Irrefutable)?;
        let type_vars = if self.is(&TokenKind::Less) {
            self.type_vars()?
        } else {
            vec![]
        };
        let annotation = if self.take(&TokenKind::Colon).is_some() {
            Some(self.type_ident()?)
        } else {
            None
        };
        self.expect(&TokenKind::Equal)?;
        let value = self.no_semi_expression()?;
        Ok((start.pos, mutability, pattern, type_vars, annotation, value))
    }

    fn let_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        let (_, mutability, pattern, type_vars, annotation, value) = self.let_parts()?;
        Ok(DeclarationKind::Let {
            mutability,
            pattern,
            type_vars,
            annotation,
            value,
        })
    }

    fn fun_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        self.expect(&TokenKind::Fun)?;
        let name = self.any_ident()?.0;
        let type_vars = if self.is(&TokenKind::Less) {
            self.type_vars()?
        } else {
            vec![]
        };
        let function = self.function_tail(false)?;
        Ok(DeclarationKind::Function {
            name,
            type_vars,
            function,
        })
    }

    fn data_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        let start = self.expect(&TokenKind::Data)?;
        if self.take(&TokenKind::JsFfi).is_some() {
            let name = self.any_ident()?.0;
            self.expect(&TokenKind::OpenBrace)?;
            let variants = self.comma_list(TokenKind::CloseBrace, |p| {
                let name = p.any_ident()?.0;
                p.expect(&TokenKind::Equal)?;
                let value = p.js_literal()?;
                Ok(JsVariant { name, value })
            })?;
            self.expect(&TokenKind::CloseBrace)?;
            return Ok(DeclarationKind::JsData { name, variants });
        }
        let name = self.any_ident()?.0;
        let type_vars = self.declaration_type_vars()?;
        let variants = if self.take(&TokenKind::OpenParen).is_some() {
            let fields = self.comma_list(TokenKind::CloseParen, |p| p.type_ident())?;
            self.expect(&TokenKind::CloseParen)?;
            vec![Variant {
                pos: start.pos,
                name: name.clone(),
                fields,
            }]
        } else {
            self.expect(&TokenKind::OpenBrace)?;
            let values = self.comma_list(TokenKind::CloseBrace, |p| {
                let (name, pos) = p.any_ident()?;
                let fields = if p.take(&TokenKind::OpenParen).is_some() {
                    let fields = p.comma_list(TokenKind::CloseParen, |p| p.type_ident())?;
                    p.expect(&TokenKind::CloseParen)?;
                    fields
                } else {
                    vec![]
                };
                Ok(Variant { pos, name, fields })
            })?;
            self.expect(&TokenKind::CloseBrace)?;
            values
        };
        Ok(DeclarationKind::Data {
            name,
            type_vars,
            variants,
        })
    }

    fn js_literal(&mut self) -> Result<JsLiteral, ParseError> {
        let token = self.bump()?;
        match token.kind {
            TokenKind::LowerIdent(s) if s == "undefined" => Ok(JsLiteral::Undefined),
            TokenKind::LowerIdent(s) if s == "null" => Ok(JsLiteral::Null),
            TokenKind::LowerIdent(s) if s == "true" => Ok(JsLiteral::True),
            TokenKind::LowerIdent(s) if s == "false" => Ok(JsLiteral::False),
            TokenKind::Integer(i) => Ok(JsLiteral::Integer(i)),
            TokenKind::String(s) => Ok(JsLiteral::String(s)),
            _ => Err(ParseError {
                pos: token.pos,
                message: "expected JavaScript literal".into(),
            }),
        }
    }

    fn alias_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        self.expect(&TokenKind::Type)?;
        let name = self.any_ident()?.0;
        let params = if self.take(&TokenKind::Less).is_some() {
            let v = self.comma_list(TokenKind::Greater, |p| Ok(p.any_ident()?.0))?;
            self.expect(&TokenKind::Greater)?;
            v
        } else {
            let mut values = vec![];
            while matches!(
                self.peek().map(|t| &t.kind),
                Some(TokenKind::LowerIdent(_) | TokenKind::UpperIdent(_))
            ) {
                values.push(self.any_ident()?.0);
            }
            values
        };
        self.expect(&TokenKind::Equal)?;
        Ok(DeclarationKind::TypeAlias {
            name,
            params,
            ty: self.type_ident()?,
        })
    }

    fn trait_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        self.expect(&TokenKind::Trait)?;
        let name = self.any_ident()?.0;
        self.expect(&TokenKind::OpenBrace)?;
        let mut methods = vec![];
        while !self.is(&TokenKind::CloseBrace) {
            let (method_name, pos) = self.any_ident()?;
            let (ty, default) = if self.is(&TokenKind::OpenParen) {
                // Method sugar: either `(Type): Type`, or annotated params and a body.
                let checkpoint = self.at;
                match self.trait_default_function(pos.clone()) {
                    Ok(pair) => pair,
                    Err(_) => {
                        self.at = checkpoint;
                        self.expect(&TokenKind::OpenParen)?;
                        let args = self.comma_list(TokenKind::CloseParen, |p| p.type_ident())?;
                        self.expect(&TokenKind::CloseParen)?;
                        self.expect(&TokenKind::Colon)?;
                        (
                            TypeIdent::Function(args, Box::new(self.type_ident()?)),
                            None,
                        )
                    }
                }
            } else {
                self.expect(&TokenKind::Colon)?;
                let ty = self.type_ident()?;
                let default = if self.take(&TokenKind::Equal).is_some() {
                    Some(self.no_semi_expression()?)
                } else {
                    None
                };
                (ty, default)
            };
            methods.push(TraitMethod {
                name: method_name,
                pos,
                ty,
                default,
            });
            self.take(&TokenKind::Semicolon);
        }
        self.expect(&TokenKind::CloseBrace)?;
        Ok(DeclarationKind::Trait { name, methods })
    }

    fn trait_default_function(
        &mut self,
        pos: Pos,
    ) -> Result<(TypeIdent, Option<Expression>), ParseError> {
        self.expect(&TokenKind::OpenParen)?;
        let params = self.comma_list(TokenKind::CloseParen, |p| p.function_param(true))?;
        self.expect(&TokenKind::CloseParen)?;
        self.expect(&TokenKind::Colon)?;
        let ret = self.type_ident()?;
        if !self.is(&TokenKind::OpenBrace) {
            return Err(self.error("expected default method body"));
        }
        let body = self.block_expression()?;
        let types = params
            .iter()
            .map(|x| x.annotation.as_ref().unwrap().0.clone())
            .collect();
        let function = Expression {
            pos: pos.clone(),
            kind: ExpressionKind::Function(Function {
                params,
                return_type: Some(ret.clone()),
                body: Box::new(body),
            }),
        };
        Ok((TypeIdent::Function(types, Box::new(ret)), Some(function)))
    }

    fn impl_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        self.expect(&TokenKind::Impl)?;
        let trait_name = self.reference()?;
        enum Header {
            Nominal(Reference, Vec<TypeVar>),
            Function(usize),
            Record,
        }
        let header = if self.take(&TokenKind::OpenParen).is_some() {
            let args = self.comma_list(TokenKind::CloseParen, |p| p.reference())?;
            self.expect(&TokenKind::CloseParen)?;
            self.expect(&TokenKind::FatArrow)?;
            self.reference()?;
            Header::Function(args.len())
        } else if self.take(&TokenKind::OpenBrace).is_some() {
            self.expect(&TokenKind::Ellipsis)?;
            self.expect(&TokenKind::CloseBrace)?;
            Header::Record
        } else {
            let name = self.reference()?;
            let vars = if self.is(&TokenKind::Less) {
                self.type_vars()?
            } else {
                vec![]
            };
            Header::Nominal(name, vars)
        };
        self.expect(&TokenKind::OpenBrace)?;
        let mut methods = vec![];
        let mut field_transformers = vec![];
        while !self.is(&TokenKind::CloseBrace) {
            if let Some(tfor) = self.take(&TokenKind::For) {
                let field = self.any_ident()?.0;
                let body = self.block_expression()?;
                field_transformers.push((tfor.pos, field, body));
            } else {
                let (name, pos) = self.any_ident()?;
                let value = if self.is(&TokenKind::OpenParen) {
                    let function = self.function_tail(false)?;
                    Expression {
                        pos,
                        kind: ExpressionKind::Function(function),
                    }
                } else {
                    self.expect(&TokenKind::Equal)?;
                    self.no_semi_expression()?
                };
                methods.push((name, value));
            }
            self.take(&TokenKind::Semicolon);
        }
        self.expect(&TokenKind::CloseBrace)?;
        let impl_type = match header {
            Header::Nominal(name, type_vars) => {
                if !field_transformers.is_empty() {
                    return Err(self.error("nominal impls do not support field transformers"));
                }
                ImplType::Nominal { name, type_vars }
            }
            Header::Function(arity) => {
                if !field_transformers.is_empty() {
                    return Err(self.error("function impls do not support field transformers"));
                }
                ImplType::Function { arity }
            }
            Header::Record => {
                if field_transformers.len() != 1 {
                    return Err(self.error("record impl must have exactly one field transformer"));
                }
                let (pos, field, body) = field_transformers.pop().unwrap();
                ImplType::Record {
                    field_function: Expression {
                        pos,
                        kind: ExpressionKind::Function(Function {
                            params: vec![FunctionParam {
                                pattern: Pattern::Binding(field),
                                annotation: None,
                            }],
                            return_type: None,
                            body: Box::new(body),
                        }),
                    },
                }
            }
        };
        Ok(DeclarationKind::Impl {
            trait_name,
            impl_type,
            context: vec![],
            methods,
        })
    }

    fn exception_decl(&mut self) -> Result<DeclarationKind, ParseError> {
        self.expect(&TokenKind::Exception)?;
        let name = self.any_ident()?.0;
        let ty = self.type_ident()?;
        Ok(DeclarationKind::Exception { name, ty })
    }

    fn function_tail(&mut self, require_annotations: bool) -> Result<Function, ParseError> {
        self.expect(&TokenKind::OpenParen)?;
        let params = self.comma_list(TokenKind::CloseParen, |p| {
            p.function_param(require_annotations)
        })?;
        self.expect(&TokenKind::CloseParen)?;
        let return_type = if self.take(&TokenKind::Colon).is_some() {
            Some(self.type_ident()?)
        } else {
            None
        };
        let body = Box::new(self.block_expression()?);
        Ok(Function {
            params,
            return_type,
            body,
        })
    }

    fn function_param(&mut self, require_annotation: bool) -> Result<FunctionParam, ParseError> {
        let pattern = self.pattern(PatternContext::Irrefutable)?;
        let annotation = if self.take(&TokenKind::Colon).is_some() {
            let ty = self.type_ident()?;
            let alias = if self.take(&TokenKind::As).is_some() {
                Some(self.any_ident()?.0)
            } else {
                None
            };
            Some((ty, alias))
        } else if require_annotation {
            return Err(self.error("expected parameter type annotation"));
        } else {
            None
        };
        Ok(FunctionParam {
            pattern,
            annotation,
        })
    }

    fn type_vars(&mut self) -> Result<Vec<TypeVar>, ParseError> {
        self.expect(&TokenKind::Less)?;
        let result = self.comma_list(TokenKind::Greater, |p| {
            let (name, pos) = p.any_ident()?;
            let constraints = if p.take(&TokenKind::Colon).is_some() {
                let record = if p.is(&TokenKind::OpenBrace) {
                    Some(p.record_constraint()?)
                } else {
                    None
                };
                let mut traits = vec![];
                if record.is_none() {
                    traits.push(p.reference()?);
                }
                while p.take(&TokenKind::Plus).is_some() {
                    traits.push(p.reference()?);
                }
                ConstraintSet { record, traits }
            } else {
                ConstraintSet::default()
            };
            Ok(TypeVar {
                name,
                pos,
                constraints,
            })
        })?;
        self.expect(&TokenKind::Greater)?;
        Ok(result)
    }

    fn declaration_type_vars(&mut self) -> Result<Vec<TypeVar>, ParseError> {
        if self.is(&TokenKind::Less) {
            return self.type_vars();
        }
        let mut result = vec![];
        while matches!(
            self.peek().map(|t| &t.kind),
            Some(TokenKind::LowerIdent(_) | TokenKind::UpperIdent(_))
        ) {
            let (name, pos) = self.any_ident()?;
            result.push(TypeVar {
                name,
                pos,
                constraints: ConstraintSet::default(),
            });
        }
        Ok(result)
    }

    fn record_constraint(&mut self) -> Result<RecordConstraint, ParseError> {
        self.expect(&TokenKind::OpenBrace)?;
        let mut fields = vec![];
        while !self.is(&TokenKind::CloseBrace) && !self.is(&TokenKind::Ellipsis) {
            let name = self.any_ident()?.0;
            self.expect(&TokenKind::Colon)?;
            fields.push((name, self.type_ident()?));
            if self.take(&TokenKind::Comma).is_none() {
                break;
            }
        }
        let rest = if self.take(&TokenKind::Ellipsis).is_some() {
            self.expect(&TokenKind::Colon)?;
            Some(self.type_ident()?)
        } else {
            None
        };
        self.expect(&TokenKind::CloseBrace)?;
        Ok(RecordConstraint { fields, rest })
    }

    fn type_ident(&mut self) -> Result<TypeIdent, ParseError> {
        if self.take(&TokenKind::Wildcard).is_some() {
            return Ok(TypeIdent::Wildcard);
        }
        if self.take(&TokenKind::Question).is_some() {
            return Ok(TypeIdent::Option(Box::new(self.type_ident()?)));
        }
        let mutable = self.take(&TokenKind::Mutable).is_some();
        if self.take(&TokenKind::OpenBracket).is_some() {
            let inner = self.type_ident()?;
            self.expect(&TokenKind::CloseBracket)?;
            return Ok(TypeIdent::Array(
                if mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                },
                Box::new(inner),
            ));
        }
        if mutable {
            return Err(self.error("mutable is only valid before an array type"));
        }
        if self.take(&TokenKind::Fun).is_some() {
            self.expect(&TokenKind::OpenParen)?;
            let args = self.comma_list(TokenKind::CloseParen, |p| p.type_ident())?;
            self.expect(&TokenKind::CloseParen)?;
            self.expect(&TokenKind::Arrow)?;
            return Ok(TypeIdent::Function(args, Box::new(self.type_ident()?)));
        }
        if self.take(&TokenKind::OpenBrace).is_some() {
            let fields = self.comma_list(TokenKind::CloseBrace, |p| {
                let mutability = if p.take(&TokenKind::Mutable).is_some() {
                    if p.take(&TokenKind::Question).is_some() {
                        None
                    } else {
                        Some(Mutability::Mutable)
                    }
                } else if matches!(p.peek().map(|t| &t.kind), Some(TokenKind::LowerIdent(s)) if s == "const") {
                    p.bump()?;
                    Some(Mutability::Immutable)
                } else {
                    Some(Mutability::Immutable)
                };
                let name = p.any_ident()?.0;
                p.expect(&TokenKind::Colon)?;
                Ok(RecordFieldType {
                    name,
                    mutability,
                    ty: p.type_ident()?,
                })
            })?;
            self.expect(&TokenKind::CloseBrace)?;
            return Ok(TypeIdent::Record(fields));
        }
        if self.take(&TokenKind::OpenParen).is_some() {
            let elements = self.comma_list(TokenKind::CloseParen, |p| p.type_ident())?;
            self.expect(&TokenKind::CloseParen)?;
            if self.take(&TokenKind::FatArrow).is_some() || self.take(&TokenKind::Arrow).is_some() {
                return Ok(TypeIdent::Function(elements, Box::new(self.type_ident()?)));
            }
            return Ok(if elements.len() == 1 {
                elements.into_iter().next().unwrap()
            } else {
                TypeIdent::Tuple(elements)
            });
        }
        let type_line = self.peek().map(|token| token.pos.line).unwrap_or(0);
        let name = self.reference()?;
        let params = if self.take(&TokenKind::Less).is_some() {
            let values = self.comma_list(TokenKind::Greater, |p| p.type_ident())?;
            self.expect(&TokenKind::Greater)?;
            values
        } else {
            vec![]
        };
        let mut params = params;
        while self.peek().is_some_and(|token| {
            token.pos.line == type_line
                && matches!(
                    token.kind,
                    TokenKind::LowerIdent(_) | TokenKind::UpperIdent(_)
                )
        }) {
            params.push(self.type_ident()?);
        }
        let named = TypeIdent::Named(name, params);
        if self.take(&TokenKind::FatArrow).is_some() || self.take(&TokenKind::Arrow).is_some() {
            Ok(TypeIdent::Function(
                vec![named],
                Box::new(self.type_ident()?),
            ))
        } else {
            Ok(named)
        }
    }

    fn reference(&mut self) -> Result<Reference, ParseError> {
        let first = self.any_ident()?.0;
        if self.take(&TokenKind::Dot).is_some() {
            Ok(Reference::Qualified(first, self.any_ident()?.0))
        } else {
            Ok(Reference::Unqualified(first))
        }
    }

    fn pattern(&mut self, context: PatternContext) -> Result<Pattern, ParseError> {
        if self.take(&TokenKind::Wildcard).is_some() {
            return Ok(Pattern::Wildcard);
        }
        if self.take(&TokenKind::OpenParen).is_some() {
            let values = self.comma_list(TokenKind::CloseParen, |p| p.pattern(context))?;
            self.expect(&TokenKind::CloseParen)?;
            return Ok(if values.len() == 1 {
                values.into_iter().next().unwrap()
            } else {
                Pattern::Tuple(values)
            });
        }
        let token = self.bump()?;
        let (name, upper) = match token.kind {
            TokenKind::LowerIdent(s) => (s, false),
            TokenKind::UpperIdent(s) => (s, true),
            _ => {
                return Err(ParseError {
                    pos: token.pos,
                    message: "expected pattern".into(),
                })
            }
        };
        if self.take(&TokenKind::Dot).is_some() {
            let (constructor, _) = self.any_ident()?;
            let args = self.pattern_args(context)?;
            return Ok(Pattern::Constructor(
                Reference::Qualified(name, constructor),
                args,
            ));
        }
        let has_args = self.is(&TokenKind::OpenParen);
        if upper && (context == PatternContext::Refutable || has_args) {
            Ok(Pattern::Constructor(
                Reference::Unqualified(name),
                self.pattern_args(context)?,
            ))
        } else {
            Ok(Pattern::Binding(name))
        }
    }

    fn pattern_args(&mut self, context: PatternContext) -> Result<Vec<Pattern>, ParseError> {
        if self.take(&TokenKind::OpenParen).is_none() {
            return Ok(vec![]);
        }
        let args = self.comma_list(TokenKind::CloseParen, |p| p.pattern(context))?;
        self.expect(&TokenKind::CloseParen)?;
        Ok(args)
    }

    fn semi_expression(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.no_semi_expression()?;
        while self.take(&TokenKind::Semicolon).is_some() {
            if self.is(&TokenKind::CloseBrace) {
                break;
            }
            let rhs = self.no_semi_expression()?;
            let pos = lhs.pos.clone();
            lhs = Expression {
                pos,
                kind: ExpressionKind::Sequence(Box::new(lhs), Box::new(rhs)),
            };
        }
        Ok(lhs)
    }

    fn no_semi_expression(&mut self) -> Result<Expression, ParseError> {
        match self.peek().map(|t| &t.kind) {
            Some(TokenKind::Let) => self.let_expression(),
            Some(TokenKind::Match) => self.match_expression(),
            Some(TokenKind::If) => self.if_expression(),
            Some(TokenKind::While) => self.while_expression(),
            Some(TokenKind::For) => self.for_expression(),
            Some(TokenKind::Throw) => self.throw_expression(),
            Some(TokenKind::Try) => self.try_expression(),
            Some(TokenKind::Return) => self.return_expression(),
            _ => self.assign_expression(),
        }
    }

    fn let_expression(&mut self) -> Result<Expression, ParseError> {
        let (pos, mutability, pattern, type_vars, annotation, value) = self.let_parts()?;
        Ok(Expression {
            pos,
            kind: ExpressionKind::Let {
                mutability,
                pattern,
                type_vars,
                annotation,
                value: Box::new(value),
            },
        })
    }

    fn match_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::Match)?;
        let subject = self.no_semi_expression()?;
        self.expect(&TokenKind::OpenBrace)?;
        let mut cases = vec![];
        while !self.is(&TokenKind::CloseBrace) {
            let pattern = self.pattern(PatternContext::Refutable)?;
            let arrow = self.expect(&TokenKind::FatArrow)?;
            let body = if self.is(&TokenKind::OpenBrace) {
                self.block_expression()?
            } else {
                self.with_continuation_floor(arrow.pos.line, arrow.line_start, |p| {
                    p.no_semi_expression()
                })?
            };
            cases.push(MatchCase { pattern, body });
            self.take(&TokenKind::Semicolon);
        }
        self.expect(&TokenKind::CloseBrace)?;
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::Match(Box::new(subject), cases),
        })
    }

    fn if_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::If)?;
        let condition = self.no_semi_expression()?;
        let (yes, no) = if self.take(&TokenKind::Then).is_some() {
            let yes = self.no_semi_expression()?;
            self.expect(&TokenKind::Else)?;
            (yes, self.no_semi_expression()?)
        } else {
            let yes = self.block_expression()?;
            let no = if self.take(&TokenKind::Else).is_some() {
                if self.is(&TokenKind::If) {
                    self.if_expression()?
                } else {
                    self.block_expression()?
                }
            } else {
                Expression {
                    pos: start.pos.clone(),
                    kind: ExpressionKind::Literal(Literal::Unit),
                }
            };
            (yes, no)
        };
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::If(Box::new(condition), Box::new(yes), Box::new(no)),
        })
    }

    fn while_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::While)?;
        let cond = self.no_semi_expression()?;
        let body = self.block_expression()?;
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::While(Box::new(cond), Box::new(body)),
        })
    }

    fn for_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::For)?;
        let pattern = self.pattern(PatternContext::Irrefutable)?;
        self.expect(&TokenKind::In)?;
        let iter = self.no_semi_expression()?;
        let body = self.block_expression()?;
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::For(pattern, Box::new(iter), Box::new(body)),
        })
    }

    fn throw_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::Throw)?;
        let exception = self.reference()?;
        let value = self.no_semi_expression()?;
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::Throw(exception, Box::new(value)),
        })
    }

    fn try_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::Try)?;
        let body = self.block_expression()?;
        self.expect(&TokenKind::Catch)?;
        let binding = if self.take(&TokenKind::Wildcard).is_some() {
            CatchBinding::Wildcard
        } else {
            CatchBinding::CruxException(
                self.reference()?,
                self.pattern(PatternContext::Irrefutable)?,
            )
        };
        let catch = self.block_expression()?;
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::TryCatch(Box::new(body), binding, Box::new(catch)),
        })
    }

    fn return_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.expect(&TokenKind::Return)?;
        let has_value = self.peek().is_some_and(|next| {
            !matches!(next.kind, TokenKind::CloseBrace | TokenKind::Semicolon)
                && (next.pos.line == start.pos.line || next.pos.column > start.line_start)
        });
        let value = if has_value {
            self.no_semi_expression()?
        } else {
            Expression {
                pos: start.pos.clone(),
                kind: ExpressionKind::Literal(Literal::Unit),
            }
        };
        Ok(Expression {
            pos: start.pos,
            kind: ExpressionKind::Return(Box::new(value)),
        })
    }

    fn assign_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.boolean_expression()?;
        if self.take(&TokenKind::Equal).is_some() {
            let pos = lhs.pos.clone();
            let rhs = self.no_semi_expression()?;
            Ok(Expression {
                pos,
                kind: ExpressionKind::Assign(Box::new(lhs), Box::new(rhs)),
            })
        } else {
            Ok(lhs)
        }
    }

    fn boolean_expression(&mut self) -> Result<Expression, ParseError> {
        self.binary_left(
            |p| p.relation_expression(),
            &[
                (TokenKind::AndAnd, BinaryOp::And),
                (TokenKind::OrOr, BinaryOp::Or),
            ],
        )
    }
    fn relation_expression(&mut self) -> Result<Expression, ParseError> {
        self.binary_left(
            |p| p.add_expression(),
            &[
                (TokenKind::DoubleEqual, BinaryOp::Equal),
                (TokenKind::NotEqual, BinaryOp::NotEqual),
                (TokenKind::Less, BinaryOp::Less),
                (TokenKind::Greater, BinaryOp::Greater),
                (TokenKind::LessEqual, BinaryOp::LessEqual),
                (TokenKind::GreaterEqual, BinaryOp::GreaterEqual),
            ],
        )
    }
    fn add_expression(&mut self) -> Result<Expression, ParseError> {
        self.binary_left(
            |p| p.multiply_expression(),
            &[
                (TokenKind::Plus, BinaryOp::Add),
                (TokenKind::Minus, BinaryOp::Subtract),
            ],
        )
    }
    fn multiply_expression(&mut self) -> Result<Expression, ParseError> {
        self.binary_left(
            |p| p.as_expression(),
            &[
                (TokenKind::Star, BinaryOp::Multiply),
                (TokenKind::Slash, BinaryOp::Divide),
            ],
        )
    }

    fn binary_left<F>(
        &mut self,
        mut term: F,
        ops: &[(TokenKind, BinaryOp)],
    ) -> Result<Expression, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Expression, ParseError>,
    {
        let mut lhs = term(self)?;
        loop {
            if !self.continuation_allowed() {
                break;
            }
            let Some((_, op)) = ops.iter().find(|(kind, _)| self.is(kind)) else {
                break;
            };
            self.bump()?;
            let rhs = term(self)?;
            let pos = rhs.pos.clone(); // matches the original parser's source annotation
            lhs = Expression {
                pos,
                kind: ExpressionKind::Binary(*op, Box::new(lhs), Box::new(rhs)),
            };
        }
        Ok(lhs)
    }

    fn as_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary_expression()?;
        if self.take(&TokenKind::As).is_some() {
            let pos = expr.pos.clone();
            expr = Expression {
                pos,
                kind: ExpressionKind::As(Box::new(expr), self.type_ident()?),
            };
        }
        Ok(expr)
    }

    fn unary_expression(&mut self) -> Result<Expression, ParseError> {
        if let Some(minus) = self.take(&TokenKind::Minus) {
            let value = self.application_expression()?;
            Ok(Expression {
                pos: minus.pos,
                kind: ExpressionKind::Unary(UnaryOp::Negate, Box::new(value)),
            })
        } else {
            self.application_expression()
        }
    }

    fn application_expression(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.basic_expression()?;
        loop {
            if !self.continuation_allowed() {
                break;
            }
            if self.take(&TokenKind::OpenParen).is_some() {
                let args = self.comma_list(TokenKind::CloseParen, |p| p.no_semi_expression())?;
                self.expect(&TokenKind::CloseParen)?;
                let pos = lhs.pos.clone();
                lhs = Expression {
                    pos,
                    kind: ExpressionKind::Apply(Box::new(lhs), args),
                };
            } else if self.take(&TokenKind::Arrow).is_some() {
                let name = self.any_ident()?.0;
                self.expect(&TokenKind::OpenParen)?;
                let args = self.comma_list(TokenKind::CloseParen, |p| p.no_semi_expression())?;
                self.expect(&TokenKind::CloseParen)?;
                let pos = lhs.pos.clone();
                lhs = Expression {
                    pos,
                    kind: ExpressionKind::MethodApply(Box::new(lhs), name, args),
                };
            } else if self.take(&TokenKind::Dot).is_some() {
                let name = self.any_ident()?.0;
                let pos = lhs.pos.clone();
                lhs = Expression {
                    pos,
                    kind: ExpressionKind::Lookup(Box::new(lhs), name),
                };
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn basic_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self
            .peek()
            .cloned()
            .ok_or_else(|| self.error("expected expression"))?;
        match token.kind {
            TokenKind::Integer(i) => {
                self.at += 1;
                Ok(Expression {
                    pos: token.pos,
                    kind: ExpressionKind::Literal(Literal::Integer(i)),
                })
            }
            TokenKind::String(s) => {
                self.at += 1;
                Ok(Expression {
                    pos: token.pos,
                    kind: ExpressionKind::Literal(Literal::String(s)),
                })
            }
            TokenKind::OpenBracket => self.array_expression(Mutability::Immutable),
            TokenKind::Mutable
                if self
                    .peek_n(1)
                    .is_some_and(|t| matches!(t.kind, TokenKind::OpenBracket)) =>
            {
                self.at += 1;
                self.array_expression_at(Mutability::Mutable, token.pos)
            }
            TokenKind::OpenBrace => self.record_expression(),
            TokenKind::Fun => {
                self.at += 1;
                let function = self.function_tail(false)?;
                Ok(Expression {
                    pos: token.pos,
                    kind: ExpressionKind::Function(function),
                })
            }
            TokenKind::OpenParen => self.paren_expression(),
            TokenKind::Wildcard
                if self
                    .peek_n(1)
                    .is_some_and(|t| matches!(t.kind, TokenKind::FatArrow)) =>
            {
                self.at += 2;
                let body = self.no_semi_expression()?;
                Ok(Expression {
                    pos: token.pos,
                    kind: ExpressionKind::Function(Function {
                        params: vec![FunctionParam {
                            pattern: Pattern::Wildcard,
                            annotation: None,
                        }],
                        return_type: None,
                        body: Box::new(body),
                    }),
                })
            }
            TokenKind::LowerIdent(_) | TokenKind::UpperIdent(_) => self.identifier_expression(),
            _ => Err(ParseError {
                pos: token.pos,
                message: format!("expected expression, found {}", token_name(&token.kind)),
            }),
        }
    }

    fn identifier_expression(&mut self) -> Result<Expression, ParseError> {
        let (name, pos) = self.any_ident()?;
        if self.take(&TokenKind::FatArrow).is_some() {
            let body = if self.is(&TokenKind::OpenBrace) {
                self.block_expression()?
            } else {
                self.no_semi_expression()?
            };
            Ok(Expression {
                pos,
                kind: ExpressionKind::Function(Function {
                    params: vec![FunctionParam {
                        pattern: Pattern::Binding(name),
                        annotation: None,
                    }],
                    return_type: None,
                    body: Box::new(body),
                }),
            })
        } else if self.take(&TokenKind::ColonColon).is_some() {
            let method = self.any_ident()?.0;
            Ok(Expression {
                pos,
                kind: ExpressionKind::TypeLookup(Reference::Unqualified(name), method),
            })
        } else {
            Ok(Expression {
                pos,
                kind: ExpressionKind::Identifier(Reference::Unqualified(name)),
            })
        }
    }

    fn paren_expression(&mut self) -> Result<Expression, ParseError> {
        let open = self.expect(&TokenKind::OpenParen)?;
        let elements = self.comma_list(TokenKind::CloseParen, |p| p.semi_expression())?;
        self.expect(&TokenKind::CloseParen)?;
        if self.take(&TokenKind::FatArrow).is_some() {
            let mut params = vec![];
            for element in elements {
                let pattern = match element.kind {
                    ExpressionKind::Identifier(Reference::Unqualified(name)) => {
                        Pattern::Binding(name)
                    }
                    ExpressionKind::Literal(Literal::Unit) => Pattern::Tuple(vec![]),
                    _ => {
                        return Err(ParseError {
                            pos: element.pos,
                            message: "not a valid lambda pattern".into(),
                        })
                    }
                };
                params.push(FunctionParam {
                    pattern,
                    annotation: None,
                });
            }
            let body = if self.is(&TokenKind::OpenBrace) {
                self.block_expression()?
            } else {
                self.no_semi_expression()?
            };
            Ok(Expression {
                pos: open.pos,
                kind: ExpressionKind::Function(Function {
                    params,
                    return_type: None,
                    body: Box::new(body),
                }),
            })
        } else {
            Ok(match elements.len() {
                0 => Expression {
                    pos: open.pos,
                    kind: ExpressionKind::Literal(Literal::Unit),
                },
                1 => elements.into_iter().next().unwrap(),
                _ => Expression {
                    pos: open.pos,
                    kind: ExpressionKind::Tuple(elements),
                },
            })
        }
    }

    fn array_expression(&mut self, mutability: Mutability) -> Result<Expression, ParseError> {
        let open = self.expect(&TokenKind::OpenBracket)?;
        self.array_contents(mutability, open.pos)
    }
    fn array_expression_at(
        &mut self,
        mutability: Mutability,
        pos: Pos,
    ) -> Result<Expression, ParseError> {
        self.expect(&TokenKind::OpenBracket)?;
        self.array_contents(mutability, pos)
    }
    fn array_contents(
        &mut self,
        mutability: Mutability,
        pos: Pos,
    ) -> Result<Expression, ParseError> {
        let values = self.comma_list(TokenKind::CloseBracket, |p| p.no_semi_expression())?;
        self.expect(&TokenKind::CloseBracket)?;
        Ok(Expression {
            pos,
            kind: ExpressionKind::Array(mutability, values),
        })
    }

    fn record_expression(&mut self) -> Result<Expression, ParseError> {
        let open = self.expect(&TokenKind::OpenBrace)?;
        let pairs = self.comma_list(TokenKind::CloseBrace, |p| {
            let mutability = if p.take(&TokenKind::Mutable).is_some() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            };
            let name = p.any_ident()?.0;
            p.expect(&TokenKind::Colon)?;
            Ok((name, (mutability, p.no_semi_expression()?)))
        })?;
        self.expect(&TokenKind::CloseBrace)?;
        Ok(Expression {
            pos: open.pos,
            kind: ExpressionKind::Record(pairs.into_iter().collect::<BTreeMap<_, _>>()),
        })
    }

    fn block_expression(&mut self) -> Result<Expression, ParseError> {
        let open = self.expect(&TokenKind::OpenBrace)?;
        if self.take(&TokenKind::CloseBrace).is_some() {
            return Ok(Expression {
                pos: open.pos,
                kind: ExpressionKind::Literal(Literal::Unit),
            });
        }
        let first_line_start = self
            .peek()
            .ok_or_else(|| self.error("unterminated block"))?
            .line_start;
        if self.peek().unwrap().pos.line > open.pos.line
            && self.peek().unwrap().pos.column <= open.line_start
        {
            return Err(self.error("block contents must be indented"));
        }
        let first_line = self.peek().unwrap().pos.line;
        let first =
            self.with_continuation_floor(first_line, first_line_start, |p| p.semi_expression())?;
        let mut expressions = vec![first];
        while !self.is(&TokenKind::CloseBrace) {
            let next = self
                .peek()
                .ok_or_else(|| self.error("unterminated block"))?;
            if next.line_start != first_line_start {
                return Err(self.error(format!(
                    "block line must begin in column {first_line_start}"
                )));
            }
            let line = next.pos.line;
            let expression =
                self.with_continuation_floor(line, first_line_start, |p| p.semi_expression())?;
            expressions.push(expression);
        }
        self.expect(&TokenKind::CloseBrace)?;
        let mut iter = expressions.into_iter();
        let mut result = iter.next().unwrap();
        for rhs in iter {
            result = Expression {
                pos: open.pos.clone(),
                kind: ExpressionKind::Sequence(Box::new(result), Box::new(rhs)),
            };
        }
        Ok(result)
    }

    fn comma_list<T, F>(&mut self, end: TokenKind, mut parse_one: F) -> Result<Vec<T>, ParseError>
    where
        F: FnMut(&mut Self) -> Result<T, ParseError>,
    {
        let mut result = vec![];
        if self.is(&end) {
            return Ok(result);
        }
        result.push(parse_one(self)?);
        while self.take(&TokenKind::Comma).is_some() {
            if self.is(&end) {
                break;
            }
            result.push(parse_one(self)?);
        }
        Ok(result)
    }

    fn continuation_allowed(&self) -> bool {
        let Some((anchor_line, floor)) = self.continuation_floor else {
            return true;
        };
        self.peek()
            .is_none_or(|token| token.pos.line == anchor_line || token.pos.column > floor)
    }

    fn with_continuation_floor<T, F>(
        &mut self,
        line: usize,
        floor: usize,
        f: F,
    ) -> Result<T, ParseError>
    where
        F: FnOnce(&mut Self) -> Result<T, ParseError>,
    {
        let previous = self.continuation_floor;
        self.continuation_floor = Some((line, floor));
        let result = f(self);
        self.continuation_floor = previous;
        result
    }
}

fn token_name(kind: &TokenKind) -> &'static str {
    use TokenKind::*;
    match kind {
        Integer(_) => "integer",
        String(_) => "string",
        UpperIdent(_) | LowerIdent(_) => "identifier",
        OpenBrace => "{",
        CloseBrace => "}",
        OpenParen => "(",
        CloseParen => ")",
        OpenBracket => "[",
        CloseBracket => "]",
        Question => "?",
        Semicolon => ";",
        ColonColon => "::",
        Colon => ":",
        Comma => ",",
        Equal => "=",
        Dot => ".",
        Arrow => "->",
        FatArrow => "=>",
        Ellipsis => "...",
        Plus => "+",
        Minus => "-",
        Star => "*",
        Slash => "/",
        Less => "<",
        Greater => ">",
        LessEqual => "<=",
        GreaterEqual => ">=",
        DoubleEqual => "==",
        NotEqual => "!=",
        AndAnd => "&&",
        OrOr => "||",
        Wildcard => "_",
        As => "as",
        Pragma => "pragma",
        Import => "import",
        Export => "export",
        Fun => "fun",
        Let => "let",
        Data => "data",
        Declare => "declare",
        Exception => "exception",
        Throw => "throw",
        Try => "try",
        Catch => "catch",
        JsFfi => "jsffi",
        Type => "type",
        Match => "match",
        If => "if",
        Then => "then",
        Else => "else",
        While => "while",
        For => "for",
        In => "in",
        Do => "do",
        Return => "return",
        Mutable => "mutable",
        Trait => "trait",
        Impl => "impl",
    }
}
