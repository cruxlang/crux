//! Experimental Chumsky parser.
//!
//! This module deliberately lives beside the compatibility parser while the
//! approach is evaluated.  Its focus is expression parsing, Crux's physical
//! line layout, recovery, and the ambiguous expression forms that put the
//! most pressure on a parser implementation.

use crate::ast::*;
use crate::lexer::{lex, TokenKind};
use chumsky::input::{Input, ValueInput};
use chumsky::prelude::*;
use chumsky::recovery::via_parser;
use std::fmt;

type Span = SimpleSpan<usize>;

/// This bound is checked before Chumsky enters any recursive parser.  Together
/// with the fixed-size stack passed to `maybe_grow`, syntax nesting cannot
/// cause unbounded native-stack growth.
pub const MAX_SYNTAX_NESTING: usize = 256;
pub const MAX_TOKENS: usize = 1_000_000;

#[derive(Clone, Debug, PartialEq, Eq)]
enum LayoutToken {
    Lex(TokenKind),
    /// A physical line boundary followed by the first token's column.
    Newline(usize),
}

#[derive(Clone, Debug)]
enum Postfix {
    Call(Vec<Expression>),
    Lookup(Name),
    Method(Name, Vec<Expression>),
}

#[derive(Clone, Debug)]
enum IdentifierTail {
    Lambda(Expression),
    TypeLookup(Name),
    Plain,
}

#[derive(Clone, Debug)]
enum ParsedImplEntry {
    Method(Name, Expression),
    Field(Pos, Name, Expression),
}

impl fmt::Display for LayoutToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lex(token) => write!(f, "{token:?}"),
            Self::Newline(indent) => write!(f, "line break at column {indent}"),
        }
    }
}

#[derive(Clone, Debug)]
struct LayoutMeta {
    pos: Pos,
    line_start: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub pos: Pos,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct ParseResult {
    /// Chumsky can produce an AST and diagnostics at the same time.
    pub output: Option<Expression>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Debug)]
pub struct ModuleParseResult {
    pub output: Option<Module>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Parse a complete Crux source file with Chumsky.
pub fn parse_module(file: &str, source: &str) -> ModuleParseResult {
    let lexical = match bounded_lex(file, source) {
        Ok(tokens) => tokens,
        Err(diagnostic) => {
            return ModuleParseResult {
                output: None,
                diagnostics: vec![diagnostic],
            }
        }
    };
    let (stream, metadata) = layout_stream(lexical);
    let len = stream.len();
    let spanned = stream
        .into_iter()
        .enumerate()
        .map(|(index, token)| (token, Span::new((), index..index + 1)))
        .collect::<Vec<_>>();
    let input = spanned.as_slice().split_token_span(Span::new((), len..len));
    let (output, errors) = stacker::maybe_grow(8 * 1024 * 1024, 64 * 1024 * 1024, || {
        module_parser(&metadata)
            .then_ignore(end())
            .parse(input)
            .into_output_errors()
    });
    ModuleParseResult {
        output,
        diagnostics: diagnostics(file, &metadata, errors),
    }
}

/// Parse one expression using the experimental Chumsky grammar.
pub fn parse_expression(file: &str, source: &str) -> ParseResult {
    let lexical = match bounded_lex(file, source) {
        Ok(tokens) => tokens,
        Err(diagnostic) => {
            return ParseResult {
                output: None,
                diagnostics: vec![diagnostic],
            }
        }
    };
    let (stream, metadata) = layout_stream(lexical);

    let len = stream.len();
    let spanned = stream
        .into_iter()
        .enumerate()
        .map(|(index, token)| (token, Span::new((), index..index + 1)))
        .collect::<Vec<_>>();
    let input = spanned.as_slice().split_token_span(Span::new((), len..len));
    // Chumsky's combinator frames are large enough that the default Rust test
    // thread stack can overflow after only a handful of nested expressions.
    // Enter a spillable stack before the first parser frame is created; the
    // crate's own recursive guard cannot help if that first frame exhausts the
    // small platform stack before reaching the guard.
    let (output, errors) = stacker::maybe_grow(8 * 1024 * 1024, 64 * 1024 * 1024, || {
        expression_parser(&metadata)
            .then_ignore(end())
            .parse(input)
            .into_output_errors()
    });

    ParseResult {
        output,
        diagnostics: diagnostics(file, &metadata, errors),
    }
}

fn bounded_lex(file: &str, source: &str) -> Result<Vec<crate::lexer::Token>, Diagnostic> {
    let lexical = lex(file, source).map_err(|error| Diagnostic {
        pos: error.pos,
        message: error.message,
    })?;
    if lexical.len() > MAX_TOKENS {
        return Err(Diagnostic {
            pos: lexical[MAX_TOKENS].pos.clone(),
            message: format!("input exceeds the limit of {MAX_TOKENS} tokens"),
        });
    }
    let mut depth = 0usize;
    for token in &lexical {
        match token.kind {
            TokenKind::OpenParen | TokenKind::OpenBracket | TokenKind::OpenBrace => {
                depth += 1;
                if depth > MAX_SYNTAX_NESTING {
                    return Err(Diagnostic {
                        pos: token.pos.clone(),
                        message: format!(
                            "syntax nesting exceeds the limit of {MAX_SYNTAX_NESTING}"
                        ),
                    });
                }
            }
            TokenKind::CloseParen | TokenKind::CloseBracket | TokenKind::CloseBrace => {
                depth = depth.saturating_sub(1)
            }
            _ => {}
        }
    }
    Ok(lexical)
}

fn layout_stream(lexical: Vec<crate::lexer::Token>) -> (Vec<LayoutToken>, Vec<LayoutMeta>) {
    let mut stream = Vec::with_capacity(lexical.len() * 2);
    let mut metadata = Vec::with_capacity(lexical.len() * 2);
    let mut previous_line = None;
    for token in lexical {
        if previous_line.is_some_and(|line| line != token.pos.line) {
            stream.push(LayoutToken::Newline(token.line_start));
            metadata.push(LayoutMeta {
                pos: token.pos.clone(),
                line_start: token.line_start,
            });
        }
        previous_line = Some(token.pos.line);
        stream.push(LayoutToken::Lex(token.kind));
        metadata.push(LayoutMeta {
            pos: token.pos,
            line_start: token.line_start,
        });
    }
    (stream, metadata)
}

fn diagnostics<'tokens>(
    file: &str,
    metadata: &[LayoutMeta],
    errors: Vec<Rich<'tokens, LayoutToken, Span>>,
) -> Vec<Diagnostic> {
    let fallback = metadata
        .last()
        .map(|meta| meta.pos.clone())
        .unwrap_or_else(|| Pos::new(file, 1, 1));
    errors
        .into_iter()
        .map(|error| Diagnostic {
            pos: metadata
                .get(error.span().start)
                .map(|meta| meta.pos.clone())
                .unwrap_or_else(|| fallback.clone()),
            message: error.to_string(),
        })
        .collect()
}

fn type_ident_parser<'tokens, I>(
    _metadata: &'tokens [LayoutMeta],
) -> impl Parser<'tokens, I, TypeIdent, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
{
    let tok = |kind| just(LayoutToken::Lex(kind));
    let newline = select! { LayoutToken::Newline(indent) => indent };
    let newlines = newline.repeated().ignored();
    let name = select! {
        LayoutToken::Lex(TokenKind::LowerIdent(name)) => name,
        LayoutToken::Lex(TokenKind::UpperIdent(name)) => name,
    };
    let reference = name
        .then(tok(TokenKind::Dot).ignore_then(name).or_not())
        .map(|(first, second)| match second {
            Some(second) => Reference::Qualified(first, second),
            None => Reference::Unqualified(first),
        });

    recursive(|ty| {
        let comma = newlines.ignore_then(tok(TokenKind::Comma)).then_ignore(newlines);
        let list = ty.clone().separated_by(comma.clone()).allow_trailing().collect::<Vec<_>>();
        let wildcard = tok(TokenKind::Wildcard).to(TypeIdent::Wildcard);
        let option = tok(TokenKind::Question).ignore_then(ty.clone()).map(|ty| TypeIdent::Option(Box::new(ty)));
        let array = tok(TokenKind::Mutable).to(Mutability::Mutable).or_not()
            .then(newlines.ignore_then(ty.clone()).then_ignore(newlines)
                .delimited_by(tok(TokenKind::OpenBracket), tok(TokenKind::CloseBracket)))
            .map(|(mutable, ty)| TypeIdent::Array(mutable.unwrap_or(Mutability::Immutable), Box::new(ty)));
        let function = tok(TokenKind::Fun).ignore_then(
            newlines.ignore_then(list.clone()).then_ignore(newlines)
                .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen)))
            .then_ignore(tok(TokenKind::Arrow))
            .then(ty.clone())
            .map(|(args, result)| TypeIdent::Function(args, Box::new(result)));
        let field_mutability = choice((
            tok(TokenKind::Mutable).ignore_then(tok(TokenKind::Question).or_not())
                .map(|question| if question.is_some() { None } else { Some(Mutability::Mutable) }),
            select! { LayoutToken::Lex(TokenKind::LowerIdent(name)) if name == "const" => Some(Mutability::Immutable) },
        )).or_not().map(|value| value.unwrap_or(Some(Mutability::Immutable)));
        let field = field_mutability
            .then(name)
            .then_ignore(tok(TokenKind::Colon))
            .then(ty.clone())
            .map(|((mutable, name), ty)| RecordFieldType {
                name,
                mutability: mutable,
                ty,
            });
        let record = newlines.ignore_then(field.separated_by(comma.clone()).allow_trailing().collect::<Vec<_>>())
            .then_ignore(newlines)
            .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace))
            .map(TypeIdent::Record);
        let paren = newlines.ignore_then(list).then_ignore(newlines)
            .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
            .then(tok(TokenKind::FatArrow).or(tok(TokenKind::Arrow)).ignore_then(ty.clone()).or_not())
            .map(|(mut values, result)| match result {
                Some(result) => TypeIdent::Function(values, Box::new(result)),
                None if values.len() == 1 => values.pop().unwrap(),
                None => TypeIdent::Tuple(values),
            });
        let named = reference.clone()
            .then(newlines.ignore_then(ty.clone().separated_by(comma).allow_trailing().collect::<Vec<_>>())
                .then_ignore(newlines)
                .delimited_by(tok(TokenKind::Less), tok(TokenKind::Greater)).or_not())
            .then(ty.clone().repeated().collect::<Vec<_>>())
            .map(|((reference, angle_args), adjacent_args)| {
                let mut args = angle_args.unwrap_or_default();
                args.extend(adjacent_args);
                TypeIdent::Named(reference, args)
            })
            .then(tok(TokenKind::FatArrow).or(tok(TokenKind::Arrow)).ignore_then(ty.clone()).or_not())
            .map(|(input, result)| result.map_or(input.clone(), |result| TypeIdent::Function(vec![input], Box::new(result))));
        choice((wildcard, option, function, array, record, named, paren)).boxed()
    })
    .labelled("type")
}

fn module_pattern_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Pattern, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
{
    let tok = |kind| just(LayoutToken::Lex(kind));
    recursive(|pattern| {
        let name = select! {
            LayoutToken::Lex(TokenKind::LowerIdent(name)) => (name, false),
            LayoutToken::Lex(TokenKind::UpperIdent(name)) => (name, true),
        };
        let wildcard = tok(TokenKind::Wildcard).to(Pattern::Wildcard);
        let named = name
            .then(
                tok(TokenKind::Dot)
                    .ignore_then(select! {
                        LayoutToken::Lex(TokenKind::LowerIdent(name)) => name,
                        LayoutToken::Lex(TokenKind::UpperIdent(name)) => name,
                    })
                    .or_not(),
            )
            .then(
                pattern
                    .clone()
                    .separated_by(tok(TokenKind::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
                    .or_not(),
            )
            .map(|(((first, upper), second), args)| {
                let args = args.unwrap_or_default();
                match (second, upper, args.is_empty()) {
                    (Some(second), _, _) => {
                        Pattern::Constructor(Reference::Qualified(first, second), args)
                    }
                    (None, true, _) => Pattern::Constructor(Reference::Unqualified(first), args),
                    (None, false, true) => Pattern::Binding(first),
                    (None, false, false) => {
                        Pattern::Constructor(Reference::Unqualified(first), args)
                    }
                }
            });
        let tuple = pattern
            .clone()
            .separated_by(tok(TokenKind::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
            .map(|mut values| {
                if values.len() == 1 {
                    values.pop().unwrap()
                } else {
                    Pattern::Tuple(values)
                }
            });
        choice((wildcard, tuple, named)).boxed()
    })
}

fn module_parser<'tokens, I>(
    metadata: &'tokens [LayoutMeta],
) -> impl Parser<'tokens, I, Module, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
{
    let tok = |kind| just(LayoutToken::Lex(kind));
    let newline = select! { LayoutToken::Newline(indent) => indent };
    let newlines = newline.repeated().ignored();
    let separator = choice((
        newline.repeated().at_least(1).ignored(),
        tok(TokenKind::Semicolon).then_ignore(newlines).ignored(),
    ));
    let name = select! {
        LayoutToken::Lex(TokenKind::LowerIdent(name)) => name,
        LayoutToken::Lex(TokenKind::UpperIdent(name)) => name,
    };
    let reference = name
        .then(tok(TokenKind::Dot).ignore_then(name).or_not())
        .map(|(first, second)| match second {
            Some(second) => Reference::Qualified(first, second),
            None => Reference::Unqualified(first),
        });
    let position = move |span: Span| {
        metadata
            .get(span.start)
            .map(|m| m.pos.clone())
            .unwrap_or_else(|| {
                metadata
                    .last()
                    .map(|m| m.pos.clone())
                    .unwrap_or_else(|| Pos::new("<input>", 1, 1))
            })
    };

    let pragma = tok(TokenKind::Pragma)
        .ignore_then(
            name.then_ignore(newlines)
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace)),
        )
        .validate(|names, span, emitter| {
            names
                .into_iter()
                .filter_map(|name| {
                    if name == "NoBuiltin" {
                        Some(Pragma::NoBuiltin)
                    } else {
                        emitter.emit(Rich::custom(span.span(), format!("unknown pragma {name}")));
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .or_not()
        .map(Option::unwrap_or_default);

    let import_target = name
        .separated_by(tok(TokenKind::Dot))
        .at_least(1)
        .collect::<Vec<_>>()
        .then(
            choice((
                tok(TokenKind::OpenParen)
                    .ignore_then(tok(TokenKind::Ellipsis))
                    .then_ignore(tok(TokenKind::CloseParen))
                    .to(ImportType::Unqualified),
                name.separated_by(tok(TokenKind::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
                    .map(ImportType::Selective),
                tok(TokenKind::As)
                    .ignore_then(tok(TokenKind::Wildcard).to(None).or(name.map(Some)))
                    .map(ImportType::Qualified),
            ))
            .or_not(),
        )
        .map(|(module, kind)| {
            let default = ImportType::Qualified(module.last().cloned());
            (module, kind.unwrap_or(default))
        });
    let grouped_imports = newlines
        .ignore_then(
            import_target
                .clone()
                .then_ignore(tok(TokenKind::Comma).or_not())
                .then_ignore(newlines)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace));
    let import = tok(TokenKind::Import)
        .map_with(move |_, extra| position(extra.span()))
        .then(grouped_imports.or(import_target.map(|target| vec![target])))
        .map(|(pos, imports)| {
            imports
                .into_iter()
                .map(|(module, kind)| Import {
                    pos: pos.clone(),
                    module,
                    kind,
                })
                .collect::<Vec<_>>()
        });

    let expr = expression_parser(metadata);
    let ty = type_ident_parser(metadata);
    let pattern = module_pattern_parser();
    let comma = newlines
        .ignore_then(tok(TokenKind::Comma))
        .then_ignore(newlines);
    let constraint_field = name.then_ignore(tok(TokenKind::Colon)).then(ty.clone());
    let open_record_constraint = constraint_field
        .clone()
        .then_ignore(comma.clone())
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(tok(TokenKind::Ellipsis))
        .then_ignore(tok(TokenKind::Colon))
        .then(ty.clone())
        .map(|(fields, rest)| RecordConstraint {
            fields,
            rest: Some(rest),
        });
    let closed_record_constraint = constraint_field
        .separated_by(comma.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|fields| RecordConstraint { fields, rest: None });
    let record_constraint = newlines
        .ignore_then(choice((open_record_constraint, closed_record_constraint)))
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace));
    let trait_constraints = reference
        .clone()
        .separated_by(tok(TokenKind::Plus))
        .at_least(1)
        .collect::<Vec<_>>();
    let constraints = tok(TokenKind::Colon)
        .ignore_then(choice((
            record_constraint
                .then(
                    tok(TokenKind::Plus)
                        .ignore_then(reference.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(record, traits)| ConstraintSet {
                    record: Some(record),
                    traits,
                }),
            trait_constraints.map(|traits| ConstraintSet {
                record: None,
                traits,
            }),
        )))
        .or_not()
        .map(Option::unwrap_or_default);
    let type_vars = name
        .map_with(move |name, extra| (name, position(extra.span())))
        .then(constraints)
        .map(|((name, pos), constraints)| TypeVar {
            name,
            pos,
            constraints,
        })
        .separated_by(comma.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tok(TokenKind::Less), tok(TokenKind::Greater))
        .or_not()
        .map(Option::unwrap_or_default);
    let annotation = tok(TokenKind::Colon).ignore_then(ty.clone()).or_not();

    let let_decl = tok(TokenKind::Let)
        .ignore_then(tok(TokenKind::Mutable).to(Mutability::Mutable).or_not())
        .then(pattern.clone())
        .then(type_vars.clone())
        .then(annotation.clone())
        .then_ignore(tok(TokenKind::Equal))
        .then(newlines.ignore_then(expr.clone()))
        .map(
            |((((mutable, pattern), type_vars), annotation), value)| DeclarationKind::Let {
                mutability: mutable.unwrap_or(Mutability::Immutable),
                pattern,
                type_vars,
                annotation,
                value,
            },
        );

    let parameter_annotation = tok(TokenKind::Colon)
        .ignore_then(ty.clone())
        .then(tok(TokenKind::As).ignore_then(name).or_not())
        .or_not();
    let parameter = pattern
        .clone()
        .then(parameter_annotation)
        .map(|(pattern, annotation)| FunctionParam {
            pattern,
            annotation,
        });
    let params = newlines
        .ignore_then(
            parameter
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen));
    let block = block_parser(expr.clone(), metadata);
    let fun_decl = tok(TokenKind::Fun)
        .ignore_then(name)
        .then(type_vars.clone())
        .then(params.clone())
        .then(annotation.clone())
        .then(block)
        .map(
            |((((name, type_vars), params), return_type), body)| DeclarationKind::Function {
                name,
                type_vars,
                function: Function {
                    params,
                    return_type,
                    body: Box::new(body),
                },
            },
        );

    let variant = name
        .map_with(move |name, extra| (name, position(extra.span())))
        .then(
            ty.clone()
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
                .or_not(),
        )
        .map(|((name, pos), fields)| Variant {
            pos,
            name,
            fields: fields.unwrap_or_default(),
        });
    let variants = newlines
        .ignore_then(
            variant
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace));
    let shorthand = ty
        .clone()
        .separated_by(comma.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen));
    let legacy_type_vars = name
        .map_with(move |name, extra| TypeVar {
            name,
            pos: position(extra.span()),
            constraints: ConstraintSet::default(),
        })
        .repeated()
        .collect::<Vec<_>>();
    let regular_data = name
        .map_with(move |name, extra| (name, position(extra.span())))
        .then(type_vars.clone())
        .then(legacy_type_vars)
        .then(
            variants
                .map(|variants| (None, variants))
                .or(shorthand.map(|fields| (Some(fields), vec![]))),
        )
        .map(
            |((((name, pos), mut type_vars), legacy_vars), (shorthand, variants))| {
                type_vars.extend(legacy_vars);
                DeclarationKind::Data {
                    name: name.clone(),
                    type_vars,
                    variants: shorthand
                        .map_or(variants, |fields| vec![Variant { pos, name, fields }]),
                }
            },
        );
    let js_value = select! {
        LayoutToken::Lex(TokenKind::LowerIdent(name)) if name == "undefined" => JsLiteral::Undefined,
        LayoutToken::Lex(TokenKind::LowerIdent(name)) if name == "null" => JsLiteral::Null,
        LayoutToken::Lex(TokenKind::LowerIdent(name)) if name == "true" => JsLiteral::True,
        LayoutToken::Lex(TokenKind::LowerIdent(name)) if name == "false" => JsLiteral::False,
        LayoutToken::Lex(TokenKind::Integer(value)) => JsLiteral::Integer(value),
        LayoutToken::Lex(TokenKind::String(value)) => JsLiteral::String(value),
    };
    let js_variant = name
        .then_ignore(tok(TokenKind::Equal))
        .then(js_value)
        .map(|(name, value)| JsVariant { name, value });
    let js_variants = newlines
        .ignore_then(
            js_variant
                .separated_by(comma.clone())
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace));
    let js_data = tok(TokenKind::JsFfi)
        .ignore_then(name)
        .then(js_variants)
        .map(|(name, variants)| DeclarationKind::JsData { name, variants });
    let data_decl = tok(TokenKind::Data).ignore_then(choice((js_data, regular_data)));

    let alias_params = name
        .separated_by(comma.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tok(TokenKind::Less), tok(TokenKind::Greater))
        .or(name.repeated().collect::<Vec<_>>());
    let alias_decl = tok(TokenKind::Type)
        .ignore_then(name)
        .then(alias_params)
        .then_ignore(tok(TokenKind::Equal))
        .then(ty.clone())
        .map(|((name, params), ty)| DeclarationKind::TypeAlias { name, params, ty });
    let declare_decl = tok(TokenKind::Declare)
        .ignore_then(name)
        .then(type_vars.clone())
        .then_ignore(tok(TokenKind::Colon))
        .then(ty.clone())
        .map(|((name, type_vars), ty)| DeclarationKind::Declare {
            name,
            type_vars,
            ty,
        });
    let exception_decl = tok(TokenKind::Exception)
        .ignore_then(name)
        .then(ty)
        .map(|(name, ty)| DeclarationKind::Exception { name, ty });
    let export_import = tok(TokenKind::Import)
        .ignore_then(name)
        .map(DeclarationKind::ExportImport);

    let trait_method_traditional = tok(TokenKind::Colon)
        .ignore_then(type_ident_parser(metadata))
        .then(tok(TokenKind::Equal).ignore_then(expr.clone()).or_not());
    let trait_default = params
        .clone()
        .then_ignore(tok(TokenKind::Colon))
        .then(type_ident_parser(metadata))
        .then(block_parser(expr.clone(), metadata))
        .validate(move |((params, result), body), extra, emitter| {
            let mut args = Vec::new();
            for param in &params {
                match &param.annotation {
                    Some((ty, _)) => args.push(ty.clone()),
                    None => emitter.emit(Rich::custom(
                        extra.span(),
                        "default trait method parameters require annotations",
                    )),
                }
            }
            let function_type = TypeIdent::Function(args, Box::new(result.clone()));
            let function = Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Function(Function {
                    params,
                    return_type: Some(result),
                    body: Box::new(body),
                }),
            };
            (function_type, Some(function))
        });
    let trait_signature = type_ident_parser(metadata)
        .separated_by(comma.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
        .then_ignore(tok(TokenKind::Colon))
        .then(type_ident_parser(metadata))
        .map(|(args, result)| (TypeIdent::Function(args, Box::new(result)), None));
    let trait_method = name
        .map_with(move |name, extra| (name, position(extra.span())))
        .then(choice((
            trait_default,
            trait_method_traditional,
            trait_signature,
        )))
        .map(|((name, pos), (ty, default))| TraitMethod {
            name,
            pos,
            ty,
            default,
        });
    let trait_methods = newlines
        .ignore_then(
            trait_method
                .then_ignore(
                    choice((
                        newline.repeated().at_least(1).ignored(),
                        tok(TokenKind::Semicolon).then_ignore(newlines).ignored(),
                    ))
                    .or_not(),
                )
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace));
    let trait_decl = tok(TokenKind::Trait)
        .ignore_then(name)
        .then(trait_methods)
        .map(|(name, methods)| DeclarationKind::Trait { name, methods });

    let impl_method_value = params
        .clone()
        .then(annotation.clone())
        .then(block_parser(expr.clone(), metadata))
        .map_with(move |((params, return_type), body), extra| Expression {
            pos: position(extra.span()),
            kind: ExpressionKind::Function(Function {
                params,
                return_type,
                body: Box::new(body),
            }),
        });
    let impl_method = name
        .then(impl_method_value.or(tok(TokenKind::Equal).ignore_then(expr.clone())))
        .map(|(name, value)| ParsedImplEntry::Method(name, value));
    let impl_field = tok(TokenKind::For)
        .map_with(move |_, extra| position(extra.span()))
        .then(name)
        .then(block_parser(expr.clone(), metadata))
        .map(|((pos, name), body)| ParsedImplEntry::Field(pos, name, body));
    let impl_methods = newlines
        .ignore_then(
            choice((impl_field, impl_method))
                .then_ignore(
                    choice((
                        newline.repeated().at_least(1).ignored(),
                        tok(TokenKind::Semicolon).then_ignore(newlines).ignored(),
                    ))
                    .or_not(),
                )
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace));
    let function_impl = reference
        .clone()
        .separated_by(comma.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen))
        .then_ignore(tok(TokenKind::FatArrow))
        .then_ignore(reference.clone())
        .map(|args| ImplType::Function { arity: args.len() });
    let record_impl = tok(TokenKind::Ellipsis)
        .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace))
        .to(ImplType::Record {
            field_function: Expression {
                pos: Pos::new("<generated>", 1, 1),
                kind: ExpressionKind::Error,
            },
        });
    let nominal_impl = reference
        .clone()
        .then(type_vars.clone())
        .map(|(name, type_vars)| ImplType::Nominal { name, type_vars });
    let impl_header = choice((function_impl, record_impl, nominal_impl));
    let impl_decl = tok(TokenKind::Impl)
        .ignore_then(reference)
        .then(impl_header)
        .then(impl_methods)
        .validate(|((trait_name, mut impl_type), entries), span, emitter| {
            let mut methods = Vec::new();
            let mut fields = Vec::new();
            for entry in entries {
                match entry {
                    ParsedImplEntry::Method(name, value) => methods.push((name, value)),
                    ParsedImplEntry::Field(pos, name, body) => fields.push((pos, name, body)),
                }
            }
            if matches!(impl_type, ImplType::Record { .. }) {
                if fields.len() == 1 {
                    let (pos, name, body) = fields.pop().unwrap();
                    impl_type = ImplType::Record {
                        field_function: Expression {
                            pos,
                            kind: ExpressionKind::Function(Function {
                                params: vec![FunctionParam {
                                    pattern: Pattern::Binding(name),
                                    annotation: None,
                                }],
                                return_type: None,
                                body: Box::new(body),
                            }),
                        },
                    };
                } else {
                    emitter.emit(Rich::custom(
                        span.span(),
                        "record impl requires exactly one field transformer",
                    ));
                }
            } else if !fields.is_empty() {
                emitter.emit(Rich::custom(
                    span.span(),
                    "only record impls support field transformers",
                ));
            }
            DeclarationKind::Impl {
                trait_name,
                impl_type,
                context: vec![],
                methods,
            }
        });
    let expression_decl = expr.map(|value| DeclarationKind::Let {
        mutability: Mutability::Immutable,
        pattern: Pattern::Wildcard,
        type_vars: vec![],
        annotation: None,
        value,
    });

    let declaration_kind = choice((
        fun_decl,
        let_decl,
        data_decl,
        alias_decl,
        declare_decl,
        trait_decl,
        impl_decl,
        exception_decl,
        export_import,
        expression_decl,
    ));
    let declaration = tok(TokenKind::Export)
        .to(true)
        .or_not()
        .map(Option::unwrap_or_default)
        .then(declaration_kind)
        .map_with(move |(exported, kind), extra| Declaration {
            exported,
            pos: position(extra.span()),
            kind,
        });

    newlines
        .ignore_then(pragma)
        .then_ignore(newlines)
        .then(
            import
                .then_ignore(separator.clone().or_not())
                .repeated()
                .collect::<Vec<_>>()
                .map(|groups| groups.into_iter().flatten().collect::<Vec<_>>()),
        )
        .then(
            declaration
                .then_ignore(separator.or_not())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(newlines)
        .map(|((pragmas, imports), declarations)| Module {
            pragmas,
            imports,
            declarations,
        })
}

fn expression_parser<'tokens, I>(
    metadata: &'tokens [LayoutMeta],
) -> impl Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
{
    let tok = |kind| just(LayoutToken::Lex(kind));
    let newline = select! { LayoutToken::Newline(indent) => indent };
    let newlines = newline.repeated().ignored();

    let position = move |span: Span| {
        metadata
            .get(span.start)
            .map(|meta| meta.pos.clone())
            .unwrap_or_else(|| metadata.last().unwrap().pos.clone())
    };

    let pattern = module_pattern_parser();
    let ty = type_ident_parser(metadata);

    recursive(|expr| {
        let literal = select! {
            LayoutToken::Lex(TokenKind::Integer(value)) => Literal::Integer(value),
            LayoutToken::Lex(TokenKind::String(value)) => Literal::String(value),
        }
        .map_with(move |literal, extra| Expression {
            pos: position(extra.span()),
            kind: ExpressionKind::Literal(literal),
        });

        let identifier_name = select! {
            LayoutToken::Lex(TokenKind::LowerIdent(name)) => name,
            LayoutToken::Lex(TokenKind::UpperIdent(name)) => name,
        };
        let identifier = identifier_name
            .then(choice((
                tok(TokenKind::FatArrow)
                    .ignore_then(expr.clone())
                    .map(IdentifierTail::Lambda),
                tok(TokenKind::ColonColon)
                    .ignore_then(identifier_name)
                    .map(IdentifierTail::TypeLookup),
                empty().to(IdentifierTail::Plain),
            )))
            .map_with(move |(name, tail), extra| {
                let pos = position(extra.span());
                match tail {
                    IdentifierTail::Lambda(body) => Expression {
                        pos,
                        kind: ExpressionKind::Function(Function {
                            params: vec![FunctionParam {
                                pattern: Pattern::Binding(name),
                                annotation: None,
                            }],
                            return_type: None,
                            body: Box::new(body),
                        }),
                    },
                    IdentifierTail::TypeLookup(method) => Expression {
                        pos,
                        kind: ExpressionKind::TypeLookup(Reference::Unqualified(name), method),
                    },
                    IdentifierTail::Plain => Expression {
                        pos,
                        kind: ExpressionKind::Identifier(Reference::Unqualified(name)),
                    },
                }
            });

        let comma = newlines
            .ignore_then(tok(TokenKind::Comma))
            .then_ignore(newlines);
        let expression_list = newlines
            .ignore_then(
                expr.clone()
                    .separated_by(comma.clone())
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(newlines);

        let delimited_expressions = tok(TokenKind::OpenParen)
            .map_with(move |_, extra| {
                let span: Span = extra.span();
                (
                    metadata[span.start].pos.clone(),
                    metadata[span.start].line_start,
                )
            })
            .then(expression_list.clone())
            .then(tok(TokenKind::CloseParen).map_with(move |_, extra| {
                let span: Span = extra.span();
                metadata[span.start].clone()
            }))
            .validate(
                |(((open_pos, open_indent), expressions), close), span, emitter| {
                    for expression in &expressions {
                        if expression.pos.line > open_pos.line
                            && expression.pos.column <= open_indent
                        {
                            emitter.emit(Rich::custom(
                                span.span(),
                                "multiline parenthesized content must be indented",
                            ));
                            break;
                        }
                    }
                    if close.pos.line > open_pos.line && close.pos.column < open_indent {
                        emitter.emit(Rich::custom(
                            span.span(),
                            "closing parenthesis is dedented past its opener",
                        ));
                    }
                    expressions
                },
            );

        let parenthesized_elements = delimited_expressions.clone();
        let parenthesized = parenthesized_elements
            .then(choice((
                tok(TokenKind::FatArrow).ignore_then(expr.clone()).map(Some),
                empty().to(None),
            )))
            .validate(move |(mut elements, body), extra, emitter| {
                let pos = position(extra.span());
                let Some(body) = body else {
                    return match elements.len() {
                        0 => Expression {
                            pos,
                            kind: ExpressionKind::Literal(Literal::Unit),
                        },
                        1 => elements.pop().unwrap(),
                        _ => Expression {
                            pos,
                            kind: ExpressionKind::Tuple(elements),
                        },
                    };
                };
                let params = elements
                    .into_iter()
                    .map(|element| {
                        let pattern = match element.kind {
                            ExpressionKind::Identifier(Reference::Unqualified(name)) => {
                                Pattern::Binding(name)
                            }
                            ExpressionKind::Literal(Literal::Unit) => Pattern::Tuple(vec![]),
                            _ => {
                                emitter.emit(Rich::custom(
                                    extra.span(),
                                    "lambda parameter is not a pattern",
                                ));
                                Pattern::Wildcard
                            }
                        };
                        FunctionParam {
                            pattern,
                            annotation: None,
                        }
                    })
                    .collect();
                Expression {
                    pos,
                    kind: ExpressionKind::Function(Function {
                        params,
                        return_type: None,
                        body: Box::new(body),
                    }),
                }
            });

        let block = block_parser(expr.clone(), metadata);

        let parameter_annotation = tok(TokenKind::Colon)
            .ignore_then(ty.clone())
            .then(tok(TokenKind::As).ignore_then(identifier_name).or_not())
            .map(|(ty, alias)| (ty, alias));
        let parameter =
            pattern
                .clone()
                .then(parameter_annotation.or_not())
                .map(|(pattern, annotation)| FunctionParam {
                    pattern,
                    annotation,
                });
        let function = tok(TokenKind::Fun)
            .ignore_then(
                newlines
                    .ignore_then(
                        parameter
                            .separated_by(comma.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(newlines)
                    .delimited_by(tok(TokenKind::OpenParen), tok(TokenKind::CloseParen)),
            )
            .then(tok(TokenKind::Colon).ignore_then(ty.clone()).or_not())
            .then(block.clone())
            .map_with(move |((params, return_type), body), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Function(Function {
                    params,
                    return_type,
                    body: Box::new(body),
                }),
            });

        let array = tok(TokenKind::Mutable)
            .to(Mutability::Mutable)
            .or_not()
            .then(
                newlines
                    .ignore_then(
                        expr.clone()
                            .separated_by(comma.clone())
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(newlines)
                    .delimited_by(tok(TokenKind::OpenBracket), tok(TokenKind::CloseBracket)),
            )
            .map_with(move |(mutable, values), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Array(mutable.unwrap_or(Mutability::Immutable), values),
            });

        let record_field = tok(TokenKind::Mutable)
            .to(Mutability::Mutable)
            .or_not()
            .then(identifier_name)
            .then_ignore(tok(TokenKind::Colon))
            .then(expr.clone())
            .map(|((mutable, name), value)| {
                (name, (mutable.unwrap_or(Mutability::Immutable), value))
            });
        let record = newlines
            .ignore_then(
                record_field
                    .separated_by(comma.clone())
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(newlines)
            .delimited_by(tok(TokenKind::OpenBrace), tok(TokenKind::CloseBrace))
            .map_with(move |fields, extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Record(fields.into_iter().collect()),
            });

        let let_expression = tok(TokenKind::Let)
            .ignore_then(tok(TokenKind::Mutable).to(Mutability::Mutable).or_not())
            .then(pattern.clone())
            .then(tok(TokenKind::Colon).ignore_then(ty.clone()).or_not())
            .then_ignore(tok(TokenKind::Equal))
            .then(newlines.ignore_then(expr.clone()))
            .map_with(
                move |(((mutable, pattern), annotation), value), extra| Expression {
                    pos: position(extra.span()),
                    kind: ExpressionKind::Let {
                        mutability: mutable.unwrap_or(Mutability::Immutable),
                        pattern,
                        type_vars: vec![],
                        annotation,
                        value: Box::new(value),
                    },
                },
            );

        let while_expression = tok(TokenKind::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map_with(move |(condition, body), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::While(Box::new(condition), Box::new(body)),
            });

        let for_expression = tok(TokenKind::For)
            .ignore_then(pattern.clone())
            .then_ignore(tok(TokenKind::In))
            .then(expr.clone())
            .then(block.clone())
            .map_with(move |((pattern, over), body), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::For(pattern, Box::new(over), Box::new(body)),
            });

        let return_expression = tok(TokenKind::Return)
            .ignore_then(expr.clone().or_not())
            .map_with(move |value, extra| {
                let pos = position(extra.span());
                Expression {
                    pos: pos.clone(),
                    kind: ExpressionKind::Return(Box::new(value.unwrap_or(Expression {
                        pos,
                        kind: ExpressionKind::Literal(Literal::Unit),
                    }))),
                }
            });

        let reference = identifier_name
            .then(tok(TokenKind::Dot).ignore_then(identifier_name).or_not())
            .map(|(first, second)| match second {
                Some(second) => Reference::Qualified(first, second),
                None => Reference::Unqualified(first),
            });
        let throw_expression = tok(TokenKind::Throw)
            .ignore_then(reference.clone())
            .then(expr.clone())
            .map_with(move |(exception, value), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Throw(exception, Box::new(value)),
            });
        let catch_binding = tok(TokenKind::Wildcard)
            .to(CatchBinding::Wildcard)
            .or(reference
                .then(pattern.clone())
                .map(|(exception, pattern)| CatchBinding::CruxException(exception, pattern)));
        let try_expression = tok(TokenKind::Try)
            .ignore_then(block.clone())
            .then_ignore(newlines)
            .then_ignore(tok(TokenKind::Catch))
            .then(catch_binding)
            .then(block.clone())
            .map_with(move |((body, binding), handler), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::TryCatch(Box::new(body), binding, Box::new(handler)),
            });

        let wildcard_lambda = tok(TokenKind::Wildcard)
            .ignore_then(tok(TokenKind::FatArrow))
            .ignore_then(expr.clone())
            .map_with(move |body, extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Function(Function {
                    params: vec![FunctionParam {
                        pattern: Pattern::Wildcard,
                        annotation: None,
                    }],
                    return_type: None,
                    body: Box::new(body),
                }),
            });

        let if_expression = tok(TokenKind::If)
            .map_with(move |_, extra| {
                let span: Span = extra.span();
                (
                    metadata[span.start].pos.clone(),
                    metadata[span.start].line_start,
                )
            })
            .then_ignore(newlines)
            .then(expr.clone())
            .then(
                newlines
                    .ignore_then(tok(TokenKind::Then))
                    .ignore_then(newlines)
                    .ignore_then(expr.clone())
                    .then_ignore(newlines.ignore_then(tok(TokenKind::Else)))
                    .then(newlines.ignore_then(expr.clone()))
                    .map(|(yes, no)| (yes, Some(no)))
                    .or(block.clone().then(
                        newlines
                            .ignore_then(tok(TokenKind::Else))
                            .ignore_then(newlines)
                            .ignore_then(expr.clone())
                            .or_not(),
                    )),
            )
            .validate(
                move |(((pos, if_indent), condition), (yes, no)), extra, emitter| {
                    if condition.pos.line > pos.line && condition.pos.column <= if_indent {
                        emitter.emit(Rich::custom(
                            extra.span(),
                            "multiline if condition must be indented",
                        ));
                    }
                    Expression {
                        pos: pos.clone(),
                        kind: ExpressionKind::If(
                            Box::new(condition),
                            Box::new(yes),
                            Box::new(no.unwrap_or(Expression {
                                pos,
                                kind: ExpressionKind::Literal(Literal::Unit),
                            })),
                        ),
                    }
                },
            );

        let match_expression = tok(TokenKind::Match)
            .ignore_then(expr.clone())
            .then(match_cases(expr.clone(), pattern.clone(), metadata))
            .map_with(move |(subject, cases), extra| Expression {
                pos: position(extra.span()),
                kind: ExpressionKind::Match(Box::new(subject), cases),
            });

        let atom = choice((
            if_expression,
            match_expression,
            let_expression,
            while_expression,
            for_expression,
            return_expression,
            throw_expression,
            try_expression,
            wildcard_lambda,
            function,
            array,
            record,
            literal,
            parenthesized,
            identifier,
            block,
        ))
        .boxed();

        let arguments = delimited_expressions.clone().map(Postfix::Call);
        let lookup = tok(TokenKind::Dot)
            .ignore_then(identifier_name)
            .map(Postfix::Lookup);
        let method = tok(TokenKind::Arrow)
            .ignore_then(identifier_name)
            .then(delimited_expressions.clone())
            .map(|(name, args)| Postfix::Method(name, args));
        let application = atom.clone().foldl(
            choice((arguments, method, lookup)).repeated(),
            |value, postfix| {
                let pos = value.pos.clone();
                match postfix {
                    Postfix::Call(arguments) => Expression {
                        pos,
                        kind: ExpressionKind::Apply(Box::new(value), arguments),
                    },
                    Postfix::Lookup(name) => Expression {
                        pos,
                        kind: ExpressionKind::Lookup(Box::new(value), name),
                    },
                    Postfix::Method(name, arguments) => Expression {
                        pos,
                        kind: ExpressionKind::MethodApply(Box::new(value), name, arguments),
                    },
                }
            },
        );

        let unary = tok(TokenKind::Minus)
            .to(())
            .or_not()
            .then(application)
            .map_with(move |(minus, expression), extra| {
                if minus.is_some() {
                    Expression {
                        pos: position(extra.span()),
                        kind: ExpressionKind::Unary(UnaryOp::Negate, Box::new(expression)),
                    }
                } else {
                    expression
                }
            });

        let cast = unary
            .then(tok(TokenKind::As).ignore_then(ty.clone()).or_not())
            .map(|(value, ty)| match ty {
                Some(ty) => Expression {
                    pos: value.pos.clone(),
                    kind: ExpressionKind::As(Box::new(value), ty),
                },
                None => value,
            });

        let product = binary_layer(
            cast,
            choice((
                tok(TokenKind::Star).to(BinaryOp::Multiply),
                tok(TokenKind::Slash).to(BinaryOp::Divide),
            )),
        );
        let sum = binary_layer(
            product,
            choice((
                tok(TokenKind::Plus).to(BinaryOp::Add),
                tok(TokenKind::Minus).to(BinaryOp::Subtract),
            )),
        );
        let relation = binary_layer(
            sum,
            choice((
                tok(TokenKind::DoubleEqual).to(BinaryOp::Equal),
                tok(TokenKind::NotEqual).to(BinaryOp::NotEqual),
                tok(TokenKind::LessEqual).to(BinaryOp::LessEqual),
                tok(TokenKind::GreaterEqual).to(BinaryOp::GreaterEqual),
                tok(TokenKind::Less).to(BinaryOp::Less),
                tok(TokenKind::Greater).to(BinaryOp::Greater),
            )),
        );
        let boolean = binary_layer(
            relation,
            choice((
                tok(TokenKind::AndAnd).to(BinaryOp::And),
                tok(TokenKind::OrOr).to(BinaryOp::Or),
            )),
        );
        let assignment = boolean
            .clone()
            .then(tok(TokenKind::Equal).ignore_then(expr.clone()).or_not())
            .map(|(target, value)| match value {
                Some(value) => Expression {
                    pos: target.pos.clone(),
                    kind: ExpressionKind::Assign(Box::new(target), Box::new(value)),
                },
                None => target,
            });
        assignment
            .foldl(
                tok(TokenKind::Semicolon)
                    .ignore_then(expr.clone())
                    .repeated(),
                |left, right| Expression {
                    pos: left.pos.clone(),
                    kind: ExpressionKind::Sequence(Box::new(left), Box::new(right)),
                },
            )
            .then_ignore(tok(TokenKind::Semicolon).or_not())
            .labelled("expression")
            .boxed()
    })
}

fn binary_layer<'tokens, I, Term, Op>(
    term: Term,
    operator: Op,
) -> impl Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
    Term: Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone,
    Op: Parser<'tokens, I, BinaryOp, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone,
{
    term.clone()
        .foldl(operator.then(term).repeated(), |left, (op, right)| {
            Expression {
                pos: right.pos.clone(),
                kind: ExpressionKind::Binary(op, Box::new(left), Box::new(right)),
            }
        })
}

fn block_parser<'tokens, I, P>(
    expression: P,
    metadata: &'tokens [LayoutMeta],
) -> impl Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
    P: Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LayoutToken, Span>>>
        + Clone
        + 'tokens,
{
    let open = just(LayoutToken::Lex(TokenKind::OpenBrace)).map_with(move |_, extra| {
        let span: Span = extra.span();
        (
            metadata[span.start].pos.clone(),
            metadata[span.start].line_start,
        )
    });
    let close = just(LayoutToken::Lex(TokenKind::CloseBrace));
    let newline = select! { LayoutToken::Newline(indent) => indent };
    let line_start = newline.map_with(|indent, extra| (indent, extra.span()));

    let stop = newline.ignored().or(close.clone().ignored());
    let recovered = expression
        .clone()
        .then_ignore(just(LayoutToken::Lex(TokenKind::Semicolon)).or_not())
        .recover_with(via_parser(
            any()
                .and_is(stop.not())
                .repeated()
                .at_least(1)
                .ignored()
                .map_with(move |_, extra| {
                    let span: Span = extra.span();
                    error_expression(metadata[span.start].pos.clone())
                }),
        ));

    let multiline = line_start
        .then(recovered)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(newline.repeated())
        .validate(|lines, _, emitter| {
            if let Some(((expected, _), _)) = lines.first() {
                for (indent, span) in lines.iter().map(|((indent, span), _)| (indent, span)) {
                    if indent != expected {
                        emitter.emit(Rich::custom(
                            *span,
                            format!("block line must begin in column {expected}, found {indent}"),
                        ));
                    }
                }
            }
            lines
                .into_iter()
                .map(|(_, expression)| expression)
                .collect::<Vec<_>>()
        });

    let inline = expression
        .clone()
        .separated_by(just(LayoutToken::Lex(TokenKind::Semicolon)))
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<_>>();

    let empty_block = newline.repeated().to(Vec::new());

    open.then(choice((multiline, inline, empty_block)))
        .then_ignore(close)
        .validate(|((pos, opener_indent), expressions), span, emitter| {
            if let Some(first) = expressions.first() {
                if first.pos.line > pos.line && first.pos.column <= opener_indent {
                    emitter.emit(Rich::custom(span.span(), "block contents must be indented"));
                }
            }
            sequence(pos, expressions)
        })
}

fn match_cases<'tokens, I, E, Pat>(
    expression: E,
    pattern: Pat,
    metadata: &'tokens [LayoutMeta],
) -> impl Parser<'tokens, I, Vec<MatchCase>, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = LayoutToken, Span = Span>,
    E: Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, LayoutToken, Span>>>
        + Clone
        + 'tokens,
    Pat:
        Parser<'tokens, I, Pattern, extra::Err<Rich<'tokens, LayoutToken, Span>>> + Clone + 'tokens,
{
    let open = just(LayoutToken::Lex(TokenKind::OpenBrace)).map_with(move |_, extra| {
        let span: Span = extra.span();
        metadata[span.start].line_start
    });
    let close = just(LayoutToken::Lex(TokenKind::CloseBrace));
    let newline = select! { LayoutToken::Newline(indent) => indent };
    let line_start = newline.map_with(|indent, extra| (indent, extra.span()));
    let case = line_start
        .then(pattern)
        .then_ignore(just(LayoutToken::Lex(TokenKind::FatArrow)))
        .then(expression)
        .map(|(((indent, span), pattern), body)| ((indent, span), MatchCase { pattern, body }));

    open.then(case.repeated().collect::<Vec<_>>())
        .then_ignore(newline.repeated())
        .then_ignore(close)
        .validate(|(opener_indent, cases), _, emitter| {
            if let Some(((expected, _), _)) = cases.first() {
                if expected <= &opener_indent {
                    emitter.emit(Rich::custom(cases[0].0 .1, "match cases must be indented"));
                }
                for ((indent, span), _) in &cases {
                    if indent != expected {
                        emitter.emit(Rich::custom(
                            *span,
                            format!("match case must begin in column {expected}, found {indent}"),
                        ));
                    }
                }
            }
            cases.into_iter().map(|(_, case)| case).collect()
        })
}

fn sequence(pos: Pos, expressions: Vec<Expression>) -> Expression {
    let mut iter = expressions.into_iter();
    let Some(mut result) = iter.next() else {
        return Expression {
            pos,
            kind: ExpressionKind::Literal(Literal::Unit),
        };
    };
    for next in iter {
        result = Expression {
            pos: pos.clone(),
            kind: ExpressionKind::Sequence(Box::new(result), Box::new(next)),
        };
    }
    result
}

fn error_expression(pos: Pos) -> Expression {
    Expression {
        pos,
        kind: ExpressionKind::Error,
    }
}
