//! The Crux compiler, reimplemented in Rust.
//!
//! The crate exposes the source model, bounded Chumsky parser, type checker,
//! JavaScript backend, and an import-aware project compiler.

pub mod ast;
pub mod chumsky_parser;
pub mod codegen;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod typecheck;

pub use lexer::{lex, LexError, Token, TokenKind};
pub use parser::ParseError;

/// Parse a complete source file with the production Chumsky grammar.
pub fn parse(file: &str, source: &str) -> Result<ast::Module, ParseError> {
    let result = chumsky_parser::parse_module(file, source);
    if let Some(diagnostic) = result.diagnostics.into_iter().next() {
        return Err(ParseError {
            pos: diagnostic.pos,
            message: diagnostic.message,
        });
    }
    result.output.ok_or_else(|| ParseError {
        pos: ast::Pos::new(file, 1, 1),
        message: "parser produced neither a module nor a diagnostic".into(),
    })
}

/// Transitional entry point retained while downstream users migrate AST
/// comparisons to the Chumsky parser's recovery-aware behavior.
pub use parser::parse as parse_compat;
