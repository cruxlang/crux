use crate::ast::Pos;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Integer(i128),
    String(String),
    UpperIdent(String),
    LowerIdent(String),
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Question,
    Semicolon,
    ColonColon,
    Colon,
    Comma,
    Equal,
    Dot,
    Arrow,
    FatArrow,
    Ellipsis,
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    DoubleEqual,
    NotEqual,
    AndAnd,
    OrOr,
    Wildcard,
    As,
    Pragma,
    Import,
    Export,
    Fun,
    Let,
    Data,
    Declare,
    Exception,
    Throw,
    Try,
    Catch,
    JsFfi,
    Type,
    Match,
    If,
    Then,
    Else,
    While,
    For,
    In,
    Do,
    Return,
    Mutable,
    Trait,
    Impl,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub pos: Pos,
    /// Column of the first non-whitespace token on this physical line.
    pub line_start: usize,
    pub kind: TokenKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LexError {
    pub pos: Pos,
    pub message: String,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.pos.file, self.pos.line, self.pos.column, self.message
        )
    }
}
impl std::error::Error for LexError {}

fn keyword(s: &str) -> Option<TokenKind> {
    use TokenKind::*;
    Some(match s {
        "_" => Wildcard,
        "as" => As,
        "pragma" => Pragma,
        "import" => Import,
        "export" => Export,
        "fun" => Fun,
        "let" => Let,
        "data" => Data,
        "declare" => Declare,
        "exception" => Exception,
        "throw" => Throw,
        "try" => Try,
        "catch" => Catch,
        "jsffi" => JsFfi,
        "type" => Type,
        "match" => Match,
        "if" => If,
        "then" => Then,
        "else" => Else,
        "while" => While,
        "for" => For,
        "in" => In,
        "do" => Do,
        "return" => Return,
        "mutable" => Mutable,
        "trait" => Trait,
        "impl" => Impl,
        _ => return None,
    })
}

/// Tokenize Crux source while retaining the layout information needed by the parser.
pub fn lex(file: &str, source: &str) -> Result<Vec<Token>, LexError> {
    let chars: Vec<char> = source.chars().collect();
    let mut out = Vec::new();
    let (mut i, mut line, mut column) = (0, 1, 1);
    let mut line_start = 1;
    let mut first_on_line = true;

    while i < chars.len() {
        match chars[i] {
            c if c.is_whitespace() => {
                if c == '\n' {
                    line += 1;
                    column = 1;
                    first_on_line = true;
                } else if c == '\r' {
                    if chars.get(i + 1) == Some(&'\n') {
                        i += 1;
                    }
                    line += 1;
                    column = 1;
                    first_on_line = true;
                } else if c == '\t' {
                    column += 8 - ((column - 1) % 8);
                } else {
                    column += 1;
                }
                i += 1;
                continue;
            }
            '/' if chars.get(i + 1) == Some(&'/') => {
                i += 2;
                column += 2;
                while i < chars.len() && chars[i] != '\n' && chars[i] != '\r' {
                    i += 1;
                    column += 1;
                }
                continue;
            }
            '/' if chars.get(i + 1) == Some(&'*') => {
                let start = Pos::new(file, line, column);
                i += 2;
                column += 2;
                let mut depth = 1;
                while i < chars.len() && depth > 0 {
                    if chars[i] == '/' && chars.get(i + 1) == Some(&'*') {
                        depth += 1;
                        i += 2;
                        column += 2;
                    } else if chars[i] == '*' && chars.get(i + 1) == Some(&'/') {
                        depth -= 1;
                        i += 2;
                        column += 2;
                    } else if chars[i] == '\n' {
                        i += 1;
                        line += 1;
                        column = 1;
                        first_on_line = true;
                    } else if chars[i] == '\r' {
                        i += 1;
                        if chars.get(i) == Some(&'\n') {
                            i += 1;
                        }
                        line += 1;
                        column = 1;
                        first_on_line = true;
                    } else {
                        i += 1;
                        column += 1;
                    }
                }
                if depth != 0 {
                    return Err(LexError {
                        pos: start,
                        message: "unterminated block comment".into(),
                    });
                }
                continue;
            }
            _ => {}
        }

        if first_on_line {
            line_start = column;
            first_on_line = false;
        }
        let pos = Pos::new(file, line, column);
        let start = i;
        let kind = if chars[i].is_ascii_digit()
            || (chars[i] == '-' && chars.get(i + 1).is_some_and(char::is_ascii_digit))
        {
            if chars[i] == '-' {
                i += 1;
                column += 1;
            }
            while i < chars.len() && chars[i].is_ascii_digit() {
                i += 1;
                column += 1;
            }
            let raw: String = chars[start..i].iter().collect();
            TokenKind::Integer(raw.parse().map_err(|_| LexError {
                pos: pos.clone(),
                message: "integer literal is out of range".into(),
            })?)
        } else if chars[i] == '"' {
            i += 1;
            column += 1;
            let mut value = String::new();
            let mut closed = false;
            while i < chars.len() {
                let c = chars[i];
                if c == '"' {
                    i += 1;
                    column += 1;
                    closed = true;
                    break;
                }
                if c == '\n' || c == '\r' {
                    break;
                }
                if c != '\\' {
                    value.push(c);
                    i += 1;
                    column += 1;
                    continue;
                }
                i += 1;
                column += 1;
                let esc = chars.get(i).copied().ok_or_else(|| LexError {
                    pos: pos.clone(),
                    message: "unterminated escape".into(),
                })?;
                i += 1;
                column += 1;
                match esc {
                    '0' => value.push('\0'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    '?' => value.push('?'),
                    '\'' => value.push('\''),
                    'a' => value.push('\u{7}'),
                    'b' => value.push('\u{8}'),
                    'f' => value.push('\u{c}'),
                    'r' => value.push('\r'),
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'v' => value.push('\u{b}'),
                    'x' | 'u' | 'U' => {
                        let n = match esc {
                            'x' => 2,
                            'u' => 4,
                            _ => 8,
                        };
                        if i + n > chars.len() {
                            return Err(LexError {
                                pos: pos.clone(),
                                message: "incomplete unicode escape".into(),
                            });
                        }
                        let raw: String = chars[i..i + n].iter().collect();
                        if !raw.chars().all(|x| x.is_ascii_hexdigit()) {
                            return Err(LexError {
                                pos: pos.clone(),
                                message: "invalid unicode escape".into(),
                            });
                        }
                        let cp = u32::from_str_radix(&raw, 16).unwrap();
                        value.push(char::from_u32(cp).ok_or_else(|| LexError {
                            pos: pos.clone(),
                            message: "invalid unicode scalar value".into(),
                        })?);
                        i += n;
                        column += n;
                    }
                    _ => {
                        return Err(LexError {
                            pos: pos.clone(),
                            message: format!("unknown escape \\{esc}"),
                        })
                    }
                }
            }
            if !closed {
                return Err(LexError {
                    pos,
                    message: "unterminated string literal".into(),
                });
            }
            TokenKind::String(value)
        } else if chars[i].is_alphabetic() || chars[i] == '_' {
            i += 1;
            column += 1;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
                column += 1;
            }
            let name: String = chars[start..i].iter().collect();
            keyword(&name).unwrap_or_else(|| {
                if name.chars().next().unwrap().is_uppercase() {
                    TokenKind::UpperIdent(name)
                } else {
                    TokenKind::LowerIdent(name)
                }
            })
        } else {
            let rest: String = chars[i..chars.len().min(i + 3)].iter().collect();
            let (kind, n) = if rest.starts_with("...") {
                (TokenKind::Ellipsis, 3)
            } else if rest.starts_with("=>") {
                (TokenKind::FatArrow, 2)
            } else if rest.starts_with("->") {
                (TokenKind::Arrow, 2)
            } else if rest.starts_with("==") {
                (TokenKind::DoubleEqual, 2)
            } else if rest.starts_with("!=") {
                (TokenKind::NotEqual, 2)
            } else if rest.starts_with("<=") {
                (TokenKind::LessEqual, 2)
            } else if rest.starts_with(">=") {
                (TokenKind::GreaterEqual, 2)
            } else if rest.starts_with("&&") {
                (TokenKind::AndAnd, 2)
            } else if rest.starts_with("||") {
                (TokenKind::OrOr, 2)
            } else if rest.starts_with("::") {
                (TokenKind::ColonColon, 2)
            } else {
                (
                    match chars[i] {
                        '?' => TokenKind::Question,
                        ';' => TokenKind::Semicolon,
                        ':' => TokenKind::Colon,
                        ',' => TokenKind::Comma,
                        '=' => TokenKind::Equal,
                        '.' => TokenKind::Dot,
                        '(' => TokenKind::OpenParen,
                        ')' => TokenKind::CloseParen,
                        '{' => TokenKind::OpenBrace,
                        '}' => TokenKind::CloseBrace,
                        '[' => TokenKind::OpenBracket,
                        ']' => TokenKind::CloseBracket,
                        '+' => TokenKind::Plus,
                        '-' => TokenKind::Minus,
                        '*' => TokenKind::Star,
                        '/' => TokenKind::Slash,
                        '<' => TokenKind::Less,
                        '>' => TokenKind::Greater,
                        c => {
                            return Err(LexError {
                                pos,
                                message: format!("unexpected character {c:?}"),
                            })
                        }
                    },
                    1,
                )
            };
            i += n;
            column += n;
            kind
        };
        out.push(Token {
            pos,
            line_start,
            kind,
        });
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_comments_and_positions() {
        let tokens = lex("x.cx", " /* a /* b */ c */\n  let x = -12").unwrap();
        assert_eq!(tokens[0].pos, Pos::new("x.cx", 2, 3));
        assert_eq!(tokens[0].line_start, 3);
        assert_eq!(tokens.last().unwrap().kind, TokenKind::Integer(-12));
    }

    #[test]
    fn string_escapes() {
        assert_eq!(
            lex("x", r#""a\n\x42\u263a""#).unwrap()[0].kind,
            TokenKind::String("a\nB☺".into())
        );
    }
}
