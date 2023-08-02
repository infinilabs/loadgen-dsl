use crate::{
    cursor::*,
    error::{Error, ErrorKind},
};

type Result<T, E = ErrorKind> = std::result::Result<T, E>;

pub struct Lexer<'a> {
    cur: Cursor<'a>,
}

macro_rules! define_pattern {
    ($name:ident, $pat:pat) => {
        #[allow(unused)]
        macro_rules! $name {
            () => {
                $pat
            };
        }
        #[allow(unused)]
        fn $name(ch: u8) -> bool {
            matches!(ch, $pat)
        }
    };
}
define_pattern!(whitespace, b'\x09' | b'\x0A' | b'\x0D' | b'\x20');
define_pattern!(digit, b'0'..=b'9');
define_pattern!(letter, b'a'..=b'z' | b'A' ..=b'Z');
define_pattern!(ident, b'_' | b'-' | letter!() | digit!());

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            cur: Cursor::new(source),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Token, Error> {
        loop {
            let start = self.cur.pos();
            let kind = match self.cur.next() {
                // skip whitespace
                whitespace!() => {
                    self.skip_whitespace();
                    continue;
                }
                // skip comment
                b'/' if self.cur.skip_if(|ch| ch == b'/') => {
                    self.skip_line();
                    continue;
                }
                letter!() => {
                    self.cur.skip_while(ident);
                    Ok(TokenKind::Ident)
                }
                ch @ (b'"' | b'\'' | b'/') => self.next_quoted(ch),
                ch @ digit!() => self.next_number(ch),
                EOF if self.cur.is_eof() => Ok(TokenKind::Eof),
                _ => todo!(),
            };
            match kind {
                Ok(kind) => {
                    break Ok(Token {
                        span: self.cur.span_from(start),
                        spacing: if self.cur.is_eof() || self.cur.skip_if(whitespace) {
                            Spacing::Alone
                        } else {
                            Spacing::Joint
                        },
                        kind,
                    });
                }
                Err(kind) => {
                    // skip all bytes until the next UTF-8 character.
                    self.cur.skip_while(|ch| (ch as i8) >= -0x40);
                    break Err(Error {
                        span: self.cur.span_from(start),
                        kind,
                    });
                }
            }
        }
    }

    fn skip_line(&mut self) {
        self.cur.skip_while(|ch| !matches!(ch, b'\n'));
    }

    fn skip_whitespace(&mut self) {
        self.cur.skip_while(whitespace);
    }

    fn skip_digits(&mut self) {
        self.cur.skip_while(digit);
    }

    fn next_number(&mut self, _first: u8) -> Result<TokenKind> {
        // leading digits
        self.skip_digits();
        // decimal part
        if self.cur.skip_if(|ch| ch == b'.') && !self.cur.skip_while(digit) {
            return Err(ErrorKind::MissingDecimal);
        }
        // exponent part
        if self.cur.skip_if(|ch| matches!(ch, b'e' | b'E')) {
            self.cur.skip_if(|ch| matches!(ch, b'+' | b'-'));
            if !self.cur.skip_while(digit) {
                return Err(ErrorKind::MissingExponent);
            }
        }
        Ok(TokenKind::Number)
    }

    fn next_quoted(&mut self, quote: u8) -> Result<TokenKind> {
        loop {
            match self.cur.next() {
                b'\\' => self.cur.advance(),
                EOF if self.cur.is_eof() => return Err(ErrorKind::UnexptedEof),
                ch if ch == quote => break,
                _ => {}
            }
        }
        Ok(match quote {
            b'\'' | b'"' => TokenKind::String,
            b'/' => TokenKind::Regexp,
            _ => unreachable!(),
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    span: Span,
    kind: TokenKind,
    spacing: Spacing,
}

impl Token {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn spacing(&self) -> Spacing {
        self.spacing
    }

    pub fn is_joint(&self) -> bool {
        self.spacing == Spacing::Joint
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    Ident,
    Number,
    Punct,
    String,
    Regexp,
    Eof,
}

/// Describes whether a [`Token`] is immediately followed by another.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Spacing {
    Alone,
    Joint,
}
