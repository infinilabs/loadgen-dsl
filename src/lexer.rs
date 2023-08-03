use crate::error::{Error, ErrorKind};

pub const EOF: u8 = b'\0';

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
define_pattern!(
    punct,
    b'!' | b'#'
        | b'$'
        | b'%'
        | b'&'
        | b'*'
        | b'+'
        | b','
        | b'-'
        | b'.'
        | b'/'
        | b':'
        | b';'
        | b'<'
        | b'='
        | b'>'
        | b'?'
        | b'@'
        | b'['
        | b']'
        | b'^'
        | b'{'
        | b'}'
        | b'|'
        | b'~'
);

type Result<T, E = ErrorKind> = std::result::Result<T, E>;

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

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    start: u32,
    end: u32,
}
pub struct Lexer<'a> {
    cur: BytesCursor<'a>,
    start: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            cur: BytesCursor {
                source,
                iter: source.bytes(),
            },
            start: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        loop {
            self.start = self.cur.pos();
            let kind = match self.cur.next() {
                // skip whitespace
                whitespace!() => {
                    self.skip_whitespace();
                    continue;
                }
                // skip comment
                b'/' if self.skip_if(|ch| ch == b'/') => {
                    self.skip_line();
                    continue;
                }
                ch @ (b'"' | b'\'') => self.next_string(ch),
                ch @ digit!() => self.next_number(ch),
                letter!() => {
                    self.skip_while(ident);
                    Ok(TokenKind::Ident)
                }
                punct!() => Ok(TokenKind::Punct),
                EOF if self.cur.is_eof() => Ok(TokenKind::Eof),
                _ => todo!(),
            };
            match kind {
                Ok(kind) => {
                    break Ok(Token {
                        span: self.span(),
                        spacing: if self.cur.is_eof() || self.skip_if(whitespace) {
                            Spacing::Alone
                        } else {
                            Spacing::Joint
                        },
                        kind,
                    });
                }
                Err(kind) => {
                    // skip all bytes until the next UTF-8 character.
                    self.skip_while(|ch| (ch as i8) >= -0x40);
                    break Err(Error {
                        span: self.span(),
                        kind,
                    });
                }
            }
        }
    }

    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.cur.pos(),
        }
    }

    fn skip_if(&mut self, p: fn(u8) -> bool) -> bool {
        if p(self.cur.next()) {
            true
        } else {
            self.cur.back();
            false
        }
    }

    fn skip_while(&mut self, p: fn(u8) -> bool) -> bool {
        if p(self.cur.next()) {
            loop {
                if self.cur.is_eof() {
                    break;
                } else if !p(self.cur.next()) {
                    self.cur.back();
                    break;
                }
            }
            true
        } else {
            self.cur.back();
            false
        }
    }

    fn skip_line(&mut self) {
        self.skip_while(|ch| !matches!(ch, b'\n'));
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(whitespace);
    }

    fn skip_digits(&mut self) {
        self.skip_while(digit);
    }

    fn next_number(&mut self, _first: u8) -> Result<TokenKind> {
        // leading digits
        self.skip_digits();
        // decimal part
        if self.skip_if(|ch| ch == b'.') && !self.skip_while(digit) {
            return Err(ErrorKind::MissingDecimal);
        }
        // exponent part
        if self.skip_if(|ch| matches!(ch, b'e' | b'E')) {
            self.skip_if(|ch| matches!(ch, b'+' | b'-'));
            if !self.skip_while(digit) {
                return Err(ErrorKind::MissingExponent);
            }
        }
        Ok(TokenKind::Number)
    }

    fn next_string(&mut self, quote: u8) -> Result<TokenKind> {
        loop {
            match self.cur.next() {
                b'\\' => self.cur.advance(),
                EOF if self.cur.is_eof() => return Err(ErrorKind::UnexptedEof),
                ch if ch == quote => break,
                _ => {}
            }
        }
        Ok(TokenKind::String)
    }
}

struct BytesCursor<'a> {
    source: &'a str,
    iter: std::str::Bytes<'a>,
}

impl<'a> BytesCursor<'a> {
    /// Checks whether this cursor reaches EOF.
    pub fn is_eof(&self) -> bool {
        self.iter.len() == 0
    }

    /// Returns the byte index of the next byte.
    fn pos(&self) -> u32 {
        (self.source.len() - self.iter.len()) as u32
    }

    /// Advances and returns the next byte. [`EOF`] is returned when reaches EOF.
    pub fn next(&mut self) -> u8 {
        self.iter.next().unwrap_or(EOF)
    }

    /// Moves the cursor to the previous byte.
    pub fn back(&mut self) {
        self.iter.next_back();
    }

    /// Moves the cursor to the next byte.
    pub fn advance(&mut self) {
        self.iter.next();
    }
}
