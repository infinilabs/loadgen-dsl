mod buffer;
pub use buffer::*;

mod lexer;
pub use lexer::*;

use crate::error::{Error, ErrorKind, ErrorKind::*, Result};

pub trait Token: Parse {
    fn display() -> &'static str;
    fn peek(cur: Cursor) -> bool;
}

pub trait Peek: Sized {
    fn peek(self, cur: Cursor) -> bool;

    fn not(self) -> Not<Self> {
        Not(self)
    }

    fn and<T: Peek>(self, and: T) -> And<Self, T> {
        And { a: self, b: and }
    }

    fn or<T: Peek>(self, or: T) -> Or<Self, T> {
        Or { a: self, b: or }
    }
}

impl<F> Peek for F
where
    F: FnOnce(Cursor) -> bool,
{
    fn peek(self, cur: Cursor) -> bool {
        (self)(cur)
    }
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub struct Parser<'a> {
    buf: ParserBuffer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            buf: ParserBuffer::new(source),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_eof()
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    pub fn peek<T: Peek>(&mut self, f: T) -> bool {
        let mut head = 0;
        self.buf.grow(1);
        f.peek(Cursor::new(&mut self.buf, &mut head))
    }

    fn parse_token(&mut self) -> Result<TokenKind> {
        let (token, result) = self.buf.next();
        result.map(Error::from).map(Err).unwrap_or(Ok(token))
    }

    fn parse_token_as<T>(&mut self) -> Result<T>
    where
        T: Token + TryFrom<TokenKind, Error = TokenKind>,
    {
        T::try_from(self.parse_token()?).map_err(|t| Error::unexpected(t.span(), T::display()))
    }
}

macro_rules! define_token {
    ($(#[$attr:meta])* enum $name:ident {
        $($variant:ident => $ty:ident,)*
    }) => {
        $(#[$attr])* enum $name {
            $($variant($ty),)*
        }

        impl $name {
            pub fn span(&self) -> Span {
                match self {
                    $(Self::$variant(v) => v.span(),)*
                }
            }
        }

        $(impl From<$ty> for $name {
            fn from(v: $ty) -> Self {
                Self::$variant(v)
            }
        }
        impl TryFrom<$name> for $ty {
            type Error = $name;
            fn try_from(v: $name) -> Result<Self, Self::Error> {
                match v {
                    $name::$variant(v) => Ok(v),
                    t => Err(t),
                }
            }
        }
        impl<'a> TryFrom<&'a $name> for &'a $ty {
            type Error = &'a $name;
            fn try_from(v: &'a $name) -> Result<Self, Self::Error> {
                if let $name::$variant(v) = v {
                    Ok(v)
                } else {
                    Err(v)
                }
            }
        }
        impl $ty {
            pub fn span(&self) -> Span {
                self.span
            }
        })*
    };
}
macro_rules! impl_peek {
    ($name:ident) => {
        #[allow(non_snake_case)]
        pub fn $name(cur: Cursor) -> bool {
            <$name as Token>::peek(cur)
        }
    };
}
macro_rules! impl_token {
    ($name:ident $display:literal) => {
        impl_peek!($name);
        impl Token for $name {
            fn display() -> &'static str {
                $display
            }
            fn peek(cur: Cursor) -> bool {
                cur.get_token_as::<Self>().is_some()
            }
        }
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                parser.parse_token_as()
            }
        }
    };
}
define_token! {
    #[derive(Debug)]
    enum TokenKind {
        Ident    => Ident,
        Number   => LitNumber,
        String   => LitString,
        Regexp   => LitRegexp,
        Punct    => Punct,
        Unknown  => Unknown,
        Eof      => Eof,
    }
}

#[derive(Clone, Debug)]
pub struct Ident {
    span: Span,
    value: Box<str>,
}

impl_token!(Ident "identifier");

impl Ident {
    pub fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitNumber {
    span: Span,
    value: f64,
}

impl_token!(LitNumber "number literal");

impl LitNumber {
    pub fn value(&self) -> f64 {
        self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitString {
    span: Span,
    value: Box<str>,
}

impl_token!(LitString "string literal");

impl LitString {
    pub fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitRegexp {
    span: Span,
    value: Box<str>,
}

impl_peek!(LitRegexp);

impl Token for LitRegexp {
    fn display() -> &'static str {
        "literal regular expression"
    }
    fn peek(cur: Cursor) -> bool {
        cur.get_punct().map(|p| p.value == '/').unwrap_or(false)
    }
}

impl Parse for LitRegexp {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.buf.set_flag(Lexer::ALLOW_REGEXP);
        parser.buf.reset();
        parser.parse_token_as()
    }
}

impl LitRegexp {
    pub fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Clone, Debug)]
pub struct Punct {
    span: Span,
    value: char,
    joint: bool,
}

impl_token!(Punct "punctuation");

impl Punct {
    pub fn value(&self) -> char {
        self.value
    }

    /// Returns whether this punctuation is immediately followed by another [`struct@Punct`].
    pub fn is_joint(&self) -> bool {
        self.joint
    }
}

#[derive(Debug)]
struct Unknown {
    span: Span,
}

#[derive(Debug)]
struct Eof {
    span: Span,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_and() {
        let mut parser = Parser::new("abc 123 'xyz'");
        assert!(parser.peek(Ident.and(LitNumber).and(LitString)));
    }

    #[test]
    fn peek_or() {
        let mut parser = Parser::new("abc 123 'xyz'");
        assert!(parser.peek(
            Any.not()
                .and(Any.not())
                .or(Any.and(LitNumber).and(LitString))
        ));
    }

    #[test]
    fn parse_token() {
        let mut parser = Parser::new("abc 123 'xyz' / /.*/");
        parser.parse::<Ident>().unwrap();
        parser.parse::<LitNumber>().unwrap();
        parser.parse::<LitString>().unwrap();
        parser.parse::<Punct>().unwrap();
        parser.parse::<LitRegexp>().unwrap();
    }

    #[test]
    fn parse_regexp() {
        let mut parser = Parser::new("/abc 123/ 'xyz'");
        parser.peek(Any.and(Any).and(Any));
        parser.parse::<LitRegexp>().unwrap();
        parser.parse::<LitString>().unwrap();
    }

    #[test]
    fn is_empty() {
        let mut parser = Parser::new("abc 123 'xyz'");
        parser.parse::<Ident>().unwrap();
        parser.parse::<LitNumber>().unwrap();
        parser.parse::<LitString>().unwrap();
        // Lexer is empty
        assert!(parser.is_empty());
        let mut parser = Parser::new("abc 123 'xyz'");
        parser.peek(Any.and(Any).and(Any));
        // Lexer is empty, but ParserBuffer is not
        assert!(!parser.is_empty());
        parser.parse::<Ident>().unwrap();
        parser.parse::<LitNumber>().unwrap();
        parser.parse::<LitString>().unwrap();
        // now ParserBuffer is empty
        assert!(parser.is_empty());
    }
}
