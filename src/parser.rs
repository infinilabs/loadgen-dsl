mod buffer;
use buffer::*;

mod lexer;
use lexer::*;

use crate::{
    error::{Error, ErrorKind, ErrorKind::*, Result},
    Parse, Peek,
};

const DUMMY: Token = Token::Eof(Eof {
    span: Span {
        start: u32::MAX,
        end: u32::MAX,
    },
});

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    start: u32,
    end: u32,
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

    pub fn peek<T: Peek>(&mut self, f: T) -> bool {
        if self.buf.is_empty() {
            self.buf.clear();
            self.buf.push();
        }
        self.buf.peek(|buf| f.peek(Cursor { buf }))
    }

    fn parse_token(&mut self) -> Result<Token> {
        let (token, result) = if self.buf.is_empty() {
            self.buf.lexer.parse()
        } else {
            self.buf.pop()
        };
        result.map(Error::from).map(Err).unwrap_or(Ok(token))
    }

    fn parse_token_as<T>(&mut self) -> Result<T>
    where
        T: crate::Token + TryFrom<Token, Error = Token>,
    {
        T::try_from(self.parse_token()?).map_err(|t| Error::unexpected(t.span(), T::display()))
    }
}

pub struct Cursor<'a, 'b> {
    buf: &'b mut ParserBuffer<'a>,
}

impl<'a, 'b> Cursor<'a, 'b> {
    fn get_token_as<T>(&self) -> Option<&T>
    where
        for<'t> &'t T: TryFrom<&'t Token>,
    {
        self.buf.get_token().try_into().ok()
    }

    pub fn advance(&mut self) {
        self.buf.advance();
    }

    pub fn get_ident(&self) -> Option<&Ident> {
        self.get_token_as()
    }

    pub fn get_number(&self) -> Option<&LitNumber> {
        self.get_token_as()
    }

    pub fn get_sting(&self) -> Option<&LitString> {
        self.get_token_as()
    }

    pub fn get_punct(&self) -> Option<&Punct> {
        self.get_token_as()
    }

    pub fn peek_punct(&mut self, display: &str) -> bool {
        let mut chars = display.chars();
        let Some(mut ch) = chars.next() else { return false };
        loop {
            let Some(p) = self.get_punct() else { break };
            if p.value != ch {
                break;
            }
            let Some(next) = chars.next() else { break };
            if !p.joint {
                break;
            }
            self.advance();
            ch = next;
        }
        false
    }
}

macro_rules! define_token {
    (enum $name:ident {
        $($variant:ident => $ty:ident,)*
    }) => {
        enum $name { $($variant($ty),)* }

        impl $name {
            pub fn span(&self) -> Span {
                match self {
                    $(Self::$variant(v) => v.span,)*
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
macro_rules! impl_token {
    ($name:ident $display:literal) => {
        #[allow(non_snake_case)]
        pub fn $name(cur: Cursor) -> bool {
            <$name as crate::Token>::peek(cur)
        }
        impl crate::Token for $name {
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
    enum Token {
        Ident    => Ident,
        Number   => LitNumber,
        String   => LitString,
        Regexp   => LitRegexp,
        Punct    => Punct,
        Unknown  => Unknown,
        Eof      => Eof,
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LitRegexp {
    span: Span,
    value: Box<str>,
}

impl crate::Token for LitRegexp {
    fn display() -> &'static str {
        "literal regular expression"
    }
    fn peek(cur: Cursor) -> bool {
        cur.get_punct().map(|p| p.value == '/').unwrap_or(false)
    }
}

impl Parse for LitRegexp {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.buf.lexer.set_flag(Lexer::ALLOW_REGEXP);
        if !parser.buf.is_empty() {
            parser.buf.reset();
        }
        parser.parse_token_as()
    }
}

impl LitRegexp {
    pub fn value(&self) -> &str {
        &self.value
    }
}

pub struct Punct {
    span: Span,
    value: char,
    joint: bool,
}

impl_token!(Punct "punctuation");

#[derive(Debug, Copy, Clone)]
struct Unknown {
    span: Span,
}

#[derive(Debug, Copy, Clone)]
struct Eof {
    span: Span,
}

pub struct And<A, B> {
    pub(crate) a: A,
    pub(crate) b: B,
}

impl<A, B> Peek for And<A, B>
where
    A: Peek,
    B: Peek,
{
    fn peek(&self, cur: Cursor) -> bool {
        let Cursor { buf } = cur;
        self.a.peek(Cursor { buf }) && {
            buf.advance();
            self.b.peek(Cursor { buf })
        }
    }
}

pub struct Or<A, B> {
    pub(crate) a: A,
    pub(crate) b: B,
}

impl<A, B> Peek for Or<A, B>
where
    A: Peek,
    B: Peek,
{
    fn peek(&self, cur: Cursor) -> bool {
        let Cursor { buf } = cur;
        buf.peek(|buf| self.a.peek(Cursor { buf })) || self.b.peek(Cursor { buf })
    }
}