use crate::error::{Error, ErrorKind, ErrorKind::*, Result};
use std::{collections::VecDeque, marker::PhantomData, ops};

mod buffer;
pub use buffer::*;

mod lexer;
pub use lexer::*;

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

pub trait Parse: 'static + Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

/// A trait that defines a pair of delimiters, which must be a punctuation or errors will result.
pub trait Delimiter: Token {
    /// The right delimiter, which must also be a punctuation.
    type Right: Token;
}

#[derive(Clone, Debug)]
pub struct Delimited<T, D>
where
    D: Delimiter,
{
    pub left: D,
    pub content: T,
    pub right: D::Right,
}

impl<T, D> Parse for Delimited<T, D>
where
    T: Parse,
    D: Delimiter,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (left, content, right) = parser.parse_delimited()?;
        Ok(Self {
            left,
            content,
            right,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Terminated<T, P> {
    elems: Vec<(T, P)>,
    end: Option<T>,
}

impl<T, P> Default for Terminated<T, P> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, P> Parse for Terminated<T, P>
where
    T: Parse,
    P: Token,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        let mut new = Self::new();
        for pair in parser.parse_terminated::<T, P>() {
            match pair? {
                Pair::Terminated(t, p) => new.elems.push((t, p)),
                Pair::End(e) => new.end = Some(e),
            }
        }
        Ok(new)
    }
}

impl<T, P> Terminated<T, P> {
    pub fn new() -> Self {
        Self {
            elems: Vec::new(),
            end: None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Pair<T, P> {
    Terminated(T, P),
    End(T),
}

pub struct TerminatedIter<'a, 'b, T, P> {
    parser: &'b mut Parser<'a>,
    _marker: PhantomData<(T, P)>,
}

impl<'a, 'b, T, P> Iterator for TerminatedIter<'a, 'b, T, P>
where
    T: Parse,
    P: Token,
{
    type Item = Result<Pair<T, P>>;

    fn next(&mut self) -> Option<Self::Item> {
        let parser = &mut *self.parser;
        tryb!({
            if parser.is_empty() {
                return Ok(None);
            }
            let t = parser.parse()?;
            if parser.is_empty() {
                return Ok(Some(Pair::End(t)));
            }
            Ok(Some(Pair::Terminated(t, parser.parse()?)))
        })
        .transpose()
    }
}

type ParserState = u8;

pub struct Parser<'a> {
    buf: ParseBuffer<'a>,
    delimiter: Option<&'static str>,
    state: ParserState,
}

impl<'a> Parser<'a> {
    const RIGHT_DELIMITED: ParserState = 0b00000001;

    pub fn new(source: &'a str) -> Self {
        Self {
            buf: ParseBuffer::new(source),
            delimiter: None,
            state: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.check_state(Self::RIGHT_DELIMITED) || self.buf.is_empty()
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    pub fn peek<T: Peek>(&mut self, f: T) -> bool {
        f.peek(self.buf.cursor(&mut 0))
    }

    /// Consumes a joint punctuation sequence in the token stream and returns its [`Span`].
    ///
    /// # Panics
    ///
    /// Panics if the punctuation is empty.
    pub fn parse_punct(&mut self, display: &str) -> Result<Span> {
        if let Some(span) = self.buf.parse_punct(display) {
            self.post_parse();
            Ok(span)
        } else {
            self.parse_token_with(|token| {
                Err(Error::new_kind(
                    token.span(),
                    ExpectedToken(Box::from(format!("`{display}`"))),
                ))
            })
        }
    }

    pub fn parse_delimited<T: Parse, D: Delimiter>(&mut self) -> Result<(D, T, D::Right)> {
        let l = self.parse()?;
        let prev = self.delimiter.replace(D::Right::display());
        let content = self.parse()?;
        self.reset_state(Parser::RIGHT_DELIMITED);
        self.delimiter = prev;
        let r = self.parse()?;
        Ok((l, content, r))
    }

    pub fn parse_terminated<'b, T, P>(&'b mut self) -> TerminatedIter<'a, 'b, T, P> {
        TerminatedIter {
            parser: self,
            _marker: PhantomData,
        }
    }

    fn reset_state(&mut self, state: ParserState) {
        self.state &= !state;
    }

    fn set_state(&mut self, state: ParserState) {
        self.state |= state;
    }

    fn check_state(&self, state: ParserState) -> bool {
        self.state & state != 0
    }

    fn post_parse(&mut self) {
        if self
            .delimiter
            .map(|r| self.buf.cursor(&mut 0).peek_punct(r))
            .unwrap_or(false)
        {
            self.set_state(Self::RIGHT_DELIMITED);
        }
    }

    fn parse_token_with<T>(&mut self, f: impl FnOnce(TokenKind) -> Result<T>) -> Result<T> {
        let (token, e) = self.buf.next();
        self.post_parse();
        f(token).map_err(|e2| {
            if let Some(mut e) = e {
                e.combine(e2);
                e
            } else {
                e2
            }
        })
    }

    fn parse_token_as<T>(&mut self) -> Result<T>
    where
        T: Token + TryFrom<TokenKind, Error = TokenKind>,
    {
        self.parse_token_with(|token| {
            T::try_from(token).map_err(|t| Error::unexpected(t.span(), T::display()))
        })
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
        parser.buf.reset(Lexer::ALLOW_REGEXP);
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
    use crate::token::*;

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
    fn parse_delimited() {
        let mut parser = Parser::new("{ 123 } [ abc ] ( /xyz/ )");
        parser.parse_delimited::<LitNumber, Brace>().unwrap();
        parser.parse_delimited::<Ident, Bracket>().unwrap();
        parser.parse_delimited::<LitRegexp, Paren>().unwrap();
        let mut parser = Parser::new("{ { [ [ ( ( 123 ) ) ] ] } }");
        parser
            .parse::<Delimited<
                Delimited<
                    Delimited<
                        Delimited<Delimited<Delimited<LitNumber, Paren>, Paren>, Bracket>,
                        Bracket,
                    >,
                    Brace,
                >,
                Brace,
            >>()
            .unwrap();
    }

    #[test]
    fn parse_terminated() {
        let mut parser = Parser::new("[ 1, 2, 3, 4 ] ( /aaa/, /bbb/, /ccc/, /ddd/, )");
        parser
            .parse_delimited::<Terminated<LitNumber, Comma>, Bracket>()
            .unwrap();
        parser
            .parse_delimited::<Terminated<LitRegexp, Comma>, Paren>()
            .unwrap();
    }
}
