use crate::error::{Error, ErrorKind, ErrorKind::*, Result};
use std::{collections::VecDeque, fmt, marker::PhantomData, ops};

mod buffer;
pub use buffer::*;

mod lexer;
pub use lexer::*;

pub(crate) mod token;
use token::*;

pub trait Token: Parse {
    fn display() -> &'static str;
    fn peek(cur: Cursor) -> bool;
    fn is_punct() -> Option<&'static str> {
        None
    }
}

pub trait Peek: Sized {
    fn peek(&self, cur: Cursor) -> bool;
    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

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

pub enum TokenMarker {}

impl<T, F> Peek for F
where
    T: Token,
    F: FnOnce(TokenMarker) -> T,
{
    fn peek(&self, cur: Cursor) -> bool {
        T::peek(cur)
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(T::display())
    }
}

pub trait Parse: 'static + Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

impl<T: Token> Parse for Option<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if T::peek(parser.buf.cursor(&mut 0)) {
            parser.parse().map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse().map(Box::new)
    }
}

/// A trait that defines a pair of delimiters, which must be a punctuation or errors will result.
pub trait Delimiter: Token {
    /// The right delimiter, which must also be a punctuation.
    type Right: Token;
}

#[derive(Clone)]
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

impl<T, D> fmt::Debug for Delimited<T, D>
where
    T: fmt::Debug,
    D: Delimiter + fmt::Debug,
    D::Right: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
            .field(&self.left)
            .field(&self.content)
            .field(&self.right)
            .finish()
    }
}

#[derive(Clone)]
pub struct Terminated<T, P> {
    elems: Vec<(T, P)>,
    end: Option<Box<T>>,
}

impl<T, P> fmt::Debug for Terminated<T, P>
where
    T: fmt::Debug,
    P: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.elems.iter().map(|(t, _)| t).chain(self.end.as_deref()))
            .finish()
    }
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
        parser.parse_terminated::<T, P>().collect()
    }
}

impl<T, P> FromIterator<Pair<T, P>> for Terminated<T, P> {
    fn from_iter<I: IntoIterator<Item = Pair<T, P>>>(iter: I) -> Self {
        let mut new = Self::new();
        new.extend(iter);
        new
    }
}

impl<T, P> Extend<Pair<T, P>> for Terminated<T, P> {
    fn extend<I: IntoIterator<Item = Pair<T, P>>>(&mut self, iter: I) {
        for pair in iter {
            match pair {
                Pair::Terminated(t, p) => self.elems.push((t, p)),
                Pair::End(e) => self.end = Some(Box::new(e)),
            }
        }
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

pub struct Parser<'a> {
    buf: ParseBuffer<'a>,
    delimiter: Option<&'static str>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            buf: ParseBuffer::new(source),
            delimiter: None,
        }
    }

    pub fn is_empty(&mut self) -> bool {
        self.is_delimited() || self.buf.is_empty()
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
            self.is_delimited();
            Ok(span)
        } else {
            self.parse_token_with(|token| {
                Err(Error::new_kind(
                    token.span(),
                    ExpectedType(Box::from(format!("`{display}`"))),
                ))
            })
        }
    }

    /// # Panics
    ///
    /// Panics if the specified delimiter is not a punctuation.
    pub fn parse_delimited<T: Parse, D: Delimiter>(&mut self) -> Result<(D, T, D::Right)> {
        let l = self.parse()?;
        let prev = self
            .delimiter
            .replace(D::Right::is_punct().expect("a delimiter must be a punctuation"));
        let content = self.parse()?;
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

    pub fn span(&mut self) -> Span {
        self.buf.fill(1);
        self.buf[0].span()
    }

    fn is_delimited(&mut self) -> bool {
        self.delimiter
            .map(|r| self.buf.cursor(&mut 0).peek_punct(r))
            .unwrap_or(false)
    }

    fn parse_token_with<T>(&mut self, f: impl FnOnce(TokenKind) -> Result<T>) -> Result<T> {
        let (token, e) = self.buf.next();
        self.is_delimited();
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
            T::try_from(token).map_err(|t| Error::expected(t.span(), T::display()))
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
