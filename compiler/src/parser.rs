// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
use crate::{
    ast::Terminated,
    error::{Error, Result},
    lexer::{LexKind, LexResult, LexToken, Lexer},
};
use std::{borrow::Cow, fmt, ops};

pub(crate) trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

impl<T> Parse for Option<T>
where
    T: Token,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek_token::<T>() {
            parser.parse().map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<T> Parse for Box<T>
where
    T: Parse,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse().map(Box::new)
    }
}

impl<T> Parse for Vec<T>
where
    T: Parse,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        let mut elems = Vec::new();
        loop {
            if parser.is_eot() {
                break;
            }
            elems.push(parser.parse()?);
            if parser.is_eot() {
                break;
            }
        }
        Ok(elems)
    }
}

pub(crate) trait Delimiter: 'static + Copy + Parse {
    const RIGHT: LexKind;
    fn combine(&mut self, right: LexToken);
}

pub(crate) trait Peek: 'static + Copy {
    fn display(&self, f: &mut fmt::Formatter) -> fmt::Result;
    fn peek(&self, cur: &mut Cursor) -> bool;

    fn or<T>(&self, other: T) -> Or<Self, T>
    where
        T: Peek,
        Self: Copy,
    {
        Or(*self, other)
    }
}

impl Peek for LexKind {
    fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.display())
    }

    fn peek(&self, cur: &mut Cursor) -> bool {
        cur.kind() == *self
    }
}

pub(crate) trait Token: 'static + Parse {
    fn display() -> &'static str;
    fn peek(cur: &mut Cursor) -> bool;
}

pub(crate) struct Parser<'a> {
    cur: Cursor<'a>,
    delim: Option<LexKind>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        Self {
            cur: Cursor {
                curr: lexer.parse_ignored(),
                next: None,
                lexer,
            },
            delim: None,
        }
    }

    /// Checks whether the cursor reaches the end of a token tree.
    pub fn is_eot(&self) -> bool {
        self.cur.lexer.is_eof()
            || (if let Some(delim) = self.delim {
                self.cur.curr.0.kind == delim
            } else {
                false
            })
    }

    pub fn source(&self) -> &str {
        self.cur.source()
    }

    /// Parses the next 2 joint tokens.
    pub fn parse_token2(&mut self) -> Result<(LexToken, LexToken)> {
        let cur = &mut self.cur;
        let (token2, err2) = cur
            .next
            .take()
            .map(|(_, r)| r)
            .unwrap_or_else(|| cur.lexer.parse());
        let (token1, err1) = std::mem::replace(&mut cur.curr, cur.lexer.parse_ignored());
        match (err1, err2) {
            (Some(mut e1), Some(e2)) => {
                e1.combine(e2);
                Err(e1)
            }
            (Some(e), _) | (_, Some(e)) => Err(e),
            (None, None) => Ok((token1, token2)),
        }
    }

    /// Parses the next token.
    pub fn parse_token(&mut self) -> Result<LexToken> {
        let cur = &mut self.cur;
        let next = cur
            .next
            .take()
            .map(|(_, r)| r)
            .unwrap_or_else(|| cur.lexer.parse_ignored());
        let (token, err) = std::mem::replace(&mut cur.curr, next);
        err.map(Err).unwrap_or(Ok(token))
    }

    pub fn expect_token(&mut self, kind: LexKind) -> Result<LexToken> {
        self.parse_token().and_then(|token| {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(Error::expected_token(token.span, kind))
            }
        })
    }

    pub(crate) fn unexpected_token<T, P>(&mut self, f: P) -> Result<T>
    where
        P: Peek,
    {
        Err(Error::expected_token(self.parse_token()?.span, f))
    }

    pub fn peek<P>(&mut self, f: P) -> bool
    where
        P: Peek,
    {
        f.peek(&mut self.cur)
    }

    pub fn peek_token<T>(&mut self) -> bool
    where
        T: Token,
    {
        T::peek(&mut self.cur)
    }

    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: Parse,
    {
        T::parse(self)
    }

    pub fn parse_delimited<T, D>(&mut self) -> Result<(D, T)>
    where
        T: Parse,
        D: Delimiter,
    {
        self.parse_delimited_with(T::parse)
    }

    pub fn parse_delimited_with<T, D, F>(&mut self, f: F) -> Result<(D, T)>
    where
        F: FnOnce(&mut Self) -> Result<T>,
        D: Delimiter,
    {
        let mut parser = self.delimited()?;
        let content = f(&mut parser)?;
        Ok((parser.finish()?, content))
    }

    pub fn delimited<D>(&mut self) -> Result<DelimitedParser<'a, '_, D>>
    where
        D: Delimiter,
    {
        Ok(DelimitedParser {
            delim: self.parse()?,
            prev_delim: self.delim.replace(D::RIGHT),
            parser: self,
        })
    }

    pub fn parse_terminated<T, P>(&mut self) -> Result<Terminated<T, P>>
    where
        T: Parse,
        P: Token,
    {
        let mut new = Terminated::new();
        new.parse_rest(self)?;
        Ok(new)
    }

    pub fn parse_seperated<T, P>(&mut self) -> Result<Terminated<T, P>>
    where
        T: Parse,
        P: Token,
    {
        let mut elems = Vec::new();
        let mut item = self.parse()?;
        loop {
            let Some(sep) = self.parse::<Option<P>>()? else {
                break;
            };
            elems.push((item, sep));
            item = self.parse()?;
        }
        Ok(Terminated {
            pairs: elems,
            trailing: Some(Box::new(item)),
        })
    }

    pub fn parse_finished<T>(&mut self) -> Result<T>
    where
        T: Parse,
    {
        let t = self.parse()?;
        self.finish()?;
        Ok(t)
    }

    pub fn finish(&mut self) -> Result<()> {
        self.expect_token(LexKind::Eof).map(|_| ())
    }
}

pub(crate) struct Cursor<'a> {
    lexer: Lexer<'a>,
    curr: LexResult,
    next: Option<(Cow<'a, str>, LexResult)>,
}

impl<'a> Cursor<'a> {
    pub fn kind(&self) -> LexKind {
        self.curr.0.kind
    }

    pub fn source(&self) -> &str {
        self.next
            .as_ref()
            .map(|(s, _)| s.as_ref())
            .unwrap_or_else(|| self.lexer.source())
    }

    pub fn next_kind(&mut self) -> LexKind {
        (self
            .next
            .get_or_insert_with(||
                // We need to save the current source, since the Lexer::source
                // returns the next token's after parsing.
                (self.lexer.copy_source(), self.lexer.parse()))
            .1)
            .0
            .kind
    }
}

pub(crate) struct DelimitedParser<'a, 'b, D> {
    parser: &'b mut Parser<'a>,
    delim: D,
    prev_delim: Option<LexKind>,
}

impl<D> DelimitedParser<'_, '_, D>
where
    D: Delimiter,
{
    pub fn finish(self) -> Result<D> {
        let Self {
            parser,
            mut delim,
            prev_delim,
        } = self;
        parser.delim = prev_delim;
        delim.combine(parser.expect_token(D::RIGHT)?);
        Ok(delim)
    }
}

impl<'a, D> ops::Deref for DelimitedParser<'a, '_, D> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        self.parser
    }
}

impl<'a, D> ops::DerefMut for DelimitedParser<'a, '_, D> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.parser
    }
}

pub enum TokenMarker {}

impl<T, F> Peek for F
where
    T: Token,
    F: 'static + Copy + FnOnce(TokenMarker) -> T,
{
    fn peek(&self, cur: &mut Cursor) -> bool {
        T::peek(cur)
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(T::display())
    }
}

#[derive(Clone, Copy)]
pub struct Or<T1, T2>(T1, T2);

impl<T1, T2> Peek for Or<T1, T2>
where
    T1: Peek,
    T2: Peek,
{
    fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.display(f)?;
        f.write_str(" or ")?;
        self.1.display(f)
    }

    fn peek(&self, cur: &mut Cursor) -> bool {
        self.0.peek(cur) || self.1.peek(cur)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    macro_rules! peek_ok {
        ($source:literal, $token:expr) => {{
            let mut parser = Parser::new($source);
            assert!(parser.peek($token));
        }};
    }

    #[test]
    fn peek() {
        peek_ok!("zzz", Ident);
        peek_ok!("\"abc\"", LitString);
        peek_ok!("123", LitInteger);
        peek_ok!("1.23", LitFloat);
        peek_ok!("'xyz'", LitString);
        peek_ok!(">=", Ge);
        peek_ok!("<", Lt);
        peek_ok!("and", And);
        peek_ok!("'wryy'", Ident.or(LitInteger).or(LitString));
    }

    #[test]
    fn parse_token() {
        let mut parser = Parser::new("abc 123 'xyz' , 4.56 == /.*/");
        parser.parse::<Ident>().unwrap();
        parser.parse::<LitInteger>().unwrap();
        parser.parse::<LitString>().unwrap();
        parser.parse::<Option<Comma>>().unwrap().unwrap();
        parser.parse::<LitFloat>().unwrap();
        parser.parse::<EqEq>().unwrap();
        parser.parse::<LitRegexp>().unwrap();
        assert!(parser.is_eot());
    }

    #[test]
    fn parse_delimited() {
        let mut parser = Parser::new("{ 123 } [ abc ] ( 'xyz' )");
        parser.parse_delimited::<LitInteger, Brace>().unwrap();
        parser.parse_delimited::<Ident, Bracket>().unwrap();
        parser.parse_delimited::<LitString, Paren>().unwrap();
        let mut parser = Parser::new("{ { [ [ ( ( 123 ) ) ] ] } }");
        parser
            .parse::<Delimited<
                Delimited<
                    Delimited<
                        Delimited<Delimited<Delimited<LitInteger, Paren>, Paren>, Bracket>,
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
        let mut parser = Parser::new("[ 1, 2, 3, 4 ] ( 'aaa', 'bbb', 'ccc', 'ddd', )");
        parser
            .parse_delimited::<Terminated<LitInteger, Comma>, Bracket>()
            .unwrap();
        parser
            .parse_delimited::<Terminated<LitString, Comma>, Paren>()
            .unwrap();
    }

    #[test]
    fn parse_seperated() {
        let mut parser = Parser::new("[ 1, 2, 3, 4 ] ( 'aaa', 'bbb', 'ccc', 'ddd', )");
        parser
            .parse_delimited::<Separated<LitInteger, Comma>, Bracket>()
            .unwrap();
        parser
            .parse_delimited::<Separated<LitString, Comma>, Paren>()
            .unwrap_err();
    }
}
