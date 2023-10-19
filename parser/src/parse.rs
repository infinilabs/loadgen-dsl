use crate::{
    error::{Error, Result},
    lex::{LexFlag, LexKind, LexResult, LexToken, Lexer, Span},
    terminated::TerminatedIter,
};
use std::{collections::VecDeque, fmt, marker::PhantomData};

pub trait Parse: 'static + Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub trait Peek: 'static + Sized {
    fn peek(&self, cur: Cursor) -> bool;

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    fn or<T>(self, or: T) -> Or<Self, T>
    where
        T: Peek,
    {
        Or { a: self, b: or }
    }
}

pub trait Token: 'static + Sized {
    fn display(f: &mut fmt::Formatter<'_>) -> fmt::Result;

    fn peek(cur: Cursor) -> bool;

    fn parse(cur: Cursor) -> Result<Self>;
}

pub trait Delimiter: Token {
    type Right: Token;

    fn combine(&mut self, right: Self::Right);
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    buf: VecDeque<LexResult>,
    pub(crate) delim: Option<fn(Cursor) -> bool>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            buf: VecDeque::new(),
            delim: None,
        }
    }

    pub fn is_eos(&mut self) -> bool {
        if let Some(delim) = self.delim {
            delim(self.cur()) || self.is_eof()
        } else {
            self.is_eof()
        }
    }

    fn is_eof(&mut self) -> bool {
        matches!(self.peek_at(0).kind, LexKind::Eof(_))
    }

    pub fn set_flag(&mut self, flag: LexFlag) {
        self.lexer.set_flag(flag);
        if self.buf.is_empty() {
            return;
        }
        self.lexer.seek(self.buf[0].0.span().start);
        self.buf.clear();
    }

    pub fn peek<P>(&mut self, f: P) -> bool
    where
        P: Peek,
    {
        f.peek(self.cur())
    }

    pub fn advance(&mut self) -> Result<LexToken> {
        self.parse_next()
    }

    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: Parse,
    {
        T::parse(self)
    }

    pub fn parse_token<T>(&mut self) -> Result<T>
    where
        T: Token,
    {
        if is_debug!() && self.is_eos() {
            panic!("attempts to parse out of stream");
        }
        T::parse(self.cur())
    }

    pub fn parse_delimited<T, D>(&mut self) -> Result<(D, T)>
    where
        T: Parse,
        D: Delimiter,
    {
        let prev = std::mem::replace(&mut self.delim, Some(D::Right::peek));
        let mut delim = self.parse_token::<D>()?;
        let content = self.parse()?;
        delim.combine(Token::parse(self.cur())?);
        self.delim = prev;
        Ok((delim, content))
    }

    pub fn parse_terminated<'b, T, P>(&'b mut self) -> TerminatedIter<'a, 'b, T, P> {
        TerminatedIter {
            parser: self,
            _marker: PhantomData,
        }
    }

    fn cur<'b>(&'b mut self) -> Cursor<'a, 'b> {
        Cursor { buf: self, head: 0 }
    }

    fn peek_at(&mut self, n: usize) -> &LexToken {
        if n >= self.buf.len() {
            self.buf
                .extend(std::iter::repeat_with(|| self.lexer.parse()).take(n - self.buf.len() + 1));
        }
        &self.buf[n].0
    }

    fn parse_next(&mut self) -> Result<LexToken> {
        let (token, err) = self.buf.pop_front().unwrap_or_else(|| self.lexer.parse());
        err.map(Err).unwrap_or(Ok(token))
    }
}

pub struct Cursor<'a, 'b> {
    pub(crate) buf: &'b mut Parser<'a>,
    pub(crate) head: usize,
}

impl<'a, 'b> Cursor<'a, 'b> {
    pub fn fetch(&self, span: Span) -> &str {
        self.buf.lexer.fetch(span)
    }

    pub fn advance(&mut self) {
        self.head += 1;
    }

    pub fn peek(&mut self) -> &LexToken {
        self.buf.peek_at(self.head)
    }

    pub fn parse(&mut self) -> Result<LexToken> {
        self.buf.parse_next()
    }

    pub fn peek_keyword(&mut self, kw: &str) -> bool {
        let tk = self.peek();
        let span = tk.span();
        tk.as_ident().is_some() && self.fetch(span) == kw
    }

    pub fn parse_keyword(&mut self, kw: &str) -> Result<Span> {
        let tk = self.parse()?;
        let span = tk.span();
        if tk.as_ident().is_some() && self.fetch(span) == kw {
            Ok(span)
        } else {
            Err(Error::expected(span, format!("`{kw}`")))
        }
    }

    pub fn peek_punct(&mut self, pn: &str) -> bool {
        let mut chars = pn.chars();
        let Some(mut ch) = chars.next() else {
            return true;
        };
        let mut tk = self.peek();
        loop {
            // not a symbol
            let Some(sym) = tk.as_symbol() else {
                break;
            };
            // not matches
            if sym.value != ch {
                break;
            }
            // all matches
            let Some(next) = chars.next() else {
                return true;
            };
            // symbol must joint with next
            if !sym.joint {
                break;
            }
            ch = next;
            self.advance();
            tk = self.peek();
        }
        false
    }

    pub fn parse_punct(&mut self, pn: &str) -> Result<Span> {
        let mut chars = pn.chars();
        let mut tk = self.parse()?;
        let first = tk.span();
        let Some(mut ch) = chars.next() else {
            return Ok(first);
        };
        loop {
            // not a symbol
            let Some(sym) = tk.as_symbol() else {
                break;
            };
            // not matches
            if sym.value != ch {
                break;
            }
            // all matches
            let Some(next) = chars.next() else {
                return Ok(sym.span());
            };
            // symbol must joint with next
            if !sym.joint {
                break;
            }
            ch = next;
            tk = self.parse()?;
        }
        Err(Error::expected(first.join(tk.span()), format!("`{pn}`")))
    }
}

pub enum TokenMarker {}

impl<T, F> Peek for F
where
    T: Token,
    F: 'static + FnOnce(TokenMarker) -> T,
{
    fn peek(&self, cur: Cursor) -> bool {
        T::peek(cur)
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::display(f)
    }
}

impl<T: Token> Parse for Option<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if T::peek(parser.cur()) {
            parser.parse_token().map(Some)
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

pub struct Or<A, B> {
    a: A,
    b: B,
}

impl<A, B> Peek for Or<A, B>
where
    A: Peek,
    B: Peek,
{
    fn peek(&self, cur: Cursor) -> bool {
        let Cursor { buf, head } = cur;
        self.a.peek(Cursor { buf, head }) || self.b.peek(Cursor { buf, head })
    }
    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.a.display(f)?;
        f.write_str(" or ")?;
        self.b.display(f)
    }
}

pub struct Any;

impl Peek for Any {
    fn peek(&self, _: Cursor) -> bool {
        true
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any token")
    }
}

#[derive(Clone, Debug)]
pub struct Delimited<T, D> {
    pub delim: D,
    pub content: T,
}

impl<T, D> Parse for Delimited<T, D>
where
    T: Parse,
    D: Delimiter,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (delim, content) = parser.parse_delimited()?;
        Ok(Self { delim, content })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{terminated::Terminated, token::*};

    macro_rules! peek_ok {
        ($source:literal, $($peek:tt)*) => {{
            let mut parser = Parser::new($source);
            assert!(parser.cur()$($peek)*);
        }};
    }

    #[test]
    fn peek() {
        peek_ok!("abc", .peek().as_ident().is_some());
        peek_ok!("123", .peek().as_number().is_some());
        peek_ok!("'xyz'", .peek().as_string().is_some());
        peek_ok!(">=", .peek_punct(">="));
        peek_ok!(">=", .peek_punct(">"));
        peek_ok!("where", .peek_keyword("where"));
    }

    #[test]
    fn peek_or() {
        let mut parser = Parser::new("'xyz'");
        assert!(parser.peek(Ident.or(LitNumber).or(LitString)));
    }

    #[test]
    fn parse_token() {
        let mut parser = Parser::new("abc 123 'xyz' /== /.*/");
        parser.parse::<Ident>().unwrap();
        parser.parse::<LitNumber>().unwrap();
        parser.parse::<LitString>().unwrap();
        parser.parse::<Div>().unwrap();
        parser.parse::<Eq>().unwrap();
        parser.set_flag(LexFlag::ALLOW_REGEXP);
        parser.parse::<LitRegexp>().unwrap();
    }

    #[test]
    fn parse_regexp() {
        let mut parser = Parser::new("/abc 123/ 'xyz'");
        assert!(parser.peek(Div));
        parser.set_flag(LexFlag::ALLOW_REGEXP);
        assert!(parser.peek(LitRegexp));
        parser.parse::<LitRegexp>().unwrap();
        parser.parse::<LitString>().unwrap();
    }

    #[test]
    fn parse_delimited() {
        let mut parser = Parser::new("{ 123 } [ abc ] ( 'xyz' )");
        parser.parse_delimited::<LitNumber, Brace>().unwrap();
        parser.parse_delimited::<Ident, Bracket>().unwrap();
        parser.parse_delimited::<LitString, Paren>().unwrap();
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
        let mut parser = Parser::new("[ 1, 2, 3, 4 ] ( 'aaa', 'bbb', 'ccc', 'ddd', )");
        parser
            .parse_delimited::<Terminated<LitNumber, Comma>, Bracket>()
            .unwrap();
        parser
            .parse_delimited::<Terminated<LitString, Comma>, Paren>()
            .unwrap();
    }
}
