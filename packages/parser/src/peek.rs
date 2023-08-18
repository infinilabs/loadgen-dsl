use crate::parse::{Cursor, Peek, Token};
use std::fmt;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::*;

    #[test]
    fn peek_or() {
        let mut parser = Parser::new("'xyz'");
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
