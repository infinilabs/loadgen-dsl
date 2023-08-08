use crate::{
    error::Result,
    parse::{Cursor, Delimiter, Parse, Parser, Span, Token},
};

macro_rules! define_token {
    ($(#[$attr:meta])* $vis:vis struct $name:ident($display:literal);) => {
        $(#[$attr])*
        $vis struct $name {
            span: Span,
        }

        #[allow(non_snake_case)]
        $vis fn $name(cur: Cursor) -> bool {
            <$name as Token>::peek(cur)
        }

        impl Token for $name {
            fn display() -> &'static str {
                $display
            }
            fn peek(cur: Cursor) -> bool {
                cur.peek_punct($display)
            }
        }

        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                parser.parse_punct($display).map(|span| Self { span })
            }
        }
    };
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct Brace("{");
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct RBrace("}");
}

impl Delimiter for Brace {
    type Right = RBrace;
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct Bracket("[");
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct RBracket("]");
}

impl Delimiter for Bracket {
    type Right = RBracket;
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct Paren("(");
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct RParen(")");
}

impl Delimiter for Paren {
    type Right = RParen;
}

define_token! {
    #[derive(Clone, Copy, Debug)]
    pub struct Comma(",");
}
