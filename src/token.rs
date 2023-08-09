use crate::{
    error::Result,
    parse::{Cursor, Delimiter, Parse, Parser, Span, Token, TokenMarker},
};
use std::fmt;

pub use crate::parse::token::*;

macro_rules! define_punct {
    ($(#[$attr:meta])* $vis:vis struct $name:ident($display:literal);) => {
        $(#[$attr])*
        $vis struct $name {
            span: Span,
        }

        #[allow(non_snake_case)]
        $vis fn $name(marker: TokenMarker) -> $name {
            match marker {}
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(&self.span)
                    .finish()
            }
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
macro_rules! define_keyword {
    ($(#[$attr:meta])* $vis:vis struct $name:ident($display:literal);) => {
        $(#[$attr])*
        $vis struct $name {
            span: Span,
        }

        #[allow(non_snake_case)]
        $vis fn $name(marker: TokenMarker) -> $name {
            match marker {}
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(&self.span)
                    .finish()
            }
        }

        impl Token for $name {
            fn display() -> &'static str {
                $display
            }
            fn peek(cur: Cursor) -> bool {
                cur.get_ident().map(|i| i.value() == $display).unwrap_or(false)
            }
        }

        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                parser.parse::<Ident>().map(|i| Self { span: i.span() })
            }
        }
    };

}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Brace("{");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct RBrace("}");
}

impl Delimiter for Brace {
    type Right = RBrace;
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Bracket("[");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct RBracket("]");
}

impl Delimiter for Bracket {
    type Right = RBracket;
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Paren("(");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct RParen(")");
}

impl Delimiter for Paren {
    type Right = RParen;
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Comma(",");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Minus("-");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Gt(">");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Lt("<");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Ge(">=");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Le("<=");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Eq("==");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Colon(":");
}

define_punct! {
    #[derive(Clone, Copy)]
    pub struct Dot(".");
}

define_keyword! {
    #[derive(Clone, Copy)]
    pub struct And("and");
}

define_keyword! {
    #[derive(Clone, Copy)]
    pub struct Or("or");
}

define_keyword! {
    #[derive(Clone, Copy)]
    pub struct Null("null");
}

define_keyword! {
    #[derive(Clone, Copy)]
    pub struct Not("not");
}
