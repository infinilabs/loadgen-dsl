use crate::{
    error::{Error, Result},
    parse::{Cursor, Delimiter, Parse, Parser, Span, Token, TokenMarker},
};
use std::fmt;

pub use crate::parse::token::*;

macro_rules! define_token {
    ($(#[$attr:meta])* $vis:vis struct $name:ident($display:tt);) => {
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
                define_token!(@display $display)
            }
            fn peek(cur: Cursor) -> bool {
                define_token!(@peek cur, $display)
            }
            fn is_punct() -> Option<&'static str> {
                define_token!(@is_punct $display)
            }
        }

        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                define_token!(@parse parser, $name, $display)
            }
        }
    };
    (@display $ident:ident) =>  {
        concat!("`", stringify!($ident), "`")
    };
    (@display $lit:literal) =>  {
        concat!("`", $lit, "`")
    };
    (@is_punct $ident:ident) =>  {
        None
    };
    (@is_punct $lit:literal) =>  {
        Some($lit)
    };
    (@peek $cur:ident, $ident:ident) =>  {
        $cur.get_ident().map(|i| i.value() == stringify!($ident)).unwrap_or(false)
    };
    (@peek $cur:ident, $lit:literal) =>  {
        $cur.peek_punct($lit)
    };
    (@parse $parser:ident, $name:ident, $ident:ident) =>  {
        if $parser.peek($name) {
            $parser.parse::<Ident>().map(|i| Self { span: i.span() })
        } else {
            Err(Error::expected_token($parser.span(), $name))
        }
    };
    (@parse $parser:ident, $name:ident, $lit:literal) =>  {
        if $parser.peek($name) {
            $parser.parse_punct($lit).map(|span| Self { span })
        } else {
            Err(Error::expected_token($parser.span(), $name))
        }
    };
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Brace("{");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct RBrace("}");
}

impl Delimiter for Brace {
    type Right = RBrace;
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Bracket("[");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct RBracket("]");
}

impl Delimiter for Bracket {
    type Right = RBracket;
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Paren("(");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct RParen(")");
}

impl Delimiter for Paren {
    type Right = RParen;
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Comma(",");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Minus("-");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Gt(">");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Lt("<");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Ge(">=");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Le("<=");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Eq("==");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Colon(":");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Dot(".");
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct And(and);
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Or(or);
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Null(null);
}

define_token! {
    #[derive(Clone, Copy)]
    pub struct Not(not);
}
