use crate::{
    ast::Spanned,
    error::Result,
    lexer::{LexKind, LexToken, Span},
    parser::{Cursor, Delimiter, Parse, Parser, Token, TokenMarker},
};

macro_rules! impl_extra {
    ($vis:vis $name:ident) => {
        #[allow(non_snake_case)]
        $vis fn $name(marker: TokenMarker) -> $name {
            match marker {}
        }

        impl Spanned for $name {
            fn span(&self) -> Span {
                self.span
            }
        }
    };
}

macro_rules! impl_delim {
    ($left:ident, $right:ident) => {
        impl Delimiter for $left {
            const RIGHT: LexKind = LexKind::$right;

            fn combine(&mut self, right: LexToken) {
                self.span = self.span.join(right.span)
            }
        }
    };
}

macro_rules! define_token {
    ($(#[symbol = $symbol:tt])?
    #[display = $display:literal]
    $vis:vis struct $name:ident;) => {
        #[doc = concat!("`", $display, "`")]
        #[derive(Clone, Copy, Debug)]
        $vis struct $name {
            span: Span,
        }

        impl_extra!($vis $name);

        impl Token for $name {
            fn display() -> &'static str {
                concat!("`", $display, "`")
            }

            fn peek(cur: &mut Cursor) -> bool {
                define_token!(@impl_peek cur, $display $(,$symbol)*)
            }
        }

        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                if parser.peek($name) {
                    Ok(Self {
                        span: define_token!(@parse_span parser, $display $(,$symbol)*),
                    })
                } else {
                    parser.unexpected_token($name)
                }
            }
        }
    };
    (@impl_peek $cur:ident, $display:literal) => {
        ($cur.kind() == LexKind::Ident) && ($cur.source() == $display)
    };
    (@impl_peek $cur:ident, $display:literal, $symbol:ident) => {
        $cur.kind() == LexKind::$symbol
    };
    (@impl_peek $cur:ident, $display:literal, ($symbol1:ident, $symbol2:ident)) => {
        ($cur.kind() == LexKind::$symbol1) && ($cur.next_kind() == LexKind::$symbol2)
    };
    (@parse_span $parser:ident, $display:literal) => {
        $parser.parse_token()?.span
    };
    (@parse_span $parser:ident, $display:literal, $symbol:ident) => {
        $parser.parse_token()?.span
    };
    (@parse_span $parser:ident, $display:literal, ($symbol1:ident, $symbol2:ident)) => {{
        let (token1, token2) = $parser.parse_token2()?;
        token1.span.join(token2.span)
    }};

}

define_token!(
    #[display = "and"]
    pub struct And;
);

define_token!(
    #[display = "or"]
    pub struct Or;
);

define_token!(
    #[display = "null"]
    pub struct Null;
);

define_token!(
    #[display = "not"]
    pub struct Not;
);

define_token!(
    #[symbol = BraceL]
    #[display = "{..}"]
    pub struct Brace;
);

impl_delim!(Brace, BraceR);

define_token!(
    #[symbol = BracketL]
    #[display = "[..]"]
    pub struct Bracket;
);

impl_delim!(Bracket, BracketR);

define_token!(
    #[symbol = ParenL]
    #[display = "(..)"]
    pub struct Paren;
);

impl_delim!(Paren, ParenR);

define_token! {
    #[symbol = Comma]
    #[display = ","]
    pub struct Comma;
}

define_token! {
    #[symbol = Minus]
    #[display = "-"]
    pub struct Minus;
}

define_token! {
    #[symbol = Gt]
    #[display = ">"]
    pub struct Gt;
}

define_token! {
    #[symbol = (Gt, Eq)]
    #[display = ">="]
    pub struct Ge;
}

define_token! {
    #[symbol = Lt]
    #[display = "<"]
    pub struct Lt;
}

define_token! {
    #[symbol = (Lt, Eq)]
    #[display = "<="]
    pub struct Le;
}

define_token! {
    #[symbol = (Eq, Eq)]
    #[display = "=="]
    pub struct EqEq;
}

define_token! {
    #[symbol = Colon]
    #[display = ":"]
    pub struct Colon;
}

define_token! {
    #[symbol = Dot]
    #[display = "."]
    pub struct Dot;
}

macro_rules! define_token_with_source {
    ($(#[doc = $doc:literal])*
    #[kind = $kind:ident]
    #[source = |$s:ident| -> $ty:ty { $source:expr }]
    $vis:vis struct $name:ident;) => {
        $(#[doc = $doc])*
        #[derive(Clone, Debug)]
        $vis struct $name {
            span: Span,
            value: $ty,
        }

        impl_extra!($vis $name);

        impl Token for $name {
            fn display() -> &'static str {
                LexKind::$kind.display()
            }

            fn peek(cur: &mut Cursor) -> bool {
                cur.kind() == LexKind::$kind
            }
        }

        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                if parser.peek($name) {
                    let $s = parser.source();
                    Ok(Self {
                        value: $source,
                        span: parser.parse_token()?.span,
                    })
                } else {
                    parser.unexpected_token($name)
                }
            }
        }
    };
}

define_token_with_source!(
    /// Identifier.
    #[kind = Ident]
    #[source = |s| -> Box<str> { s.into() }]
    pub struct Ident;
);

impl Ident {
    pub fn value(&self) -> &str {
        &self.value
    }
}

impl<T> PartialEq<T> for Ident
where
    T: AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
        self.value() == other.as_ref()
    }
}

fn source_to_string(s: &str) -> Box<str> {
    debug_assert_matches!(s.as_bytes()[0], b'\'' | b'"' | b'/');
    debug_assert_matches!(s.as_bytes()[s.len() - 1], b'\'' | b'"' | b'/');
    s[1..s.len() - 1].into()
}

define_token_with_source!(
    /// Literal string.
    #[kind = String]
    #[source = |s| -> Box<str> { source_to_string(s) }]
    pub struct LitString;
);

impl LitString {
    pub fn value(&self) -> &str {
        &self.value
    }
}

define_token_with_source!(
    /// Literal regular expression.
    #[kind = Regexp]
    #[source = |s| -> Box<str> { source_to_string(s) }]
    pub struct LitRegexp;
);

impl LitRegexp {
    pub fn value(&self) -> &str {
        &self.value
    }
}

define_token_with_source!(
    /// Literal integer.
    #[kind = Integer]
    #[source = |s| -> i64 { s.parse().expect("invalid integer") }]
    pub struct LitInteger;
);

impl LitInteger {
    pub fn value(&self) -> i64 {
        self.value
    }
}

define_token_with_source!(
    /// Literal float.
    #[kind = Float]
    #[source = |s| -> f64 { s.parse().expect("invalid float") }]
    pub struct LitFloat;
);

impl LitFloat {
    pub fn value(&self) -> f64 {
        self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitBool {
    span: Span,
    value: bool,
}

impl_extra!(pub LitBool);

impl LitBool {
    pub fn value(&self) -> bool {
        self.value
    }
}

impl Token for LitBool {
    fn display() -> &'static str {
        "literal boolean"
    }

    fn peek(cur: &mut Cursor) -> bool {
        (cur.kind() == LexKind::Ident) && matches!(cur.source(), "false" | "true")
    }
}

impl Parse for LitBool {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(LitBool) {
            let source = parser.source();
            let value = match source {
                "false" => false,
                "true" => true,
                _ => return parser.unexpected_token(LitBool),
            };
            Ok(Self {
                value,
                span: parser.parse_token()?.span,
            })
        } else {
            parser.unexpected_token(LitBool)
        }
    }
}
