use crate::{
    error::{Error, Result},
    lex::*,
    parse::{Cursor, Delimiter, Parse, Parser, Token, TokenMarker},
};
use std::fmt;

fn parse_as<K, T>(
    mut cur: Cursor,
    display: &str,
    parse: fn(&mut Cursor, Span, K) -> Result<T>,
) -> Result<T>
where
    K: TryFrom<LexKind>,
    T: Token,
{
    let tk = cur.parse()?;
    let span = tk.span();
    if let Ok(kind) = K::try_from(tk.kind) {
        parse(&mut cur, span, kind)
    } else {
        Err(Error::expected(span, display))
    }
}

macro_rules! impl_extra {
    ($name:ident) => {
        #[allow(non_snake_case)]
        pub fn $name(marker: TokenMarker) -> $name {
            match marker {}
        }
        impl $name {
            pub fn span(&self) -> Span {
                self.span
            }
        }
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> Result<Self> {
                parser.parse_token()
            }
        }
    };
}
macro_rules! impl_token {
    ($name:ident: $kind:ident $display:literal, $parse:expr $(,)?) => {
        impl_extra!($name);
        impl Token for $name {
            fn display(f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str($display)
            }
            fn peek(mut cur: Cursor) -> bool {
                <&$kind>::try_from(&cur.peek().kind).is_ok()
            }
            fn parse(cur: Cursor) -> Result<Self> {
                parse_as::<$kind, $name>(cur, $display, $parse)
            }
        }
    };
}

#[derive(Clone, Debug)]
pub struct Ident {
    span: Span,
    value: Box<str>,
}

impl_token!(
    Ident: LexIdent "identifier",
    |cur, span, _| Ok(Self {
        span,
        value: cur.fetch(span).into(),
    }),
);

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

impl_token!(
    LitNumber: LexNumber "literal number",
    |_, span, kind| Ok(Self {
        span,
        value: kind.value,
    }),
);

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

impl_token!(
    LitString: LexString "literal string",
    |_, span, kind| Ok(Self {
        span,
        value: kind.value,
    }),
);

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

impl_token!(
    LitRegexp: LexRegexp "literal regular expression",
    |_, span, kind| Ok(Self {
        span,
        value: kind.value,
    }),
);

impl LitRegexp {
    pub fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitBool {
    span: Span,
    value: bool,
}

impl_extra!(LitBool);

impl Token for LitBool {
    fn display(f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("literal boolean")
    }

    fn peek(mut cur: Cursor) -> bool {
        let token = cur.peek();
        let span = token.span();
        token.as_ident().is_some() && matches!(cur.fetch(span), "true" | "false")
    }

    fn parse(mut cur: Cursor) -> Result<Self> {
        let token = cur.parse()?;
        let span = token.span();
        if token.as_ident().is_some() {
            if let Ok(value) = cur.fetch(span).parse() {
                return Ok(Self { span, value });
            }
        }
        Err(Error::expected(span, "literal boolean"))
    }
}

impl LitBool {
    pub fn value(&self) -> bool {
        self.value
    }
}

macro_rules! define_token {
    ($(#[$attr:meta])* $vis:vis struct $name:ident($display:tt);) => {
        $(#[$attr])*
        $vis struct $name {
            span: Span,
        }
        impl_extra!($name);
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(&self.span)
                    .finish()
            }
        }
        impl Token for $name {
            fn display(f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(define_token!(@display $display))
            }
            fn peek(mut cur: Cursor) -> bool {
                define_token!(@peek cur, $display)
            }
            fn parse(mut cur: Cursor) -> Result<Self> {
                define_token!(@parse cur, $display).map(|span| Self { span })
            }
        }
    };
    (@display $kw:ident) =>  {
        concat!("`", stringify!($kw), "`")
    };
    (@display $pn:literal) =>  {
        concat!("`", $pn, "`")
    };
    (@peek $cur:ident, $kw:ident) =>  {
        $cur.peek_keyword(stringify!($kw))
    };
    (@peek $cur:ident, $pn:literal) =>  {
        $cur.peek_punct($pn)
    };
    (@parse $cur:ident, $kw:ident) =>  {
        $cur.parse_keyword(stringify!($kw))
    };
    (@parse $cur:ident, $pn:literal) =>  {
        $cur.parse_punct($pn)
    };
}
macro_rules! impl_delim {
    ($name:ident $right:ident) => {
        impl Delimiter for $name {
            type Right = $right;
            fn combine(&mut self, right: Self::Right) {
                self.span.combine(right.span)
            }
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
impl_delim!(Brace RBrace);

define_token! {
    #[derive(Clone, Copy)]
    pub struct Bracket("[");
}
define_token! {
    #[derive(Clone, Copy)]
    pub struct RBracket("]");
}
impl_delim!(Bracket RBracket);

define_token! {
    #[derive(Clone, Copy)]
    pub struct Paren("(");
}
define_token! {
    #[derive(Clone, Copy)]
    pub struct RParen(")");
}
impl_delim!(Paren RParen);

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

#[cfg(test)]
define_token! {
    #[derive(Clone, Copy)]
    pub struct Div("/");
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
