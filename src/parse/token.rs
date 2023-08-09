use super::*;

macro_rules! impl_peek {
    ($name:ident) => {
        #[allow(non_snake_case)]
        pub fn $name(marker: TokenMarker) -> $name {
            match marker {}
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

#[derive(Clone, Debug)]
pub struct Ident {
    pub(super) span: Span,
    pub(super) value: Box<str>,
}

impl_token!(Ident "identifier");

impl Ident {
    pub fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitNumber {
    pub(super) span: Span,
    pub(super) value: f64,
}

impl_token!(LitNumber "number literal");

impl LitNumber {
    pub fn value(&self) -> f64 {
        self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitString {
    pub(super) span: Span,
    pub(super) value: Box<str>,
}

impl_token!(LitString "string literal");

impl LitString {
    pub fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Clone, Debug)]
pub struct LitRegexp {
    pub(super) span: Span,
    pub(super) value: Box<str>,
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
    pub(super) span: Span,
    pub(super) value: char,
    pub(super) joint: bool,
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

#[derive(Clone, Debug)]
pub struct LitBool {
    pub(super) span: Span,
    pub(super) value: bool,
}

impl_peek!(LitBool);

impl Token for LitBool {
    fn display() -> &'static str {
        "literal boolean"
    }
    fn peek(cur: Cursor) -> bool {
        cur.get_ident()
            .map(|i| matches!(i.value(), "true" | "false"))
            .unwrap_or(true)
    }
}

impl Parse for LitBool {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let i = parser.parse::<Ident>()?;
        Ok(Self {
            span: i.span(),
            value: match i.value() {
                "true" => true,
                "false" => false,
                _ => {
                    return Err(Error::new_kind(
                        i.span(),
                        ExpectedType(Self::display().into()),
                    ))
                }
            },
        })
    }
}

impl LitBool {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn value(&self) -> bool {
        self.value
    }
}
