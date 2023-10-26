#[path = "token.rs"]
mod token;

use crate::{
    error::Result,
    parser::{Delimiter, Parse, Parser, Peek, Token},
};
use std::fmt;

pub use {crate::lexer::Span, token::*};

pub trait Spanned {
    fn span(&self) -> Span;
}

impl<T> Spanned for Vec<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        let Some(first) = self.first() else {
            return Span::dummy();
        };
        let last = self.last().unwrap();
        first.span().join(last.span())
    }
}

#[derive(Clone, Debug)]
pub struct Terminated<T, P> {
    pub(crate) elems: Vec<(T, P)>,
    pub(crate) trailing: Option<Box<T>>,
}

impl<T, P> Terminated<T, P> {
    pub fn items(&self) -> impl Iterator<Item = &T> {
        self.elems
            .iter()
            .map(|(t, _)| t)
            .chain(self.trailing.as_deref().into_iter())
    }
}

impl<T, P> Parse for Terminated<T, P>
where
    T: Parse,
    P: Token,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse_terminated()
    }
}

impl<T, P> Spanned for Terminated<T, P>
where
    T: Spanned,
    P: Spanned,
{
    fn span(&self) -> Span {
        let Some(start) = self.elems.first() else {
            debug_assert!(self.trailing.is_none());
            return Span::dummy();
        };
        let end = self
            .trailing
            .as_deref()
            .map(T::span)
            .or_else(|| self.elems.last().map(|(_, p)| p.span()))
            .unwrap();
        start.0.span().join(end)
    }
}

#[derive(Clone, Debug)]
pub struct Delimited<T, P> {
    pub delim: P,
    pub content: T,
}

impl<T, P> Parse for Delimited<T, P>
where
    T: Parse,
    P: Delimiter,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (delim, content) = parser.parse_delimited()?;
        Ok(Self { delim, content })
    }
}

impl<T, P> Spanned for Delimited<T, P>
where
    T: Spanned,
    P: Spanned,
{
    fn span(&self) -> Span {
        self.delim.span()
    }
}

#[derive(Clone)]
pub struct Separated<T, P>(pub Terminated<T, P>);

impl<T, P> Parse for Separated<T, P>
where
    T: Parse,
    P: Token,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse_seperated().map(Self)
    }
}

impl<T, P> fmt::Debug for Separated<T, P>
where
    T: fmt::Debug,
    P: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T, P> Spanned for Separated<T, P>
where
    T: Spanned,
    P: Spanned,
{
    fn span(&self) -> Span {
        self.0.span()
    }
}

macro_rules! define_ast_enum {
    ($(#[$attr:meta])*
    $vis:vis enum $name:ident {
        $($var:ident($ty:ty),)*
    }) => {
        $(#[$attr])*
        #[derive(Clone, Debug)]
        $vis enum $name {
            $($var($ty),)*
        }

        impl Spanned for $name {
            fn span(&self) -> Span {
                match self {
                    $($name::$var(t) => t.span(),)*
                }
            }
        }
    };
}

macro_rules! define_ast_struct {
    (#[span = $span:tt]
    $(#[$attr:meta])*
    $vis:vis struct $name:ident {
        $($field:ident: $ty:ty,)*
    }) => {
        $(#[$attr])*
        #[derive(Clone, Debug)]
        $vis struct $name {
            $(pub $field: $ty,)*
        }

        impl Spanned for $name {
            define_ast_struct!(@impl_spanned $span);
        }
    };
    (@impl_spanned _span) => {
        fn span(&self) -> Span {
            self._span()
        }
    };
    (@impl_spanned $span:ident) => {
        fn span(&self) -> Span {
            self.$span.span()
        }
    };
    (@impl_spanned ($start:ident, $end:ident)) => {
        fn span(&self) -> Span {
            self.$start.span().join(self.$end.span())
        }
    };
}

define_ast_enum!(
    pub enum Dsl {
        Brief(DslBrief),
        Full(DslFull),
    }
);

impl Dsl {
    fn peek() -> impl Peek {
        LitInteger.or(Brace).or(Ident).or(LitString)
    }
}

impl Parse for Dsl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(LitInteger) || parser.peek(Brace) {
            parser.parse().map(Self::Brief)
        } else if parser.peek(Ident) || parser.peek(LitString) {
            parser.parse().map(Self::Full)
        } else {
            parser.unexpected_token(Self::peek())
        }
    }
}

define_ast_struct!(
    #[span = _span]
    pub struct DslBrief {
        status: Option<LitInteger>,
        body: ExprObject,
    }
);

impl DslBrief {
    fn _span(&self) -> Span {
        if let Some(status) = self.status.as_ref() {
            status.span().join(self.body.span())
        } else {
            self.body.span()
        }
    }
}

impl Parse for DslBrief {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self {
            status: parser.parse()?,
            body: parser.parse()?,
        })
    }
}

define_ast_struct!(
    #[span = fields]
    pub struct DslFull {
        fields: Terminated<Field, Comma>,
    }
);

impl Parse for DslFull {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self {
            fields: parser.parse()?,
        })
    }
}

define_ast_enum!(
    pub enum Expr {
        Array(ExprArray),
        Binary(ExprBinary),
        Funcall(ExprFuncall),
        Lit(ExprLit),
        Object(ExprObject),
        Paren(ExprParen),
        Unary(ExprUnary),
    }
);

impl Expr {
    fn peek() -> impl Peek {
        #[derive(Clone, Copy)]
        struct ExprDisplay;
        impl Peek for ExprDisplay {
            fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("expression")
            }
            fn peek(&self, _: &mut crate::parser::Cursor) -> bool {
                unreachable!()
            }
        }
        ExprDisplay
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let left = if parser.peek(ExprLit::peek()) {
            parser.parse().map(Self::Lit)
        } else if parser.peek(Bracket) {
            parser.parse().map(Self::Array)
        } else if parser.peek(Brace) {
            parser.parse().map(Self::Object)
        } else if parser.peek(UnaryOp::peek()) {
            parser.parse().map(Self::Unary)
        } else if parser.peek(Ident) {
            parser.parse().map(Self::Funcall)
        } else if parser.peek(Paren) {
            parser.parse().map(Self::Paren)
        } else {
            return parser.unexpected_token(Self::peek());
        }?;
        if parser.peek(BinaryOp::peek()) {
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(left),
                op: parser.parse()?,
                right: parser.parse()?,
            }))
        } else {
            Ok(left)
        }
    }
}

define_ast_enum!(
    pub enum ExprLit {
        Bool(LitBool),
        Float(LitFloat),
        Integer(LitInteger),
        Null(Null),
        Regexp(LitRegexp),
        String(LitString),
    }
);

impl ExprLit {
    fn peek() -> impl Peek {
        LitBool
            .or(LitFloat)
            .or(LitInteger)
            .or(Null)
            .or(LitRegexp)
            .or(LitString)
    }
}

impl Parse for ExprLit {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(LitBool) {
            parser.parse().map(Self::Bool)
        } else if parser.peek(LitFloat) {
            parser.parse().map(Self::Float)
        } else if parser.peek(LitInteger) {
            parser.parse().map(Self::Integer)
        } else if parser.peek(Null) {
            parser.parse().map(Self::Null)
        } else if parser.peek(LitRegexp) {
            parser.parse().map(Self::Regexp)
        } else if parser.peek(LitString) {
            parser.parse().map(Self::String)
        } else {
            parser.unexpected_token(Self::peek())
        }
    }
}

define_ast_struct!(
    #[span = bracket_token]
    pub struct ExprArray {
        bracket_token: Bracket,
        elems: Terminated<Expr, Comma>,
    }
);

impl Parse for ExprArray {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (bracket_token, elems) = parser.parse_delimited()?;
        Ok(Self {
            bracket_token,
            elems,
        })
    }
}

define_ast_struct!(
    #[span = brace_token]
    pub struct ExprObject {
        brace_token: Brace,
        fields: Terminated<Field, Comma>,
    }
);

impl Parse for ExprObject {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (brace_token, fields) = parser.parse_delimited()?;
        Ok(Self {
            brace_token,
            fields,
        })
    }
}

define_ast_struct!(
    #[span = (path, value)]
    pub struct Field {
        path: Path,
        colon_token: Colon,
        value: Box<Expr>,
    }
);

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let path = parser.parse()?;
        let colon_token = parser.parse()?;
        let value = parser.parse()?;
        Ok(Self {
            path,
            colon_token,
            value,
        })
    }
}

define_ast_struct!(
    #[span = segments]
    pub struct Path {
        segments: Terminated<Key, Dot>,
    }
);

impl Parse for Path {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self {
            segments: parser.parse_seperated()?,
        })
    }
}

define_ast_enum!(
    pub enum Key {
        Array(LitInteger),
        Ident(Ident),
        String(LitString),
    }
);

impl Key {
    fn peek() -> impl Peek {
        LitInteger.or(Ident).or(LitString)
    }
}

impl Parse for Key {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(LitInteger) {
            parser.parse().map(Self::Array)
        } else if parser.peek(Ident) {
            parser.parse().map(Self::Ident)
        } else if parser.peek(LitString) {
            parser.parse().map(Self::String)
        } else {
            parser.unexpected_token(Self::peek())
        }
    }
}

define_ast_struct!(
    #[span = (op, elem)]
    pub struct ExprUnary {
        op: UnaryOp,
        elem: Box<Expr>,
    }
);

impl Parse for ExprUnary {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let op = parser.parse()?;
        let elem = parser.parse()?;
        Ok(Self { op, elem })
    }
}

define_ast_enum!(
    pub enum UnaryOp {
        Eq(EqEq),
        Ge(Ge),
        Gt(Gt),
        Le(Le),
        Lt(Lt),
        Neg(Minus),
        Not(Not),
    }
);

impl UnaryOp {
    fn peek() -> impl Peek {
        EqEq.or(Ge).or(Gt).or(Le).or(Lt).or(Minus).or(Not)
    }
}

impl Parse for UnaryOp {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(EqEq) {
            parser.parse().map(Self::Eq)
        } else if parser.peek(Ge) {
            parser.parse().map(Self::Ge)
        } else if parser.peek(Gt) {
            parser.parse().map(Self::Gt)
        } else if parser.peek(Le) {
            parser.parse().map(Self::Le)
        } else if parser.peek(Lt) {
            parser.parse().map(Self::Lt)
        } else if parser.peek(Minus) {
            parser.parse().map(Self::Neg)
        } else if parser.peek(Not) {
            parser.parse().map(Self::Not)
        } else {
            parser.unexpected_token(Self::peek())
        }
    }
}

define_ast_struct!(
    #[span = (left, right)]
    pub struct ExprBinary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    }
);

impl Parse for ExprBinary {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let left = parser.parse()?;
        let op = parser.parse()?;
        let right = parser.parse()?;
        Ok(Self { left, op, right })
    }
}

define_ast_enum!(
    pub enum BinaryOp {
        And(And),
        Or(Or),
    }
);

impl BinaryOp {
    fn peek() -> impl Peek {
        And.or(Or)
    }
}

impl Parse for BinaryOp {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(And) {
            parser.parse().map(Self::And)
        } else if parser.peek(Or) {
            parser.parse().map(Self::Or)
        } else {
            parser.unexpected_token(Self::peek())
        }
    }
}

define_ast_struct!(
    #[span = (ident, params)]
    pub struct ExprFuncall {
        ident: Ident,
        paren_token: Paren,
        params: Terminated<Expr, Comma>,
    }
);

impl Parse for ExprFuncall {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let ident = parser.parse()?;
        let (paren_token, params) = parser.parse_delimited()?;
        Ok(Self {
            ident,
            paren_token,
            params,
        })
    }
}

define_ast_struct!(
    #[span = paren_token]
    pub struct ExprParen {
        paren_token: Paren,
        elem: Box<Expr>,
    }
);

impl Parse for ExprParen {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (paren_token, elem) = parser.parse_delimited()?;
        Ok(Self { paren_token, elem })
    }
}
