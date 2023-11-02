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

// TODO: make comma optional
#[derive(Clone, Debug)]
pub struct Terminated<T, P> {
    pub(crate) pairs: Vec<(T, P)>,
    pub(crate) trailing: Option<Box<T>>,
}

impl<T, P> Terminated<T, P> {
    pub fn new() -> Self {
        Self {
            pairs: Vec::new(),
            trailing: None,
        }
    }

    pub(crate) fn parse_rest(&mut self, parser: &mut Parser) -> Result<()>
    where
        T: Parse,
        P: Token,
    {
        debug_assert!(self.trailing.is_none());
        loop {
            if parser.is_eot() {
                break;
            }
            let val = parser.parse()?;
            if parser.is_eot() {
                self.trailing = Some(Box::new(val));
                break;
            }
            self.pairs.push((val, parser.parse()?));
        }
        Ok(())
    }

    pub fn items(&self) -> impl Iterator<Item = &T> {
        self.pairs
            .iter()
            .map(|(t, _)| t)
            .chain(self.trailing.as_deref())
    }
}

impl<T, P> Default for Terminated<T, P> {
    fn default() -> Self {
        Self::new()
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
        let Some(start) = self.pairs.first() else {
            if let Some(ref trailing) = self.trailing {
                return trailing.span();
            } else {
                return Span::dummy();
            }
        };
        let end = self
            .trailing
            .as_deref()
            .map(T::span)
            .or_else(|| self.pairs.last().map(|(_, p)| p.span()))
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

        $(impl From<$ty> for $name {
            fn from(val: $ty) -> Self {
                $name::$var(val)
            }
        }
        impl TryFrom<$name> for $ty {
            type Error = $name;

            fn try_from(val: $name) -> Result<Self, $name> {
                match val {
                    $name::$var(val) => Ok(val),
                    _ => Err(val),
                }
            }
        })*
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
        } else if parser.is_eot() {
            Ok(Self::Brief(DslBrief {
                status: None,
                body: None,
            }))
        } else {
            parser.unexpected_token(Self::peek())
        }
    }
}

define_ast_struct!(
    #[span = _span]
    pub struct DslBrief {
        status: Option<LitInteger>,
        body: Option<ExprObject>,
    }
);

impl DslBrief {
    fn _span(&self) -> Span {
        match (
            self.status.as_ref().map(<_>::span),
            self.body.as_ref().map(<_>::span),
        ) {
            (Some(start), Some(end)) => start.join(end),
            (Some(span), _) | (_, Some(span)) => span,
            (None, None) => Span::dummy(),
        }
    }
}

impl Parse for DslBrief {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self {
            status: parser.parse()?,
            body: if parser.peek(Brace) {
                Some(parser.parse()?)
            } else {
                None
            },
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
        Tuple(ExprTuple),
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

    fn parse_one(parser: &mut Parser) -> Result<Self> {
        if parser.peek(Bracket) {
            parser.parse().map(Self::Array)
        } else if parser.peek(Brace) {
            parser.parse().map(Self::Object)
        } else if parser.peek(Paren) {
            Self::parse_paren_or_tuple(parser)
        } else if parser.peek(UnaryOp::peek()) {
            parser.parse().map(Self::Unary)
        } else if parser.peek(ExprLit::peek()) {
            parser.parse().map(Self::Lit)
        } else if parser.peek(Ident) {
            parser.parse().map(Self::Funcall)
        } else {
            parser.unexpected_token(Self::peek())
        }
    }

    fn parse_paren_or_tuple(parser: &mut Parser) -> Result<Self> {
        let mut parser = parser.delimited()?;
        if parser.is_eot() {
            return Ok(ExprTuple {
                paren_token: parser.finish()?,
                elems: Terminated::new(),
            }
            .into());
        }
        let elem = parser.parse::<Expr>()?;
        Ok(if let Some(comma) = parser.parse::<Option<Comma>>()? {
            let mut elems = Terminated {
                pairs: vec![(elem, comma)],
                trailing: None,
            };
            elems.parse_rest(&mut parser)?;
            ExprTuple {
                elems,
                paren_token: parser.finish()?,
            }
            .into()
        } else {
            ExprParen {
                elem: Box::new(elem),
                paren_token: parser.finish()?,
            }
            .into()
        })
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let left = Self::parse_one(parser)?;
        if parser.peek(BinaryOp::peek()) {
            let (op, right) = ExprBinary::parse_next(parser)?;
            ExprBinary {
                left: left.into(),
                op,
                right: right.into(),
            }
            .parse_rest(parser)
            .map(Self::Binary)
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
        let elem = Expr::parse_one(parser)?.into();
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

impl ExprBinary {
    fn parse_next(parser: &mut Parser) -> Result<(BinaryOp, Expr)> {
        Ok((parser.parse::<BinaryOp>()?, Expr::parse_one(parser)?))
    }

    fn parse_rest(self, parser: &mut Parser) -> Result<Self> {
        let mut left = self.left;
        let mut op = self.op;
        let mut right = self.right;

        loop {
            if !parser.peek(BinaryOp::peek()) {
                break;
            }
            let (next_op, next_right) = Self::parse_next(parser)?;
            let next_right = Box::new(next_right);

            if op.precedence() >= next_op.precedence() {
                left = Box::new(Self { left, op, right }.into());
                op = next_op;
                right = next_right;
            } else {
                right = Box::new(
                    Self {
                        left: right,
                        op: next_op,
                        right: next_right,
                    }
                    .parse_rest(parser)?
                    .into(),
                );
                break;
            }
        }

        Ok(Self { left, op, right })
    }
}

impl Parse for ExprBinary {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let left = Expr::parse_one(parser)?;
        let (op, right) = Self::parse_next(parser)?;
        Self {
            left: left.into(),
            op,
            right: right.into(),
        }
        .parse_rest(parser)
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

    fn precedence(&self) -> usize {
        match self {
            Self::And(_) => 1,
            Self::Or(_) => 0,
        }
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
    #[span = (ident, paren_token)]
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

define_ast_struct!(
    #[span = paren_token]
    pub struct ExprTuple {
        paren_token: Paren,
        elems: Terminated<Expr, Comma>,
    }
);

impl Parse for ExprTuple {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (paren_token, elems) = parser.parse_delimited_with(|parser| {
            let mut elems = Terminated::new();

            if parser.is_eot() {
                return Ok(elems);
            }
            elems.pairs.push((parser.parse()?, parser.parse()?));
            elems.parse_rest(parser)?;

            Ok(elems)
        })?;
        Ok(Self { paren_token, elems })
    }
}
