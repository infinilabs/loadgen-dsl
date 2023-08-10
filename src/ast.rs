use crate::{
    error::{Error, Result},
    parse::{Pair, Parse, Parser, Peek, Terminated},
    token::*,
};

#[derive(Clone, Debug)]
pub enum Expr {
    Lit(ExprLit),
    Array(ExprArray),
    Object(ExprObject),
    Unary(ExprUnary),
    Binary(ExprBinary),
    Funcall(ExprFuncall),
    Paren(ExprParen),
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
            return Err(Error::expected(parser.span(), "expression"));
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

#[derive(Clone, Debug)]
pub enum ExprLit {
    Null(Null),
    Bool(LitBool),
    Number(LitNumber),
    String(LitString),
    Regexp(LitRegexp),
}

impl Parse for ExprLit {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(Null) {
            parser.parse().map(Self::Null)
        } else if parser.peek(LitBool) {
            parser.parse().map(Self::Bool)
        } else if parser.peek(LitNumber) {
            parser.parse().map(Self::Number)
        } else if parser.peek(LitString) {
            parser.parse().map(Self::String)
        } else if parser.peek(LitRegexp) {
            parser.parse().map(Self::Regexp)
        } else {
            Err(Error::expected_token(parser.span(), Self::peek()))
        }
    }
}

impl ExprLit {
    fn peek() -> impl Peek {
        Null.or(LitBool).or(LitNumber).or(LitString).or(LitRegexp)
    }
}

#[derive(Clone, Debug)]
pub struct ExprArray {
    pub bracket_token: Bracket,
    pub elems: Terminated<Expr, Comma>,
}

impl Parse for ExprArray {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (bracket_token, elems, _) = parser.parse_delimited()?;
        Ok(Self {
            bracket_token,
            elems,
        })
    }
}

#[derive(Clone, Debug)]
pub struct ExprObject {
    pub brace_token: Brace,
    pub fields: Terminated<Field, Comma>,
}

impl Parse for ExprObject {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (brace_token, fields, _) = parser.parse_delimited()?;
        Ok(Self {
            brace_token,
            fields,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: FieldName,
    pub colon_token: Colon,
    pub value: Box<Expr>,
}

impl Parse for Field {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let name = parser.parse()?;
        let colon_token = parser.parse()?;
        let value = parser.parse()?;
        Ok(Self {
            name,
            colon_token,
            value,
        })
    }
}

#[derive(Clone, Debug)]
pub enum FieldName {
    Ident(Terminated<Ident, Dot>),
    String(LitString),
}

impl Parse for FieldName {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(Ident) {
            let mut ident = Some(parser.parse::<Ident>()?);
            std::iter::from_fn(|| {
                tryb!({
                    let Some(id) = ident.take() else { return Ok(None) };
                    if let Some(dot) = parser.parse::<Option<Dot>>()? {
                        ident = Some(parser.parse()?);
                        Ok(Some(Pair::Terminated(id, dot)))
                    } else {
                        Ok(Some(Pair::End(id)))
                    }
                })
                .transpose()
            })
            .collect::<Result<_>>()
            .map(Self::Ident)
        } else if parser.peek(LitString) {
            parser.parse().map(Self::String)
        } else {
            Err(Error::expected_token(parser.span(), Self::peek()))
        }
    }
}

impl FieldName {
    fn peek() -> impl Peek {
        Ident.or(LitString)
    }
}

#[derive(Clone, Debug)]
pub struct ExprUnary {
    pub op: UnaryOp,
    pub elem: Box<Expr>,
}

impl Parse for ExprUnary {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let op = parser.parse()?;
        let elem = parser.parse()?;
        Ok(Self { op, elem })
    }
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Neg(Minus),
    Gt(Gt),
    Lt(Lt),
    Ge(Ge),
    Le(Le),
    Eq(Eq),
    Not(Not),
}

impl Parse for UnaryOp {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(Minus) {
            parser.parse().map(Self::Neg)
        } else if parser.peek(Eq) {
            parser.parse().map(Self::Eq)
        } else if parser.peek(Ge) {
            parser.parse().map(Self::Ge)
        } else if parser.peek(Le) {
            parser.parse().map(Self::Le)
        } else if parser.peek(Gt) {
            parser.parse().map(Self::Gt)
        } else if parser.peek(Lt) {
            parser.parse().map(Self::Lt)
        } else if parser.peek(Not) {
            parser.parse().map(Self::Not)
        } else {
            Err(Error::expected_token(parser.span(), Self::peek()))
        }
    }
}

impl UnaryOp {
    fn peek() -> impl Peek {
        Minus.or(Eq).or(Ge).or(Le).or(Gt).or(Lt).or(Not)
    }
}

#[derive(Clone, Debug)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

impl Parse for ExprBinary {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let left = parser.parse()?;
        let op = parser.parse()?;
        let right = parser.parse()?;
        Ok(Self { left, op, right })
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    And(And),
    Or(Or),
}

impl Parse for BinaryOp {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.peek(And) {
            parser.parse().map(Self::And)
        } else if parser.peek(Or) {
            parser.parse().map(Self::Or)
        } else {
            Err(Error::expected_token(parser.span(), Self::peek()))
        }
    }
}

impl BinaryOp {
    fn peek() -> impl Peek {
        And.or(Or)
    }
}

#[derive(Clone, Debug)]
pub struct ExprFuncall {
    pub ident: Ident,
    pub paren_token: Paren,
    pub params: Terminated<Expr, Comma>,
}

impl Parse for ExprFuncall {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let ident = parser.parse()?;
        let (paren_token, params, _) = parser.parse_delimited()?;
        Ok(Self {
            ident,
            paren_token,
            params,
        })
    }
}

#[derive(Clone, Debug)]
pub struct ExprParen {
    pub paren_token: Paren,
    pub elem: Box<Expr>,
}

impl Parse for ExprParen {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let (paren_token, elem, _) = parser.parse_delimited()?;
        Ok(Self { paren_token, elem })
    }
}
