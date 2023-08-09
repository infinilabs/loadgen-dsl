use crate::{parse::Terminated, token::*};

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

#[derive(Clone, Debug)]
pub enum ExprLit {
    Null(Null),
    Bool(LitBool),
    Number(LitNumber),
    String(LitString),
    Regexp(LitRegexp),
}

#[derive(Clone, Debug)]
pub struct ExprArray {
    pub elems: Terminated<Expr, Comma>,
}

#[derive(Clone, Debug)]
pub struct ExprObject {
    pub brace_token: Brace,
    pub fields: Terminated<Field, Comma>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: FieldName,
    pub colon_token: Colon,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum FieldName {
    Ident(Terminated<Ident, Dot>),
    String(LitString),
}

#[derive(Clone, Debug)]
pub struct ExprUnary {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
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

#[derive(Clone, Debug)]
pub struct ExprBinary {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub righ: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    And(And),
    Or(Or),
}

#[derive(Clone, Debug)]
pub struct ExprFuncall {
    pub ident: Ident,
    pub paren_token: Paren,
    pub params: Terminated<Expr, Comma>,
}

#[derive(Clone, Debug)]
pub struct ExprParen {
    pub paren_token: Paren,
    pub elem: Box<Expr>,
}
