use crate::lexer::Span;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct Error {
    pub(crate) span: Span,
    pub(crate) kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    MissingDecimal,
    MissingExponent,
    UnexptedEof,
}
