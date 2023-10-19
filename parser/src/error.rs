use crate::{lex::Span, parse::Peek};
use std::fmt;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct Error {
    inner: Vec<ErrorMessage>,
}

impl Error {
    pub(crate) fn new_kind(span: Span, kind: ErrorKind) -> Self {
        Self {
            inner: vec![ErrorMessage { span, kind }],
        }
    }

    pub fn new<S>(span: Span, msg: S) -> Self
    where
        S: Into<String>,
    {
        Self::new_kind(span, ErrorKind::Custom(Box::from(msg.into())))
    }

    pub fn expected<S>(span: Span, expected: S) -> Self
    where
        S: Into<String>,
    {
        Self::new_kind(span, ErrorKind::ExpectedToken(Box::from(expected.into())))
    }

    pub fn expected_token<T>(span: Span, expected: T) -> Self
    where
        T: Peek,
    {
        struct Display<T>(T);
        impl<T: Peek> fmt::Display for Display<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.display(f)
            }
        }
        Self::expected(span, Display(expected).to_string())
    }

    pub fn combine(&mut self, err: Self) {
        self.inner.extend(err.inner);
    }

    pub fn iter(&self) -> ErrorIter<'_> {
        ErrorIter {
            iter: self.inner.iter(),
        }
    }

    #[cfg(test)]
    pub(crate) fn into_iter(self) -> impl Iterator<Item = (Span, ErrorKind)> {
        self.inner.into_iter().map(|msg| (msg.span, msg.kind))
    }
}

pub struct ErrorIter<'a> {
    iter: std::slice::Iter<'a, ErrorMessage>,
}

impl Iterator for ErrorIter<'_> {
    type Item = (Span, String);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|msg| (msg.span, msg.kind.to_string()))
    }
}

#[derive(Debug)]
struct ErrorMessage {
    span: Span,
    kind: ErrorKind,
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
    MissingDecimal,
    MissingExponent,
    InvalidEscape,
    UnterminatedLiteral,
    ExpectedToken(Box<str>),
    Custom(Box<str>),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self {
            MissingDecimal => write!(f, "missing digits after the dot symbol"),
            MissingExponent => write!(f, "missing digits after the exponent symbol"),
            InvalidEscape => write!(f, "invalid character escape"),
            UnterminatedLiteral => write!(f, "unterminated literal"),
            ExpectedToken(t) => write!(f, "expected {t}"),
            Custom(s) => write!(f, "{s}"),
        }
    }
}
