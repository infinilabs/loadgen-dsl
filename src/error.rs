use crate::parse::{Peek, Span};
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

    pub fn new<S: Into<String>>(span: Span, msg: S) -> Self {
        Self::new_kind(span, ErrorKind::Custom(Box::from(msg.into())))
    }

    pub fn expected<S: Into<String>>(span: Span, expected: S) -> Self {
        Self::new_kind(span, ErrorKind::ExpectedType(Box::from(expected.into())))
    }

    pub fn expected_token<T: Peek>(span: Span, expected: T) -> Self {
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

    #[cfg(test)]
    pub(crate) fn into_iter(self) -> impl Iterator<Item = (Span, ErrorKind)> {
        self.inner.into_iter().map(|msg| (msg.span, msg.kind))
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
    InvalidEscape(char),
    UnterminatedString,
    UnterminatedRegexp,
    ExpectedType(Box<str>),
    Custom(Box<str>),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self {
            MissingDecimal => write!(f, "missing digits after the dot symbol"),
            MissingExponent => write!(f, "missing digits after the exponent symbol"),
            InvalidEscape(ch) => write!(f, "invalid character escape: `{ch}`"),
            UnterminatedString => write!(f, "unterminated string literal"),
            UnterminatedRegexp => write!(f, "unterminated regular expression"),
            ExpectedType(t) => write!(f, "expected {t}"),
            Custom(s) => write!(f, "{s}"),
        }
    }
}
