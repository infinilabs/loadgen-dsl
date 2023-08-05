use crate::parser::Span;
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

    pub fn unexpected<S: Into<String>>(span: Span, expected: S) -> Self {
        Self::new_kind(span, ErrorKind::Unexpected(Box::from(expected.into())))
    }

    pub fn combine(&mut self, err: Self) {
        self.inner.extend(err.inner);
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
    UnterminatedString,
    UnterminatedRegexp,
    Unexpected(Box<str>),
    Custom(Box<str>),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self {
            MissingDecimal => write!(f, "missing digits after the dot symbol"),
            MissingExponent => write!(f, "missing digits after the exponent symbol"),
            UnterminatedString => write!(f, "unterminated string literal"),
            UnterminatedRegexp => write!(f, "unterminated regular expression"),
            Unexpected(t) => write!(f, "expected {t}"),
            Custom(s) => write!(f, "{s}"),
        }
    }
}
