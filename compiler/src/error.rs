use crate::{lexer::Span, parser::Peek};
use std::fmt;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct Error {
    pub(crate) msgs: Vec<ErrorMsg>,
}

impl Error {
    pub(crate) fn new<S>(span: Span, kind: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            msgs: vec![ErrorMsg {
                span,
                kind: Box::from(kind.into()),
            }],
        }
    }

    pub(crate) fn combine(&mut self, other: Error) {
        self.msgs.extend(other.msgs);
    }

    pub(crate) fn expected_token<P>(span: Span, p: P) -> Self
    where
        P: Peek,
    {
        Self::new(span, format!("expected {}", DisplayPeek(p)))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for msg in self.msgs.iter() {
            writeln!(f, "{} at {}:{}", msg.kind, msg.span.start, msg.span.end)?;
        }
        Ok(())
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub(crate) struct ErrorMsg {
    pub span: Span,
    pub kind: Box<str>,
}

struct DisplayPeek<P>(P);

impl<P> fmt::Display for DisplayPeek<P>
where
    P: Peek,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display(f)
    }
}
