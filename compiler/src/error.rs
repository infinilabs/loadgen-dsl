use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use crate::{lexer::Span, parser::Peek};
use std::fmt;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Error)]
#[error("compile errors")]
pub struct Error {
    pub(crate) msgs: Vec<ErrorMsg>,
    pub(crate) source_code: Option<String>,
}

impl Error {
    pub(crate) fn from_vec(msgs: Vec<ErrorMsg>) -> Self {
        Self {
            msgs,
            source_code: None,
        }
    }

    pub(crate) fn new<S>(span: Span, desc: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            msgs: vec![ErrorMsg {
                span,
                desc: Box::from(desc.into()),
            }],
            source_code: None,
        }
    }

    pub(crate) fn with_source(self, source: String) -> Self {
        Self {
            source_code: Some(source),
            ..self
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

impl Diagnostic for Error {
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.source_code.as_ref().map(|s| s as _)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.msgs.iter().map(|msg| {
            LabeledSpan::new_with_span(Some(msg.desc.clone().into()), msg.span)
        })))
    }
}

#[derive(Debug)]
pub(crate) struct ErrorMsg {
    pub span: Span,
    pub desc: Box<str>,
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::from((value.start as usize, (value.end - value.start) as usize))
    }
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
