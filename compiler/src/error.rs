use crate::{lexer::Span, parser::Peek};
use miette::{Diagnostic, LabeledSpan, SourceSpan};
use std::fmt;
use thiserror::Error;

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

    pub(crate) fn with_source<S>(mut self, source: S) -> Self
    where
        S: Into<String>,
    {
        self.source_code.get_or_insert_with(|| source.into());
        self
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
        let Some(source_code) = self.source_code.as_deref() else {
            return None;
        };
        Some(Box::new(self.msgs.iter().map(|msg| {
            let source_len = source_code.len() as u32;
            let span = if msg.span == Span::dummy() {
                // For dummy span, returns the entire source code.
                Span::new(0, source_len)
            } else if msg.span.start >= source_len {
                // For EOF span, returns the last byte.
                Span::new(source_len - 1, source_len)
            } else {
                msg.span
            };
            LabeledSpan::new_with_span(Some(msg.desc.clone().into()), span)
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
