use crate::{
    error::Result,
    parse::{Parse, Parser, Token},
};
use std::{fmt, marker::PhantomData};

#[derive(Clone)]
pub struct Terminated<T, P> {
    elems: Vec<(T, P)>,
    end: Option<Box<T>>,
}

impl<T, P> fmt::Debug for Terminated<T, P>
where
    T: fmt::Debug,
    P: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.elems.iter().map(|(t, _)| t).chain(self.end.as_deref()))
            .finish()
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
        parser.parse_terminated::<T, P>().collect()
    }
}

impl<T, P> FromIterator<Pair<T, P>> for Terminated<T, P> {
    fn from_iter<I: IntoIterator<Item = Pair<T, P>>>(iter: I) -> Self {
        let mut new = Self::new();
        new.extend(iter);
        new
    }
}

impl<T, P> Extend<Pair<T, P>> for Terminated<T, P> {
    fn extend<I: IntoIterator<Item = Pair<T, P>>>(&mut self, iter: I) {
        for pair in iter {
            match pair {
                Pair::Terminated(t, p) => self.elems.push((t, p)),
                Pair::End(e) => self.end = Some(Box::new(e)),
            }
        }
    }
}

impl<T, P> Terminated<T, P> {
    pub fn new() -> Self {
        Self {
            elems: Vec::new(),
            end: None,
        }
    }

    pub fn values(&self) -> impl '_ + Iterator<Item = &'_ T> {
        self.elems.iter().map(|(t, _)| t).chain(self.end.as_deref())
    }
}

#[derive(Clone, Debug)]
pub enum Pair<T, P> {
    Terminated(T, P),
    End(T),
}

pub struct TerminatedIter<'a, 'b, T, P> {
    pub(crate) parser: &'b mut Parser<'a>,
    pub(crate) _marker: PhantomData<(T, P)>,
}

impl<'a, 'b, T, P> Iterator for TerminatedIter<'a, 'b, T, P>
where
    T: Parse,
    P: Token,
{
    type Item = Result<Pair<T, P>>;

    fn next(&mut self) -> Option<Self::Item> {
        let parser = &mut *self.parser;
        tryb!({
            if parser.is_eos() {
                return Ok(None);
            }
            let t = parser.parse()?;
            if parser.is_eos() {
                return Ok(Some(Pair::End(t)));
            }
            Ok(Some(Pair::Terminated(t, parser.parse_token()?)))
        })
        .transpose()
    }
}
