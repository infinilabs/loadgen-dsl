use std::{collections::VecDeque, ops};

use super::*;

pub(super) struct ParserBuffer<'a> {
    pub lexer: Lexer<'a>,
    buf: VecDeque<LexResult>,
}

impl<'a> ParserBuffer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            buf: VecDeque::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn reset(&mut self) {
        self.buf.clear();
    }

    pub fn grow(&mut self, len: usize) {
        self.buf.resize_with(len, || self.lexer.parse());
    }

    pub fn next(&mut self) -> LexResult {
        self.buf.pop_front().unwrap_or_else(|| self.lexer.parse())
    }
}

impl ops::Index<usize> for ParserBuffer<'_> {
    type Output = TokenKind;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buf[index].0
    }
}
