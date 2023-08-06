use super::*;

pub(super) struct ParserBuffer<'a> {
    pub lexer: Lexer<'a>,
    buf: Vec<LexResult>,
    head: usize,
}

impl<'a> ParserBuffer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            buf: Vec::new(),
            head: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head >= self.buf.len()
    }

    pub fn gc(&mut self) {
        if self.is_empty() {
            self.head = 0;
            self.buf.clear();
        } else if self.head > 12 {
            debug_assert!(matches!(self.buf[self.head - 1].0, TokenKind::Empty));
            self.buf.drain(..self.head);
        }
    }

    pub fn reset(&mut self) {
        if !self.is_empty() {
            self.lexer.seek(self.get_token().span().start);
            self.buf.truncate(self.head);
        }
    }

    pub fn advance(&mut self) {
        debug_assert!(!self.is_empty());
        if self.is_empty() {
            self.push();
        }
        self.head += 1;
    }

    pub fn peek(&mut self, f: impl FnOnce(&mut Self) -> bool) -> bool {
        let head = self.head;
        let peek = f(self);
        self.head = head;
        peek
    }

    pub fn push(&mut self) {
        self.buf.push(self.lexer.parse());
    }

    pub fn pop(&mut self) -> LexResult {
        let head = self.head;
        self.head = head + 1;
        std::mem::replace(&mut self.buf[head], (TokenKind::Empty, None))
    }

    pub fn get_token(&self) -> &TokenKind {
        &self.buf[self.head].0
    }
}
