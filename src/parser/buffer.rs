use super::*;

pub(super) struct ParserBuffer<'a> {
    pub lexer: Lexer<'a>,
    buf: Vec<LexResult>,
    head: u32,
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
        self.head as usize >= self.buf.len()
    }

    pub fn clear(&mut self) {
        debug_assert!(self.is_empty());
        self.head = 0;
        self.buf.clear();
    }

    pub fn reset(&mut self) {
        self.lexer.seek(self.get_token().span().start);
        self.buf.truncate(self.head as usize);
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
        std::mem::replace(&mut self.buf[head as usize], (DUMMY, None))
    }

    pub fn get_token(&self) -> &TokenKind {
        &self.buf[self.head as usize].0
    }
}
