use std::{collections::VecDeque, ops};

use super::*;

pub(super) struct ParserBuffer<'a> {
    lexer: Lexer<'a>,
    buf: VecDeque<LexResult>,
}

impl<'a> ParserBuffer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            buf: VecDeque::new(),
        }
    }

    pub fn is_eof(&self) -> bool {
        self.lexer.is_empty() && matches!(self.buf.front(), None | Some((TokenKind::Eof(_), _)))
    }

    /// [`Lexer::set_flag`]
    pub fn set_flag(&mut self, flag: u8) {
        self.lexer.set_flag(flag);
    }

    pub fn reset(&mut self) {
        let Some((first, _)) = self.buf.front() else { return; };
        self.lexer.seek(first.span().start);
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

    fn index(&self, i: usize) -> &Self::Output {
        &self.buf[i].0
    }
}

pub struct Cursor<'a, 'b> {
    buf: &'b mut ParserBuffer<'a>,
    head: &'b mut usize,
}

impl<'a, 'b> Cursor<'a, 'b> {
    pub(super) fn new(buf: &'b mut ParserBuffer<'a>, head: &'b mut usize) -> Self {
        debug_assert_eq!(*head, 0);
        Self { buf, head }
    }

    pub(super) fn get_token(&self) -> &TokenKind {
        &self.buf[*self.head]
    }

    pub(super) fn get_token_as<T>(&self) -> Option<&T>
    where
        for<'t> &'t T: TryFrom<&'t TokenKind>,
    {
        self.get_token().try_into().ok()
    }

    pub fn advance(self) -> Self {
        *self.head += 1;
        self.buf.grow(*self.head);
        Self { ..self }
    }

    pub fn get_ident(&self) -> Option<&Ident> {
        self.get_token_as()
    }

    pub fn get_number(&self) -> Option<&LitNumber> {
        self.get_token_as()
    }

    pub fn get_sting(&self) -> Option<&LitString> {
        self.get_token_as()
    }

    pub fn get_punct(&self) -> Option<&Punct> {
        self.get_token_as()
    }

    pub fn peek_punct(self, display: &str) -> bool {
        let mut cur = self;
        let mut chars = display.chars();
        let Some(mut ch) = chars.next() else { return false };
        loop {
            let Some(p) = cur.get_punct() else { break };
            if p.value != ch {
                break;
            }
            let Some(next) = chars.next() else { break };
            if !p.joint {
                break;
            }
            cur = cur.advance();
            ch = next;
        }
        false
    }
}

pub struct And<A, B> {
    pub(super) a: A,
    pub(super) b: B,
}

impl<A, B> Peek for And<A, B>
where
    A: Peek,
    B: Peek,
{
    fn peek(self, cur: Cursor) -> bool {
        let Cursor { buf, head } = cur;
        self.a.peek(Cursor { buf, head }) && self.b.peek(Cursor { buf, head }.advance())
    }
}

pub struct Or<A, B> {
    pub(super) a: A,
    pub(super) b: B,
}

impl<A, B> Peek for Or<A, B>
where
    A: Peek,
    B: Peek,
{
    fn peek(self, cur: Cursor) -> bool {
        let Cursor { buf, head } = cur;
        let current_head = *head;
        self.a.peek(Cursor { buf, head }) || {
            *head = current_head;
            self.b.peek(Cursor { buf, head })
        }
    }
}
