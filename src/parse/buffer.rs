use super::*;

pub(super) struct ParseBuffer<'a> {
    lexer: Lexer<'a>,
    buf: VecDeque<LexResult>,
}

impl<'a> ParseBuffer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            buf: VecDeque::with_capacity(4),
        }
    }

    /// Returns whether there are remaining tokens.
    pub fn is_empty(&self) -> bool {
        self.lexer.is_eof() && matches!(self.buf.front(), None | Some((TokenKind::Eof(_), _)))
    }

    /// Sets the given `flag`, then clears the buffer and seeks the lexer to the start of the first
    /// peeked [`TokenKind`].
    pub fn reset(&mut self, flag: LexFlag) {
        self.lexer.set_flag(flag);
        let Some((first, _)) = self.buf.front() else { return; };
        self.lexer.seek(first.span().start);
        self.buf.clear();
    }

    /// Creates a [`Cursor`] for peeking.
    pub fn cursor<'b>(&'b mut self, head: &'b mut usize) -> Cursor<'a, '_> {
        self.fill(*head + 1);
        Cursor { buf: self, head }
    }

    pub fn parse_punct(&mut self, display: &str) -> Option<Span> {
        let mut head = 0;
        if self.cursor(&mut head).peek_punct(display) {
            let span = self[0].span().join(self[head].span());
            drop(self.buf.drain(..=head));
            Some(span)
        } else {
            None
        }
    }

    /// Fills the buffer to fit the specified length.
    pub fn fill(&mut self, len: usize) {
        self.buf
            .resize_with(self.buf.len().max(len), || self.lexer.parse());
    }

    /// Removes or parses the next [`TokenKind`].
    pub fn next(&mut self) -> LexResult {
        self.buf.pop_front().unwrap_or_else(|| self.lexer.parse())
    }
}

impl ops::Index<usize> for ParseBuffer<'_> {
    type Output = TokenKind;

    fn index(&self, i: usize) -> &Self::Output {
        &self.buf[i].0
    }
}

pub struct Cursor<'a, 'b> {
    buf: &'b mut ParseBuffer<'a>,
    head: &'b mut usize,
}

impl<'a, 'b> Cursor<'a, 'b> {
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
        self.buf.fill(*self.head + 1);
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

    /// Returns whether it points to a joint punctuation sequence.
    ///
    /// # Panics
    ///
    /// Panics if the punctuation is empty.
    pub fn peek_punct(self, display: &str) -> bool {
        let mut cur = self;
        let mut chars = display.chars();
        let Some(mut ch) = chars.next() else {
            panic!("a punctuation must not be empty");
        };
        loop {
            // not a punctuation
            let Some(p) = cur.get_punct() else { break };
            // not matches
            if p.value != ch {
                break;
            }
            // all match
            let Some(next) = chars.next() else { return true };
            // not joint
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
    fn peek(&self, cur: Cursor) -> bool {
        let Cursor { buf, head } = cur;
        self.a.peek(Cursor { buf, head }) && self.b.peek(Cursor { buf, head }.advance())
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.a.display(f)
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
    fn peek(&self, cur: Cursor) -> bool {
        let Cursor { buf, head } = cur;
        let current_head = *head;
        self.a.peek(Cursor { buf, head }) || {
            *head = current_head;
            self.b.peek(Cursor { buf, head })
        }
    }
    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.a.display(f)?;
        f.write_str(" or ")?;
        self.b.display(f)
    }
}

pub struct Not<T>(pub T);

impl<T: Peek> Peek for Not<T> {
    fn peek(&self, cur: Cursor) -> bool {
        !self.0.peek(cur)
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("not ")?;
        self.0.display(f)
    }
}

pub struct Any;

impl Peek for Any {
    fn peek(&self, _: Cursor) -> bool {
        true
    }

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any token")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! parse_next {
        ($buffer:expr, $ty:ident) => {
            let (token, err) = $buffer.next();
            assert!(err.is_none());
            assert!(<$ty>::try_from(token).is_ok());
        };
    }

    #[test]
    fn is_empty() {
        let mut buffer = ParseBuffer::new("abc 123 'xyz'");
        parse_next!(buffer, Ident);
        parse_next!(buffer, LitNumber);
        parse_next!(buffer, LitString);
        // Lexer reaches EOF
        assert!(buffer.lexer.is_eof());
        assert!(buffer.buf.is_empty());
        assert!(buffer.is_empty());
        let mut buf = ParseBuffer::new("abc 123 'xyz'");
        buf.fill(3);
        // Lexer reaches EOF, but buffer is not empty
        assert!(buf.lexer.is_eof());
        assert!(!buf.buf.is_empty());
        assert!(!buf.is_empty());
        parse_next!(buf, Ident);
        parse_next!(buf, LitNumber);
        parse_next!(buf, LitString);
        // now buffer is empty
        assert!(buf.buf.is_empty());
        assert!(buf.is_empty());
    }

    #[test]
    fn reset() {
        let mut buf = ParseBuffer::new("/.*/");
        buf.fill(1);
        parse_next!(buf, Punct);
        let mut buffer = ParseBuffer::new("/.*/");
        buffer.fill(1);
        buffer.reset(Lexer::ALLOW_REGEXP);
        parse_next!(buffer, LitRegexp);
    }

    #[test]
    fn peek_punct() {
        let mut buf = ParseBuffer::new("<< =>");
        assert!(buf.cursor(&mut 0).peek_punct("<"));
        assert!(buf.cursor(&mut 0).peek_punct("<<"));
        assert!(!buf.cursor(&mut 0).peek_punct("<<="));
        let mut head = 0;
        assert!(buf.cursor(&mut head).peek_punct("<<"));
        assert!(buf.cursor(&mut head).advance().peek_punct("=>"));
    }
}
