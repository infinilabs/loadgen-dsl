pub const EOF: u8 = b'\0';

pub struct Cursor<'a> {
    source: &'a str,
    iter: std::slice::Iter<'a, u8>,
}

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    start: u32,
    end: u32,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.as_bytes().iter(),
        }
    }

    /// Checks whether the cursor reaches EOF.
    pub fn is_eof(&self) -> bool {
        self.iter.as_slice().is_empty()
    }

    /// Returns the byte index of the next byte.
    pub fn pos(&self) -> u32 {
        (self.source.len() - self.iter.as_slice().len()) as u32
    }

    /// Returns a span from the specified position to the current position.
    pub fn span_from(&self, start: u32) -> Span {
        Span {
            start,
            end: self.pos(),
        }
    }

    /// Fetches the source code of given span.
    pub fn fetch(&self, span: Span) -> &str {
        &self.source[span.start as usize..span.end as usize]
    }

    /// Advances and returns the next byte.
    pub fn next(&mut self) -> u8 {
        self.iter.next().copied().unwrap_or(EOF)
    }

    /// Advances the cursor.
    pub fn advance(&mut self) {
        self.iter.next();
    }

    /// Looks at the next byte in the input stream.
    pub fn peek(&self) -> u8 {
        self.iter.clone().next().copied().unwrap_or(EOF)
    }

    /// Advances the cursor if the next byte statisfies the condition.
    pub fn skip_if(&mut self, p: fn(u8) -> bool) -> bool {
        if p(self.peek()) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Advances while the byte statisfies the condition or the input stream reaches EOF and
    /// returns whether any byte is consumed.
    pub fn skip_while(&mut self, p: fn(u8) -> bool) -> bool {
        if self.skip_if(p) {
            while !self.is_eof() && self.skip_if(p) {}
            true
        } else {
            false
        }
    }
}
