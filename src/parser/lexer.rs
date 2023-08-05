use super::*;

const EOF: char = '\0';

macro_rules! define_pattern {
    ($name:ident, $pat:pat) => {
        #[allow(unused)]
        macro_rules! $name {
            () => {
                $pat
            };
        }
        #[allow(unused)]
        fn $name(ch: char) -> bool {
            matches!(ch, $pat)
        }
    };
}
define_pattern!(whitespace, '\x09' | '\x0A' | '\x0D' | '\x20');
define_pattern!(digit, '0'..='9');
define_pattern!(ident_start, '_' | 'a'..='z' | 'A' ..='Z');
define_pattern!(ident_body, ident_start!() | '-' | digit!());
define_pattern!(
    punct,
    '!' | '#'
        | '$'
        | '%'
        | '&'
        | '*'
        | '+'
        | ','
        | '-'
        | '.'
        | '/'
        | ':'
        | ';'
        | '<'
        | '='
        | '>'
        | '?'
        | '@'
        | '['
        | ']'
        | '^'
        | '{'
        | '}'
        | '|'
        | '~'
);

pub(super) type LexResult<T = TokenKind> = (T, Option<Error>);

pub(super) struct Lexer<'a> {
    cur: Cursor<'a>,
    /// Starting position in a parse.
    start: u32,
    /// Characters buffered in a parse.
    buf: String,
    /// Possbile errors in a parse.
    err: Option<Error>,
    flag: u8,
}

impl<'a> Lexer<'a> {
    pub const ALLOW_REGEXP: u8 = 0b00000001;

    pub fn new(source: &'a str) -> Self {
        Self {
            cur: Cursor::new(source),
            start: 0,
            buf: String::new(),
            err: None,
            flag: 0,
        }
    }

    pub fn parse(&mut self) -> LexResult {
        loop {
            // buffer/error should be taken in the last parse.
            debug_assert!(self.buf.is_empty());
            debug_assert!(self.err.is_none());
            self.start = self.cur.pos();
            let ch = self.cur.next();
            let token = match ch {
                // skip whitespace
                whitespace!() => {
                    self.skip_whitespace();
                    continue;
                }
                ident_start!() => self.next_ident(ch),
                digit!() => self.next_number(ch),
                '"' | '\'' => self.next_quoted(ch),
                // skip comment
                '/' if self.skip_if(|ch| ch == '/') => {
                    self.skip_line();
                    continue;
                }
                '/' if self.flag & Self::ALLOW_REGEXP != 0 => self.next_regexp(ch),
                punct!() => Punct {
                    value: ch,
                    span: self.span(),
                    joint: !self.cur.is_eof() && !self.skip_if(whitespace),
                }
                .into(),
                _ if !self.cur.is_eof() => Unknown { span: self.span() }.into(),
                _ => Eof { span: self.span() }.into(),
            };
            self.flag = 0;
            break (token, self.take_error());
        }
    }

    /// Changes the runtime behavior.
    pub fn set_flag(&mut self, flag: u8) {
        self.flag |= flag;
    }

    /// Moves the cursor to the specified position.
    pub fn seek(&mut self, pos: u32) {
        self.cur.seek(pos);
    }

    fn next_ident(&mut self, _start: char) -> TokenKind {
        self.skip_while(ident_body);
        Ident {
            value: self.source().into(),
            span: self.span(),
        }
        .into()
    }

    fn next_number(&mut self, _leading: char) -> TokenKind {
        debug_assert!(digit(_leading));
        // leading digits
        self.skip_digits();
        // decimal digits
        if self.skip_if(|ch| ch == '.') && !self.skip_while(digit) {
            self.error(self.span(), MissingDecimal);
        }
        // exponent digits
        if self.skip_if(|ch| matches!(ch, 'e' | 'E')) {
            self.skip_if(|ch| matches!(ch, '+' | '-'));
            if !self.skip_while(digit) {
                self.error(self.span(), MissingExponent);
            }
        }
        LitNumber {
            span: self.span(),
            value: if self.err.is_some() {
                f64::MAX
            } else {
                self.source().parse().unwrap()
            },
        }
        .into()
    }

    fn next_quoted(&mut self, quote: char) -> TokenKind {
        debug_assert!(matches!(quote, '"' | '\'' | '/'));
        loop {
            // TODO: handle escape characters
            match self.cur.next() {
                '\\' => self.cur.advance(),
                ch if ch == quote => break,
                ch if !self.cur.is_eof() => self.buf.push(ch),
                _ => {
                    self.error(self.span(), UnterminatedString);
                    break;
                }
            }
        }
        LitString {
            span: self.span(),
            value: self.take_buf(),
        }
        .into()
    }

    fn next_regexp(&mut self, quote: char) -> TokenKind {
        debug_assert!(matches!(quote, '/'));
        loop {
            match self.cur.next() {
                '\\' => self.cur.advance(),
                ch if ch == quote => break,
                ch if !self.cur.is_eof() => self.buf.push(ch),
                _ => {
                    self.error(self.span(), UnterminatedRegexp);
                    break;
                }
            }
        }
        LitRegexp {
            span: self.span(),
            value: self.take_buf(),
        }
        .into()
    }

    fn error(&mut self, span: Span, e: ErrorKind) {
        let e = Error::new_kind(span, e);
        if let Some(err) = self.err.as_mut() {
            err.combine(e);
        } else {
            self.err = Some(e)
        }
    }

    fn take_error(&mut self) -> Option<Error> {
        self.err.take()
    }

    fn take_buf(&mut self) -> Box<str> {
        let buf = Box::from(&*self.buf);
        self.buf.clear();
        buf
    }

    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.cur.pos(),
        }
    }

    fn source(&self) -> &str {
        self.cur.fetch(self.start, self.cur.pos())
    }

    fn skip_if(&mut self, p: fn(char) -> bool) -> bool {
        if p(self.cur.next()) {
            true
        } else {
            self.cur.back();
            false
        }
    }

    fn skip_while(&mut self, p: fn(char) -> bool) -> bool {
        if !p(self.cur.next()) {
            self.cur.back();
            return false;
        }
        loop {
            if self.cur.is_eof() {
                break;
            } else if !p(self.cur.next()) {
                self.cur.back();
                break;
            }
        }
        true
    }

    fn skip_line(&mut self) {
        self.skip_while(|ch| !matches!(ch, '\n'));
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(whitespace);
    }

    fn skip_digits(&mut self) {
        self.skip_while(digit);
    }
}

struct Cursor<'a> {
    src: &'a str,
    iter: std::str::Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            src: source,
            iter: source.chars(),
        }
    }

    /// Checks whether this cursor reaches EOF.
    pub fn is_eof(&self) -> bool {
        self.iter.as_str().is_empty()
    }

    /// Returns the byte index of the next character.
    pub fn pos(&self) -> u32 {
        (self.src.len() - self.iter.as_str().len()) as u32
    }

    /// Returns the source in the given [`Span`].
    pub fn fetch(&self, start: u32, end: u32) -> &str {
        &self.src[start as usize..end as usize]
    }

    /// Advances and returns the next character. [`EOF`] is returned when reaches EOF.
    pub fn next(&mut self) -> char {
        self.iter.next().unwrap_or(EOF)
    }

    /// Moves the cursor to the specifed position.
    pub fn seek(&mut self, pos: u32) {
        self.iter = self.src[pos as usize..].chars();
    }

    /// Moves the cursor to the previous character.
    pub fn back(&mut self) {
        let mut iter = self.src[..self.pos() as usize].chars();
        iter.next_back();
        self.iter = self.src[iter.as_str().len()..].chars();
    }

    /// Moves the cursor to the next character.
    pub fn advance(&mut self) {
        self.iter.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cursor() {
        let mut cursor = Cursor::new("xy");
        assert_eq!(cursor.pos(), 0);
        assert_eq!(cursor.next(), 'x');
        assert_eq!(cursor.pos(), 1);
        assert_eq!(cursor.next(), 'y');
        assert_eq!(cursor.next(), EOF);
        assert_eq!(cursor.pos(), 2);
        cursor.back();
        assert_eq!(cursor.next(), 'y');
        assert!(cursor.is_eof());
    }
}
