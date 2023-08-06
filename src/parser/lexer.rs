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

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub(crate) start: u32,
    pub(crate) end: u32,
}

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
                EOF if self.cur.is_eof() => Eof { span: self.span() }.into(),
                _ => Unknown { span: self.span() }.into(),
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

    pub fn is_empty(&self) -> bool {
        self.cur.is_eof()
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
            match self.cur.next() {
                '\\' => {
                    let start = self.cur.pos();
                    let ch = self.cur.next();
                    match ch {
                        'b' => self.buf.push('\x08'),
                        'f' => self.buf.push('\x0c'),
                        'n' => self.buf.push('\n'),
                        'r' => self.buf.push('\r'),
                        't' => self.buf.push('\t'),
                        '\'' | '"' | '\\' | '/' => self.buf.push(ch),
                        _ => self.error(self.span_from(start - 1), InvalidEscape(ch)),
                    }
                }
                ch if ch == quote => break,
                EOF if self.cur.is_eof() => {
                    self.error(self.span(), UnterminatedString);
                    break;
                }
                ch => self.buf.push(ch),
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
                '\\' if self.skip_if(|ch| ch == '/') => self.buf.push('/'),
                ch if ch == quote => break,
                EOF if self.cur.is_eof() => {
                    self.error(self.span(), UnterminatedRegexp);
                    break;
                }
                ch => self.buf.push(ch),
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
        self.span_from(self.start)
    }

    fn span_from(&self, start: u32) -> Span {
        Span {
            start,
            end: self.cur.pos(),
        }
    }

    fn source(&self) -> &str {
        self.cur.fetch(self.start, self.cur.pos())
    }

    fn skip_if(&mut self, p: fn(char) -> bool) -> bool {
        if p(self.cur.peek()) {
            self.cur.advance();
            true
        } else {
            false
        }
    }

    fn skip_while(&mut self, p: fn(char) -> bool) -> bool {
        if self.skip_if(p) {
            while !self.cur.is_eof() && self.skip_if(p) {}
            true
        } else {
            false
        }
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

    /// Looks at the next character in the stream.
    pub fn peek(&mut self) -> char {
        self.iter.clone().next().unwrap_or(EOF)
    }

    /// Moves the cursor to the specifed position.
    pub fn seek(&mut self, pos: u32) {
        self.iter = self.src[pos as usize..].chars();
    }

    /// Moves the cursor to the next character.
    pub fn advance(&mut self) {
        self.iter.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! lex_err {
        ($res:expr, $(($start:literal, $end:literal) $err:pat),+ $(,)?) => {{
            let (_, result) = $res;
            let mut err = result.unwrap().into_iter();
            $({
                let (span, e) = err.next().unwrap();
                assert_eq!((span.start, span.end), ($start, $end));
                assert!(matches!(e, $err));
            })*
        }};
    }

    macro_rules! lex_ok {
        ($res:expr, ($start:literal, $end:literal) $ty:ident) => {{
            let (token, result) = $res;
            let span = token.span();
            assert_eq!((span.start, span.end), ($start, $end));
            assert!(result.is_none());
            <$ty>::try_from(token).unwrap()
        }};
    }

    macro_rules! lex_ident_eq {
        ($res:expr, $span:tt $val:literal) => {
            let token = lex_ok!($res, $span Ident);
            assert_eq!(&*token.value, $val);
        };
    }

    macro_rules! lex_number_eq {
        ($res:expr, ($start:literal, $end:literal) $val:literal) => {
            let token = lex_ok!($res, ($start, $end) LitNumber);
            assert_eq!(token.value, $val);
        };
    }

    macro_rules! lex_string_eq {
        ($res:expr, ($start:literal, $end:literal) $val:literal) => {
            let token = lex_ok!($res, ($start, $end) LitString);
            assert_eq!(&*token.value, $val);
        };
    }

    #[test]
    fn cursor() {
        let mut cursor = Cursor::new("xy");
        assert_eq!(cursor.pos(), 0);
        assert_eq!(cursor.next(), 'x');
        assert_eq!(cursor.pos(), 1);
        assert_eq!(cursor.peek(), 'y');
        assert_eq!(cursor.pos(), 1);
        assert_eq!(cursor.next(), 'y');
        assert_eq!(cursor.next(), EOF);
        assert_eq!(cursor.next(), EOF);
        assert_eq!(cursor.pos(), 2);
        assert!(cursor.is_eof());
    }

    #[test]
    fn lex_ident() {
        let mut lexer = Lexer::new("var_1 _var2 var-3");
        lex_ident_eq!(lexer.parse(), (0, 5) "var_1");
        lex_ident_eq!(lexer.parse(), (6, 11) "_var2");
        lex_ident_eq!(lexer.parse(), (12, 17) "var-3");
    }

    #[test]
    fn lex_number() {
        let mut lexer = Lexer::new("0 001 2.3 4E5 6.7E-8 9.1E+2");
        lex_number_eq!(lexer.parse(), (0, 1) 0.0);
        lex_number_eq!(lexer.parse(), (2, 5) 1.0);
        lex_number_eq!(lexer.parse(), (6, 9) 2.3);
        lex_number_eq!(lexer.parse(), (10, 13) 4E5);
        lex_number_eq!(lexer.parse(), (14, 20) 6.7E-8);
        lex_number_eq!(lexer.parse(), (21, 27) 9.1E+2);
        let mut lexer = Lexer::new("0. 0E 0.E 0.E+");
        lex_err!(lexer.parse(), (0, 2) MissingDecimal);
        lex_err!(lexer.parse(), (3, 5) MissingExponent);
        lex_err!(lexer.parse(), (6, 8) MissingDecimal, (6, 9) MissingExponent);
        lex_err!(lexer.parse(), (10, 12) MissingDecimal, (10, 14) MissingExponent);
    }

    #[test]
    fn lex_string() {
        let mut lexer = Lexer::new(r#"'abc' "def" 'gh\'i' "j\"kl""#);
        lex_string_eq!(lexer.parse(), (0, 5) "abc");
        lex_string_eq!(lexer.parse(), (6, 11) "def");
        lex_string_eq!(lexer.parse(), (12, 19) "gh\'i");
        lex_string_eq!(lexer.parse(), (20, 27) "j\"kl");
    }
}
