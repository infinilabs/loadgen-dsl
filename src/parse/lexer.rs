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
define_pattern!(whitespace, '\t' | '\n' | '\r' | ' ');
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

pub(super) type ByteIndex = u32;
pub(super) type LexFlag = u8;
pub(super) type LexResult<T = TokenKind> = (T, Option<Error>);

/// A region of source code.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub(super) start: ByteIndex,
    pub(super) end: ByteIndex,
}

pub(super) struct Lexer<'a> {
    cur: Cursor<'a>,
    /// Starting position in a parse.
    start: ByteIndex,
    /// Characters buffered in a parse.
    buf: String,
    /// Possbile errors in a parse.
    err: Option<Error>,
    flag: LexFlag,
}

impl<'a> Lexer<'a> {
    #[allow(clippy::unnecessary_cast)]
    pub const MAX_LEN: usize = (ByteIndex::MAX as usize) >> 1;
    pub const ALLOW_REGEXP: LexFlag = 0b00000001;

    pub fn new(source: &'a str) -> Self {
        if source.len() > Self::MAX_LEN {
            panic!("Lexer supports up to {} bytes", Self::MAX_LEN);
        }
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
                '/' if self.check_flag(Self::ALLOW_REGEXP) => self.next_regexp(ch),
                punct!() => Punct {
                    span: self.span(),
                    value: ch,
                    joint: punct(self.cur.peek()),
                }
                .into(),
                EOF if self.cur.is_eof() => Eof { span: self.span() }.into(),
                _ => Unknown { span: self.span() }.into(),
            };
            self.flag = 0;
            break (token, self.take_error());
        }
    }

    /// Changes the runtime behavior of the next parse.
    pub fn set_flag(&mut self, flag: LexFlag) {
        self.flag |= flag;
    }

    /// Moves the cursor to the specified position.
    pub fn seek(&mut self, pos: ByteIndex) {
        self.cur.seek(pos);
    }

    pub fn is_empty(&self) -> bool {
        self.cur.is_eof()
    }

    fn check_flag(&self, flag: LexFlag) -> bool {
        self.flag & flag != 0
    }

    fn span(&self) -> Span {
        self.span_from(self.start)
    }

    fn span_from(&self, start: ByteIndex) -> Span {
        Span {
            start,
            end: self.cur.pos(),
        }
    }

    fn source(&self) -> &str {
        self.cur.fetch(self.start, self.cur.pos())
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

    fn next_ident(&mut self, _start: char) -> TokenKind {
        self.skip_while(ident_body);
        Ident {
            span: self.span(),
            value: self.source().into(),
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
                    // TODO: dedent, escape `\n`
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
    pub fn pos(&self) -> ByteIndex {
        (self.src.len() - self.iter.as_str().len()) as ByteIndex
    }

    /// Returns the source in the given [`Span`].
    #[allow(clippy::unnecessary_cast)]
    pub fn fetch(&self, start: ByteIndex, end: ByteIndex) -> &str {
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
    #[allow(clippy::unnecessary_cast)]
    pub fn seek(&mut self, pos: ByteIndex) {
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

    macro_rules! assert_matches {
        ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
            match $left {
                $( $pattern )|+ $( if $guard )? => {}
                ref left => {
                    panic!(
                        "assertion failed: `(left matches right)`\n left: `{}`\nright: `{:?}`",
                        stringify!($($pattern)|+ $(if $guard)?),
                        left,
                    )
                }
            }
        };
    }

    macro_rules! lex_err {
        ($lexer:expr, $(($start:expr, $end:expr) $err:pat),+ $(,)?) => {{
            let (_, result) = $lexer.parse();
            let mut err = result.unwrap().into_iter();
            $({
                let (span, e) = err.next().unwrap();
                assert_eq!((span.start, span.end), ($start, $end));
                assert_matches!(e, $err);
            })*
            assert!(err.next().is_none());
        }};
    }

    macro_rules! lex_ok {
        ($lexer:expr, ($start:expr, $end:expr) $ty:ident) => {{
            let (token, result) = $lexer.parse();
            let span = token.span();
            assert_eq!((span.start, span.end), ($start, $end));
            assert!(result.is_none());
            <$ty>::try_from(token).unwrap()
        }};
    }

    macro_rules! lex_ident {
        ($lexer:expr, $span:tt $val:literal) => {
            let token = lex_ok!($lexer, $span Ident);
            assert_eq!(&*token.value, $val);
        };
    }

    macro_rules! lex_number {
        ($lexer:expr, ($start:expr, $end:expr) $val:literal) => {
            let token = lex_ok!($lexer, ($start, $end) LitNumber);
            assert_eq!(token.value, $val);
        };
    }

    macro_rules! lex_string {
        ($lexer:expr, ($start:expr, $end:expr) $val:literal) => {
            let token = lex_ok!($lexer, ($start, $end) LitString);
            assert_eq!(&*token.value, $val);
        };
    }

    macro_rules! lex_regexp {
        ($lexer:expr, ($start:expr, $end:expr) $val:literal) => {
            $lexer.set_flag(Lexer::ALLOW_REGEXP);
            let token = lex_ok!($lexer, ($start, $end) LitRegexp);
            assert_eq!(&*token.value, $val);
        };
    }

    macro_rules! lex_punct {
        ($lexer:expr, ($start:expr, $end:expr) $ch:literal) => {
            let token = lex_ok!($lexer, ($start, $end) Punct);
            assert_eq!(token.value, $ch);
            assert_eq!(token.joint, false);
        };
        ($lexer:expr, ($start:expr, $end:expr) $ch1:literal $($ch:literal)+) => {
            let token = lex_ok!($lexer, ($start, $start + 1) Punct);
            assert_eq!(token.value, $ch1);
            assert_eq!(token.joint, true);
            lex_punct!($lexer, ($start + 1, $end) $($ch)*);
        };
    }

    macro_rules! lex_unknown {
        ($lexer:expr, ($start:expr, $end:expr) $ch:literal) => {
            let token = lex_ok!($lexer, ($start, $end) Unknown);
            let chars = $lexer
                .cur
                .fetch(token.span.start, token.span.end)
                .chars()
                .collect::<Vec<_>>();
            assert_eq!(&*chars, &[$ch]);
        };
    }

    macro_rules! lex_eof {
        ($lexer:expr, ($start:expr, $end:expr)) => {
            lex_ok!($lexer, ($start, $end) Eof);
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
    fn lex_whitespace() {
        let mut lexer = Lexer::new("abc\tdef\nghi\rjkl mno\t\n\r pqr");
        lex_ident!(lexer, (0, 3) "abc");
        lex_ident!(lexer, (4, 7) "def");
        lex_ident!(lexer, (8, 11) "ghi");
        lex_ident!(lexer, (12, 15) "jkl");
        lex_ident!(lexer, (16, 19) "mno");
        lex_ident!(lexer, (23, 26) "pqr");
    }

    #[test]
    fn lex_comment() {
        let s = ["abc", "// ignored", "def", "// also ignored"].join("\n");
        let mut lexer = Lexer::new(&s);
        lex_ident!(lexer, (0, 3) "abc");
        lex_ident!(lexer, (15, 18) "def");
        lex_eof!(lexer, (34, 34));
    }

    #[test]
    fn lex_ident() {
        let mut lexer = Lexer::new("var_1 _var2 var-3");
        lex_ident!(lexer, (0, 5) "var_1");
        lex_ident!(lexer, (6, 11) "_var2");
        lex_ident!(lexer, (12, 17) "var-3");
    }

    #[test]
    fn lex_number() {
        let mut lexer = Lexer::new("0 001 2.3 4E5 6.7E-8 9.1E+2");
        lex_number!(lexer, (0, 1) 0.0);
        lex_number!(lexer, (2, 5) 1.0);
        lex_number!(lexer, (6, 9) 2.3);
        lex_number!(lexer, (10, 13) 4E5);
        lex_number!(lexer, (14, 20) 6.7E-8);
        lex_number!(lexer, (21, 27) 9.1E+2);
        let mut lexer = Lexer::new("0. 0E 0.E 0.E+");
        lex_err!(lexer, (0, 2) MissingDecimal);
        lex_err!(lexer, (3, 5) MissingExponent);
        lex_err!(lexer, (6, 8) MissingDecimal, (6, 9) MissingExponent);
        lex_err!(lexer, (10, 12) MissingDecimal, (10, 14) MissingExponent);
    }

    #[test]
    fn lex_string() {
        let mut lexer = Lexer::new(r#"'abc' "def" 'gh\'i' "j\"kl" "\b\f\n\r\t""#);
        lex_string!(lexer, (0, 5) "abc");
        lex_string!(lexer, (6, 11) "def");
        lex_string!(lexer, (12, 19) "gh\'i");
        lex_string!(lexer, (20, 27) "j\"kl");
        lex_string!(lexer, (28, 40) "\x08\x0c\n\r\t");
        let mut lexer = Lexer::new(r#"'abc\de"#);
        lex_err!(lexer, (4, 6) InvalidEscape('d'), (0, 7) UnterminatedString);
        let mut lexer = Lexer::new(r#""abcd\e"#);
        lex_err!(lexer, (5, 7) InvalidEscape('e'), (0, 7) UnterminatedString);
    }

    #[test]
    fn lex_regexp() {
        let mut lexer = Lexer::new(r"/abc\w\+/ /def\d\//");
        lex_regexp!(lexer, (0, 9) r"abc\w\+");
        lex_regexp!(lexer, (10, 19) r"def\d/");
        let mut lexer = Lexer::new(r"/abcdef\/");
        lexer.set_flag(Lexer::ALLOW_REGEXP);
        lex_err!(lexer, (0, 9) UnterminatedRegexp);
    }

    #[test]
    fn lex_punct() {
        let mut lexer = Lexer::new(r"!#$ %&* +,- ./: ;<= >?@ []^ {}| ~");
        lex_punct!(lexer, (0, 3) '!' '#' '$');
        lex_punct!(lexer, (4, 7) '%' '&' '*');
        lex_punct!(lexer, (8, 11) '+' ',' '-');
        lex_punct!(lexer, (12, 15) '.' '/' ':');
        lex_punct!(lexer, (16, 19) ';' '<' '=');
        lex_punct!(lexer, (20, 23) '>' '?' '@');
        lex_punct!(lexer, (24, 27) '[' ']' '^');
        lex_punct!(lexer, (28, 31) '{' '}' '|');
        lex_punct!(lexer, (32, 33) '~');
    }

    #[test]
    fn lex_unknown() {
        let mut lexer = Lexer::new("你好");
        lex_unknown!(lexer, (0, 3) '你');
        lex_unknown!(lexer, (3, 6) '好');
    }

    #[test]
    fn lex_eof() {
        let mut lexer = Lexer::new("abc");
        lex_ident!(lexer, (0, 3) "abc");
        lex_eof!(lexer, (3, 3));
    }
}
