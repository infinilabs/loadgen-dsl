use crate::error::{Error, ErrorMsg};
use std::{borrow::Cow, fmt};

const EOF: char = '\0';

pub(crate) type LexResult = (LexToken, Option<Error>);

const INVALID_ESCAPE: &str = "invalid escape character";
const UNTERMINATED_STRING: &str = "unterminated string";
const UNTERMINATED_REGEXP: &str = "unterminated regular expression";
const MISSING_EXPONENT: &str = "missing exponent";
const MISSING_DECIMAL: &str = "missing decimal";

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub struct Span {
    pub(crate) start: u32,
    pub(crate) end: u32,
}

impl Span {
    pub(crate) fn dummy() -> Self {
        Self {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    pub(crate) fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub(crate) fn join(&self, other: Span) -> Self {
        debug_assert!(self < &other);
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct LexToken {
    pub span: Span,
    pub kind: LexKind,
}

macro_rules! define_lex_kind {
    ($(#[$attr:meta])*
    $vis:vis enum $name:ident {
        $(#[display = $display:literal] $kind:ident,)*
        $(#[symbol = $symbol:literal] #[display = $Sdisplay:literal] $Skind:ident,)*
    }) => {
        $(#[$attr])*
        $vis enum $name {
            $(#[doc = $display] $kind,)*
            $(#[doc = concat!("`` ", $Sdisplay, " ``")] $Skind,)*
        }

        impl $name {
            #[cfg(test)]
            const SYMBOLS: &[$name] = &[$($name::$Skind,)*];

            fn from_symbol(ch: char) -> Option<Self> {
                match ch {
                    $($symbol => Some($name::$Skind),)*
                    _ => None,
                }
            }

            pub(crate) fn display(&self) -> &'static str {
                match self {
                    $($name::$kind => $display,)*
                    $($name::$Skind => concat!("`", $Sdisplay, "`"),)*
                }
            }
        }
    };
}

define_lex_kind!(
    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub enum LexKind {
        #[display = "whitespace"]
        Whitespace,
        #[display = "comment"]
        Comment,
        #[display = "identifier"]
        Ident,
        #[display = "literal string"]
        String,
        #[display = "literal regular expression"]
        Regexp,
        #[display = "literal integer"]
        Integer,
        #[display = "literal float"]
        Float,
        #[display = "unknown"]
        Unknown,
        #[display = "EOF"]
        Eof,
        #[symbol = '!']
        #[display = "!"]
        Bang,
        #[symbol = '#']
        #[display = "#"]
        Pound,
        #[symbol = '$']
        #[display = "$"]
        Dollar,
        #[symbol = '%']
        #[display = "%"]
        Percent,
        #[symbol = '&']
        #[display = "&"]
        And,
        #[symbol = '(']
        #[display = "("]
        ParenL,
        #[symbol = ')']
        #[display = ")"]
        ParenR,
        #[symbol = '*']
        #[display = "*"]
        Asterisk,
        #[symbol = '+']
        #[display = "+"]
        Plus,
        #[symbol = ',']
        #[display = ","]
        Comma,
        #[symbol = '-']
        #[display = "-"]
        Minus,
        #[symbol = '.']
        #[display = "."]
        Dot,
        // #[symbol = '/']
        // #[display = "/"]
        // Slash,
        #[symbol = ':']
        #[display = ":"]
        Colon,
        #[symbol = ';']
        #[display = ";"]
        Semi,
        #[symbol = '<']
        #[display = "<"]
        Lt,
        #[symbol = '=']
        #[display = "="]
        Eq,
        #[symbol = '>']
        #[display = ">"]
        Gt,
        #[symbol = '?']
        #[display = "?"]
        Question,
        #[symbol = '@']
        #[display = "@"]
        At,
        #[symbol = '[']
        #[display = "["]
        BracketL,
        #[symbol = '\\']
        #[display = "\\"]
        Backslash,
        #[symbol = ']']
        #[display = "]"]
        BracketR,
        #[symbol = '^']
        #[display = "^"]
        Caret,
        #[symbol = '`']
        #[display = "`"]
        Backtick,
        #[symbol = '{']
        #[display = "{"]
        BraceL,
        #[symbol = '|']
        #[display = "|"]
        Or,
        #[symbol = '}']
        #[display = "}"]
        BraceR,
        #[symbol = '~']
        #[display = "~"]
        Tilde,
    }
);

macro_rules! define_pattern {
    (match $ch:ident {
        $($pat:pat => $name:ident,)*
    } ) => {$(
        #[allow(unused)]
        macro_rules! $name {
            () => {
                $pat
            };
        }
        #[allow(unused)]
        fn $name($ch: char) -> bool {
            matches!($ch, $pat)
        }
    )*};
}

define_pattern!(match ch {
    '\t' | '\n' | '\r' | ' ' => whitespace,
    '0'..='9' => digit,
    '_' | 'a'..='z' | 'A'..='Z' => ident_head,
    ident_head!() | '-' | digit!() => ident_body,
});

pub(crate) struct Lexer<'a> {
    cur: std::str::Chars<'a>,
    source: &'a str,
    /// Start position of the current token.
    start: usize,
    /// Cached string.
    buf: String,
    errors: Vec<ErrorMsg>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        if source.len() > (u32::MAX as usize) {
            panic!("input source is too large");
        }
        Self {
            cur: source.chars(),
            source,
            start: 0,
            buf: String::new(),
            errors: Vec::new(),
        }
    }

    pub fn is_eof(&self) -> bool {
        self.cur.as_str().is_empty()
    }

    fn pos(&self) -> usize {
        self.source.len() - self.cur.as_str().len()
    }

    fn peek_ch(&self) -> char {
        self.cur.clone().next().unwrap_or(EOF)
    }

    fn advance(&mut self) -> char {
        self.cur.next().unwrap_or(EOF)
    }

    /// Get the source of the current token, strings and regular expressions are
    /// escaped.
    pub fn source(&self) -> &str {
        if self.buf.is_empty() {
            self.raw_source()
        } else {
            &self.buf
        }
    }

    /// Returns a copy of the current source.
    pub fn copy_source(&self) -> Cow<'a, str> {
        if self.buf.is_empty() {
            Cow::Borrowed(self.raw_source())
        } else {
            Cow::Owned(self.buf.clone())
        }
    }

    fn raw_source(&self) -> &'a str {
        &self.source[self.start..self.pos()]
    }

    /// Parses the next token, ingores whitespace and comments.
    pub fn parse_ignored(&mut self) -> LexResult {
        loop {
            let r = self.parse();
            if r.0.kind > LexKind::Comment {
                break r;
            }
        }
    }

    /// Parses the next token.
    pub fn parse(&mut self) -> LexResult {
        let token = LexToken {
            kind: self.next_kind(),
            span: self.span_from(self.start),
        };
        let err = if self.errors.is_empty() {
            None
        } else {
            Some(Error {
                msgs: std::mem::take(&mut self.errors),
            })
        };
        (token, err)
    }

    fn next_kind(&mut self) -> LexKind {
        // Reset previous state.
        self.buf.clear();
        self.start = self.pos();
        let ch = self.advance();
        match ch {
            whitespace!() => {
                self.skip_while(whitespace);
                LexKind::Whitespace
            }
            '/' if self.skip_ch('/') => {
                self.skip_line();
                LexKind::Comment
            }
            ident_head!() => {
                self.skip_while(ident_body);
                LexKind::Ident
            }
            '\'' | '"' => self.next_string(ch),
            '/' => self.next_regexp(ch),
            digit!() => self.next_number(ch),
            EOF => LexKind::Eof,
            _ => LexKind::from_symbol(ch).unwrap_or(LexKind::Unknown),
        }
    }

    fn span_from(&self, start: usize) -> Span {
        Span::new(start as u32, self.pos() as u32)
    }

    fn error(&mut self, kind: &'static str) {
        self.error_from(self.start, kind);
    }

    fn error_from(&mut self, start: usize, kind: &'static str) {
        self.errors.push(ErrorMsg {
            span: self.span_from(start),
            desc: kind.into(),
        })
    }

    /// Appends the given `char` to the buffer. If the buffer is empty, all consumed
    /// charaters are extended at first.
    fn init_buf(&mut self, ch: char) {
        if self.buf.is_empty() {
            // Ignore the backslash
            let end = self.pos() - 2;
            self.buf.push_str(&self.source[self.start..end]);
        }
        self.buf_push(ch);
    }

    /// Appends the given `char` to the buffer if the buffer has been initialized.
    fn feed_buf(&mut self, ch: char) {
        if !self.buf.is_empty() {
            self.buf_push(ch);
        }
    }

    fn buf_push(&mut self, ch: char) {
        self.buf
            .push(char::from_u32(ch as u32).expect("invalid character"));
    }

    fn skip_if(&mut self, f: impl FnOnce(char) -> bool) -> bool {
        if f(self.peek_ch()) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_ch(&mut self, expected: char) -> bool {
        self.skip_if(|ch| ch == expected)
    }

    fn skip_matches(&mut self, patterns: &[char]) -> bool {
        self.skip_if(|ch| patterns.contains(&ch))
    }

    fn skip_while(&mut self, mut f: impl FnMut(char) -> bool) -> bool {
        if self.skip_if(&mut f) {
            while self.skip_if(&mut f) {}
            true
        } else {
            false
        }
    }

    fn skip_line(&mut self) {
        loop {
            match self.advance() {
                '\n' => break,
                EOF if self.is_eof() => break,
                _ => {}
            }
        }
    }

    fn skip_digits(&mut self) -> bool {
        self.skip_while(digit)
    }

    fn next_string(&mut self, quote: char) -> LexKind {
        debug_assert_matches!(quote, '\'' | '"');
        loop {
            match self.advance() {
                '\\' => {
                    let ch = self.advance();
                    let escaped = match ch {
                        'b' => '\x08',
                        'f' => '\x0c',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\'' | '"' | '\\' | '/' => ch,
                        ch => {
                            self.feed_buf('\\');
                            self.feed_buf(ch);
                            self.error_from(self.pos() - 2, INVALID_ESCAPE);
                            continue;
                        }
                    };
                    self.init_buf(escaped);
                }
                EOF if self.is_eof() => {
                    self.error(UNTERMINATED_STRING);
                    break;
                }
                ch => {
                    self.feed_buf(ch);
                    if ch == quote {
                        break;
                    }
                }
            }
        }
        LexKind::String
    }

    fn next_regexp(&mut self, quote: char) -> LexKind {
        debug_assert_eq!(quote, '/');
        loop {
            match self.advance() {
                '\\' if self.skip_ch('/') => self.init_buf('/'),
                EOF if self.is_eof() => {
                    self.error(UNTERMINATED_REGEXP);
                    break;
                }
                ch => {
                    self.feed_buf(ch);
                    if ch == quote {
                        break;
                    }
                }
            }
        }
        LexKind::Regexp
    }

    fn next_number(&mut self, first: char) -> LexKind {
        debug_assert_matches!(first, digit!());
        self.skip_digits();
        let mut kind = LexKind::Integer;
        if self.skip_ch('.') {
            if !self.skip_digits() {
                self.error(MISSING_DECIMAL);
            }
            kind = LexKind::Float;
        }
        if self.skip_matches(&['e', 'E']) {
            self.skip_matches(&['+', '-']);
            if !self.skip_digits() {
                self.error(MISSING_EXPONENT);
            }
            kind = LexKind::Float;
        }
        kind
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! lex {
        ($lexer:ident $(,)?) => {};
        // Ignore whitespace and comments
        ($lexer:ident, _ $($rest:tt)*) => {
            let (token, err) = $lexer.parse();
            assert_matches!(err, None);
            assert_matches!(token.kind, LexKind::Whitespace | LexKind::Comment);
            lex!($lexer $($rest)*);
        };
        // LexToken without errors
        ($lexer:ident, $kind:ident $span:tt $($rest:tt)*) => {
            lex!($lexer, ($kind $span) $($rest)*);
        };
        // LexToken with errors
        ($lexer:ident,
        ($kind:ident[$start:expr, $end:expr] $(, $Edesc:ident[$Estart:expr, $Eend:expr])* $(,)*)
        $($rest:tt)*) => {
            let (token, err) = $lexer.parse();
            // Validate token
            assert_eq!(token.kind, LexKind::$kind);
            assert_eq!(token.span, Span::new($start, $end));
            // Validate errors
            let mut err = err.into_iter().flat_map(|e| e.msgs);
            $(let msg = err.next().expect("too few errors");
            assert_eq!(&*msg.desc, $Edesc);
            assert_eq!(msg.span, Span::new($Estart, $Eend));)*
            assert!(err.next().is_none(), "too many errros");
            // Process rest tokens
            lex!($lexer $($rest)*);
        };
    }

    #[test]
    fn cursor() {
        let mut lexer = Lexer::new("xy");
        assert_eq!(lexer.pos(), 0);
        assert_eq!(lexer.advance(), 'x');
        assert_eq!(lexer.pos(), 1);
        assert_eq!(lexer.peek_ch(), 'y');
        assert_eq!(lexer.pos(), 1);
        assert_eq!(lexer.advance(), 'y');
        assert_eq!(lexer.advance(), EOF);
        assert_eq!(lexer.advance(), EOF);
        assert_eq!(lexer.pos(), 2);
        assert!(lexer.is_eof());
    }

    #[test]
    fn lex_whitespace() {
        let mut lexer = Lexer::new("abc\tdef\nghi\rjkl mno\t\n\r pqr");
        lex!(lexer,
            Ident[0, 3],
            Whitespace[3, 4],
            Ident[4, 7],
            Whitespace[7, 8],
            Ident[8, 11],
            Whitespace[11, 12],
            Ident[12, 15],
            Whitespace[15, 16],
            Ident[16, 19],
            Whitespace[19, 23],
            Ident[23, 26],
            Eof[26, 26],
        );
    }

    #[test]
    fn lex_comment() {
        let mut lexer = Lexer::new("abc// ignored\ndef// also ignored");
        lex!(lexer,
            Ident[0, 3],
            Comment[3, 14],
            Ident[14, 17],
            Comment[17, 32],
            Eof[32, 32],
        );
    }

    #[test]
    fn lex_ident() {
        let mut lexer = Lexer::new("Var_1 _var2 var-3");

        lex!(lexer, Ident[0, 5]);
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "Var_1");

        lex!(lexer, _, Ident[6, 11]);
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "_var2");

        lex!(lexer, _, Ident[12, 17]);
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "var-3");

        lex!(lexer, Eof[17, 17]);
    }

    #[test]
    fn lex_string() {
        let mut lexer = Lexer::new(r#""a\bc" "de\f" "ghi" "j\kl" "m\no" "pq\r\s\t" "uv"#);

        lex!(lexer, String[0, 6]);
        assert!(!lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"a\x08c\"");

        lex!(lexer, _, String[7, 13]);
        assert!(!lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"de\x0c\"");

        lex!(lexer, _, String[14, 19]);
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"ghi\"");

        // Invalid escape characters should not cause the buffer to initialize.
        lex!(lexer, _, (String[20, 26], INVALID_ESCAPE[22, 24]));
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"j\\kl\"");

        lex!(lexer, _, String[27, 33]);
        assert!(!lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"m\no\"");

        lex!(lexer, _, (String[34, 44], INVALID_ESCAPE[39, 41]));
        assert!(!lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"pq\r\\s\t\"");

        lex!(lexer, _, (String[45, 48], UNTERMINATED_STRING[45, 48]));
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "\"uv");

        lex!(lexer, Eof[48, 48]);
    }

    #[test]
    fn lex_regexp() {
        let mut lexer = Lexer::new(r"/abc\d\efg/ /hij\k\/lm\n/ /opq");

        lex!(lexer, Regexp[0, 11]);
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), r"/abc\d\efg/");

        lex!(lexer, _, Regexp[12, 25]);
        assert!(!lexer.buf.is_empty());
        assert_eq!(lexer.source(), r"/hij\k/lm\n/");

        lex!(lexer, _, (Regexp[26, 30], UNTERMINATED_REGEXP[26, 30]));
        assert!(lexer.buf.is_empty());
        assert_eq!(lexer.source(), "/opq");

        lex!(lexer, Eof[30, 30]);
    }

    #[test]
    fn lex_number() {
        let mut lexer = Lexer::new("0 001 2.3 0. 4E5 6.7E-8 0E 9.1E+2 0.E 0.E+");
        lex!(lexer,
            Integer[0, 1],
            _, Integer[2, 5],
            _, Float[6, 9],
            _, (Float[10, 12], MISSING_DECIMAL[10, 12]),
            _, Float[13, 16],
            _, Float[17, 23],
            _, (Float[24, 26], MISSING_EXPONENT[24, 26]),
            _, Float[27, 33],
            _, (Float[34, 37], MISSING_DECIMAL[34, 36], MISSING_EXPONENT[34, 37]),
            _, (Float[38, 42], MISSING_DECIMAL[38, 40], MISSING_EXPONENT[38, 42]),
            Eof[42, 42],
        );
    }

    #[test]
    fn lex_symbol() {
        let mut lexer = Lexer::new("!#$ %&( )*+ ,-. :;< =>? @[\\ ]^` {|} ~ ");
        let mut i = 0u32;
        for group in LexKind::SYMBOLS.chunks(3) {
            for kind in group {
                let (token, err) = lexer.parse();
                assert_matches!(err, None);
                assert_eq!(&token.kind, kind);
                assert_eq!(token.span, Span::new(i, i + 1));
                i += 1;
            }
            lex!(lexer, Whitespace[i, i + 1]);
            i += 1;
        }
        lex!(lexer, Eof[38, 38]);
    }

    #[test]
    fn lex_unknown() {
        let mut lexer = Lexer::new("你好");
        lex!(lexer,
            Unknown[0, 3],
            Unknown[3, 6],
            Eof[6, 6],
        );
    }

    #[test]
    fn lex_eof() {
        let mut lexer = Lexer::new("abc");
        lex!(lexer, Ident[0, 3], Eof[3, 3]);
    }
}