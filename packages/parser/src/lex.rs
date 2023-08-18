use crate::error::{Error, ErrorKind, ErrorKind::*};
use std::{fmt, ops};

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
    symbol,
    '!' | '#'
        | '$'
        | '%'
        | '&'
        | '('
        | ')'
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

pub(crate) type LexResult<T = LexToken> = (T, Option<Error>);

#[derive(Clone, Copy, Debug, Default)]
pub struct LexFlag(u8);

impl ops::BitOr for LexFlag {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl LexFlag {
    pub const ALLOW_REGEXP: Self = Self(0b00000001);

    pub(crate) fn set(&mut self, flag: Self) {
        self.0 |= flag.0;
    }

    pub(crate) fn check(&self, flag: Self) -> bool {
        self.0 & flag.0 != 0
    }
}

pub(crate) struct Lexer<'a> {
    cur: Cursor<'a>,
    /// Starting position of a parse.
    start: usize,
    /// String value in a parse.
    buf: String,
    err: Option<Error>,
    prev: Option<(
        // Previous parsed token.
        LexKind,
        // Possbile errors in a parse.
        Option<Error>,
    )>,
    flag: LexFlag,
}

impl<'a> Lexer<'a> {
    pub const MAX_LEN: usize = usize::MAX >> 1;

    pub fn new(source: &'a str) -> Self {
        if source.len() > Self::MAX_LEN {
            panic!("Lexer supports up to {} bytes", Self::MAX_LEN);
        }
        Self {
            cur: Cursor::new(source),
            start: 0,
            err: None,
            buf: String::new(),
            prev: None,
            flag: LexFlag::default(),
        }
    }

    pub fn parse(&mut self) -> LexResult {
        debug_assert!(self.err.is_none());
        let (kind, err) = loop {
            self.start = self.cur.pos();
            let ch = self.cur.next();
            let kind = match ch {
                whitespace!() => {
                    self.skip_whitespace();
                    continue;
                }
                '/' if self.skip_if(|ch| ch == '/') => {
                    self.skip_line();
                    continue;
                }
                ident_start!() => {
                    self.skip_while(ident_body);
                    self.LexIdent().into()
                }
                digit!() => self.next_number(ch),
                '"' | '\'' => self.next_string(ch),
                '/' if self.flag.check(LexFlag::ALLOW_REGEXP) => self.next_regexp(ch),
                symbol!() => self.LexSymbol(ch, false).into(),
                EOF if self.cur.is_eof() => {
                    break self.prev.take().unwrap_or((self.LexEof().into(), None))
                }
                _ => self.LexUnknown().into(),
            };
            self.flag = LexFlag::default();
            let next = (kind, self.err.take());
            let prev = std::mem::replace(&mut self.prev, Some(next));
            break match prev {
                Some((LexKind::Symbol(sym), _)) => {
                    let joint = sym.span.end == self.start;
                    (LexSymbol { joint, ..sym }.into(), None)
                }
                Some(prev) => prev,
                None => continue,
            };
        };
        (LexToken { kind }, err)
    }

    /// Returns the source in the given [`Span`].
    pub fn fetch(&self, span: Span) -> &str {
        &self.cur.src[span.start..span.end]
    }

    /// Enables certain flags in the next parse.
    pub fn set_flag(&mut self, flag: LexFlag) {
        if let Some(prev) = self.prev.take() {
            self.seek(prev.0.span().start);
        }
        self.flag.set(flag);
    }

    /// Moves the cursor to the specifed position.
    pub fn seek(&mut self, pos: usize) {
        self.prev = None;
        self.cur.iter = self.cur.src[pos..].chars();
    }

    fn span(&self) -> Span {
        self.span_from(self.start)
    }

    fn span_from(&self, start: usize) -> Span {
        Span {
            start,
            end: self.cur.pos(),
        }
    }

    fn take_buf(&mut self) -> Box<str> {
        let buf = Box::from(&*self.buf);
        self.buf.clear();
        buf
    }

    fn error(&mut self, span: Span, e: ErrorKind) {
        let e = Error::new_kind(span, e);
        if let Some(err) = self.err.as_mut() {
            err.combine(e);
        } else {
            self.err = Some(e)
        }
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
            while p(self.cur.peek()) {
                self.cur.advance()
            }
            true
        } else {
            false
        }
    }

    fn skip_line(&mut self) {
        while !self.cur.is_eof() && self.cur.next() != '\n' {}
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(whitespace);
    }

    fn skip_digits(&mut self) {
        self.skip_while(digit);
    }

    fn next_number(&mut self, _leading: char) -> LexKind {
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
        let value = self
            .err
            .as_ref()
            .map(|_| f64::MAX)
            .unwrap_or_else(|| self.fetch(self.span()).parse().unwrap());
        self.LexNumber(value).into()
    }

    fn next_string(&mut self, quote: char) -> LexKind {
        debug_assert!(matches!(quote, '"' | '\''));
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
                        _ => self.error(self.span_from(start - 1), InvalidEscape),
                    }
                }
                ch if ch == quote => break,
                EOF if self.cur.is_eof() => {
                    self.error(self.span(), UnterminatedLiteral);
                    break;
                }
                ch => self.buf.push(ch),
            }
        }
        let buf = self.take_buf();
        self.LexString(buf).into()
    }

    fn next_regexp(&mut self, quote: char) -> LexKind {
        debug_assert!(matches!(quote, '/'));
        loop {
            match self.cur.next() {
                '\\' if self.skip_if(|ch| ch == '/') => self.buf.push('/'),
                ch if ch == quote => break,
                EOF if self.cur.is_eof() => {
                    self.error(self.span(), UnterminatedLiteral);
                    break;
                }
                ch => self.buf.push(ch),
            }
        }
        let buf = self.take_buf();
        self.LexRegexp(buf).into()
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
    pub fn pos(&self) -> usize {
        self.src.len() - self.iter.as_str().len()
    }

    /// Advances and returns the next character. [`EOF`] is returned when reaches EOF.
    pub fn next(&mut self) -> char {
        self.iter.next().unwrap_or(EOF)
    }

    /// Looks at the next character in the stream.
    pub fn peek(&self) -> char {
        self.iter.clone().next().unwrap_or(EOF)
    }

    /// Moves the cursor to the next character.
    pub fn advance(&mut self) {
        self.iter.next();
    }
}

/// A region of source code.
#[derive(Clone, Copy)]
pub struct Span {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
            .field(&self.start)
            .field(&self.end)
            .finish()
    }
}

impl Span {
    pub fn combine(&mut self, another: Span) {
        self.end = another.end;
    }

    pub fn join(&self, another: Span) -> Self {
        Span {
            start: self.start,
            end: another.end,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LexToken {
    pub(crate) kind: LexKind,
}

impl LexToken {
    pub fn span(&self) -> Span {
        self.kind.span()
    }

    fn as_kind<T>(&self) -> Option<&T>
    where
        for<'a> &'a T: TryFrom<&'a LexKind>,
    {
        TryFrom::try_from(&self.kind).ok()
    }

    fn into_kind<T>(self) -> Result<T, Self>
    where
        T: TryFrom<LexKind, Error = LexKind>,
    {
        let Self { kind } = self;
        TryFrom::try_from(kind).map_err(|kind| Self { kind })
    }

    pub fn as_ident(&self) -> Option<&LexIdent> {
        self.as_kind()
    }

    pub fn into_ident(self) -> Result<LexIdent, Self> {
        self.into_kind()
    }

    pub fn as_number(&self) -> Option<&LexNumber> {
        self.as_kind()
    }

    pub fn into_number(self) -> Result<LexNumber, Self> {
        self.into_kind()
    }

    pub fn as_string(&self) -> Option<&LexString> {
        self.as_kind()
    }

    pub fn into_string(self) -> Result<LexString, Self> {
        self.into_kind()
    }

    pub fn as_symbol(&self) -> Option<&LexSymbol> {
        self.as_kind()
    }

    pub fn into_symbol(self) -> Result<LexSymbol, Self> {
        self.into_kind()
    }
}

macro_rules! define_token {
    ($(#[$attr:meta])* $vis:vis enum $name:ident {$(
        $variant:ident($(#[$vattr:meta])* $vvis:vis struct $vname:ident {
            $($vfield:ident: $vtype:ty,)*
        }),
    )*}) => {
        $(#[$attr])* $vis enum $name {
            $($variant($vname),)*
        }
        impl $name {
            pub fn span(&self) -> Span {
                match self {
                    $($name::$variant(v) => v.span(),)*
                }
            }
        }
        $($(#[$vattr])* $vvis struct $vname {
            pub(crate) span: Span,
            $(pub(crate) $vfield: $vtype,)*
        }
        impl Lexer<'_> {
            #[allow(non_snake_case)]
            pub(crate) fn $vname(&self, $($vfield: $vtype,)*) -> $vname {
                $vname {
                    span: self.span(),
                    $($vfield,)*
                }
            }
        }
        impl $vname {
            pub fn span(&self) -> Span {
                self.span
            }
        }
        impl From<$vname> for $name {
            fn from(v: $vname) -> Self {
                Self::$variant(v)
            }
        }
        impl TryFrom<$name> for $vname {
            type Error = $name;
            fn try_from(v: $name) -> Result<Self, $name> {
                match v {
                    $name::$variant(v) => Ok(v),
                    _ => Err(v),
                }
            }
        }
        impl<'a> TryFrom<&'a $name> for &'a $vname {
            type Error = &'a $name;
            fn try_from(v: &'a $name) -> Result<Self, &'a $name> {
                match v {
                    $name::$variant(v) => Ok(v),
                    _ => Err(v),
                }
            }
        })*
    };
}

define_token! {
    #[derive(Clone, Debug)]
    pub(crate) enum LexKind {
        Ident(
            #[derive(Clone, Debug)]
            pub struct LexIdent {
            }
        ),
        Number(
            #[derive(Clone, Debug)]
            pub struct LexNumber {
                value: f64,
            }
        ),
        String(
            #[derive(Clone, Debug)]
            pub struct LexString {
                value: Box<str>,
            }
        ),
        Regexp(
            #[derive(Clone, Debug)]
            pub struct LexRegexp {
                value: Box<str>,
            }
        ),
        Symbol(
            #[derive(Clone, Debug)]
            pub struct LexSymbol {
                value: char,
                joint: bool,
            }
        ),
        Unknown(
            #[derive(Clone, Debug)]
            pub(crate) struct LexUnknown {
            }
        ),
        Eof(
            #[derive(Clone, Debug)]
            pub(crate) struct LexEof {
            }
        ),
    }
}

impl LexNumber {
    pub fn value(&self) -> f64 {
        self.value
    }
}

impl LexString {
    pub fn value(&self) -> &str {
        &self.value
    }
}

impl LexRegexp {
    pub fn value(&self) -> &str {
        &self.value
    }
}

impl LexSymbol {
    pub fn value(&self) -> char {
        self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! lex_err {
        ($lexer:expr, $(($start:expr, $end:expr) $err:pat),+ $(,)?) => {{
            let (_, err) = $lexer.parse();
            let mut err = err.unwrap().into_iter();
            $(let (span, e) = err.next().unwrap();
            assert_matches!(e, $err);
            assert_eq!((span.start, span.end), ($start, $end));)*
            assert_matches!(err.next(), None);
        }};
    }

    macro_rules! lex_ok {
        ($lexer:expr, ($start:expr, $end:expr) $kind:tt) => {{
            let (token, err) = $lexer.parse();
            assert_matches!(err, None);
            match token.kind {
                LexKind::$kind(token) => {
                    let span = token.span;
                    assert_eq!((span.start, span.end), ($start, $end));
                    token
                }
                _ => panic!("expects {}", stringify!($kind)),
            }
        }};
    }

    macro_rules! lex_ident {
        ($lexer:expr, $span:tt $val:literal) => {
            let token = lex_ok!($lexer, $span Ident);
            assert_eq!($lexer.fetch(token.span), $val);
        };
    }

    macro_rules! lex_number {
        ($lexer:expr, ($start:expr, $end:expr) $val:literal) => {
            let token = lex_ok!($lexer, ($start, $end) Number);
            assert_eq!(token.value, $val);
        };
    }

    macro_rules! lex_string {
        ($lexer:expr, ($start:expr, $end:expr) $val:literal) => {
            let token = lex_ok!($lexer, ($start, $end) String);
            assert_eq!(&*token.value, $val);
        };
    }

    macro_rules! lex_regexp {
        ($lexer:expr, ($start:expr, $end:expr) $val:literal) => {
            $lexer.set_flag(LexFlag::ALLOW_REGEXP);
            let token = lex_ok!($lexer, ($start, $end) Regexp);
            assert_eq!(&*token.value, $val);
        };
    }

    macro_rules! lex_symbols {
        ($lexer:expr, ($start:expr, $end:expr) $ch:literal) => {
            let token = lex_ok!($lexer, ($start, $end) Symbol);
            assert_eq!(token.value, $ch);
            assert_eq!(token.joint, false);
        };
        ($lexer:expr, ($start:expr, $end:expr) $ch1:literal $($ch:literal)+) => {
            let token = lex_ok!($lexer, ($start, $start + 1) Symbol);
            assert_eq!(token.value, $ch1);
            assert_eq!(token.joint, true);
            lex_symbols!($lexer, ($start + 1, $end) $($ch)*);
        };
    }

    macro_rules! lex_unknown {
        ($lexer:expr, ($start:expr, $end:expr) $ch:literal) => {
            let token = lex_ok!($lexer, ($start, $end) Unknown);
            let chars = $lexer
                .fetch(token.span)
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
        let mut lexer = Lexer::new("Var_1 _var2 var-3");
        lex_ident!(lexer, (0, 5) "Var_1");
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
        lex_err!(lexer, (4, 6) InvalidEscape, (0, 7) UnterminatedLiteral);
        let mut lexer = Lexer::new(r#""abcd\e"#);
        lex_err!(lexer, (5, 7) InvalidEscape, (0, 7) UnterminatedLiteral);
    }

    #[test]
    fn lex_regexp() {
        let mut lexer = Lexer::new(r"/abc\w\+/ /def\d\//");
        lex_regexp!(lexer, (0, 9) r"abc\w\+");
        lex_regexp!(lexer, (10, 19) r"def\d/");
        let mut lexer = Lexer::new(r"/abcdef\/");
        lexer.set_flag(LexFlag::ALLOW_REGEXP);
        lex_err!(lexer, (0, 9) UnterminatedLiteral);
    }

    #[test]
    fn lex_symbols() {
        let mut lexer = Lexer::new("!#$ %&* +,- ./: ;<= >?@ []^ {}| ~");
        lex_symbols!(lexer, (0, 3) '!' '#' '$');
        lex_symbols!(lexer, (4, 7) '%' '&' '*');
        lex_symbols!(lexer, (8, 11) '+' ',' '-');
        lex_symbols!(lexer, (12, 15) '.' '/' ':');
        lex_symbols!(lexer, (16, 19) ';' '<' '=');
        lex_symbols!(lexer, (20, 23) '>' '?' '@');
        lex_symbols!(lexer, (24, 27) '[' ']' '^');
        lex_symbols!(lexer, (28, 31) '{' '}' '|');
        lex_symbols!(lexer, (32, 33) '~');
        let mut lexer = Lexer::new("+// ignored\n+");
        lex_symbols!(lexer, (0, 1) '+');
        lex_symbols!(lexer, (12, 13) '+');
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
