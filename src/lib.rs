pub mod error;

mod parser;
pub use parser::*;

pub trait Token: Parse {
    fn display() -> &'static str;
    fn peek(cur: Cursor) -> bool;
}

pub trait Peek: Sized {
    fn peek(&self, cur: Cursor) -> bool;

    fn and<T: Peek>(self, and: T) -> And<Self, T> {
        And { a: self, b: and }
    }

    fn or<T: Peek>(self, or: T) -> Or<Self, T> {
        Or { a: self, b: or }
    }
}

impl Peek for fn(Cursor) -> bool {
    fn peek(&self, cur: Cursor) -> bool {
        (self)(cur)
    }
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> error::Result<Self>;
}
