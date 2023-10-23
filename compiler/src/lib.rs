#[macro_use]
mod util;
mod compiler;
mod lexer;
mod parser;

pub mod ast;
pub mod error;

pub fn compile(s: &str) -> error::Result<serde_yaml::Value> {
    let ast = parser::Parser::new(s).parse::<ast::Dsl>()?;
    compiler::Compiler::new().compile(&ast)
}
