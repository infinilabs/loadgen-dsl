#[macro_use]
mod util;
pub mod ast;
pub mod compile;
pub mod error;
pub mod parse;
pub mod token;

pub fn compile(input: &str) -> error::Result<String> {
    let config = compile::Compiler::new().compile(input)?;
    Ok(serde_yaml::to_string(&config).expect("should be a invalid configuration"))
}
