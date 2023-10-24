#[macro_use]
mod util;
mod compiler;
mod lexer;
mod parser;

pub mod ast;
pub mod error;

use error::Result;
use serde_yaml::{Sequence, Value as Yaml};

pub fn compile(s: &str) -> Result<Yaml> {
    let ast = parser::Parser::new(s).parse::<ast::Dsl>()?;
    compiler::Compiler::new().compile(&ast)
}

pub fn compile_requests(s: &str) -> Result<Yaml> {
    match _compile_requests(s) {
        Ok(t) => Ok(t),
        Err(e) if e.source_code.is_none() => Err(e.with_source(s.to_owned())),
        Err(e) => Err(e),
    }
}

fn _compile_requests(s: &str) -> Result<Yaml> {
    let mut reqs = Sequence::new();
    let mut parser = lexer::ReqParser::new(s);
    while let Some(req) = parser.parse()? {
        let assertion = match compile(&req.assertion) {
            Ok(t) => t,
            Err(e) => return Err(e.with_source(req.assertion)),
        };
        reqs.push(
            yaml!({
                ["request"]: {
                    ["method"]: req.method,
                    ["url"]: req.url,
                    ["body"]: req.body,
                },
                ["assert"]: assertion,
            })
            .into(),
        );
    }

    Ok(Yaml::from(reqs))
}
