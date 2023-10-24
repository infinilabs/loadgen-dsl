#[macro_use]
mod util;
mod compiler;
mod lexer;
mod parser;

pub mod ast;
pub mod error;

use error::Result;
use serde_yaml::{Mapping, Sequence, Value as Yaml};

pub fn compile(s: &str) -> Result<Mapping> {
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
    while let Some(dsl) = parser.parse()? {
        let mut req = match compile(&dsl.assertion) {
            Ok(t) => t,
            Err(e) => return Err(e.with_source(dsl.assertion)),
        };
        req.insert(
            Yaml::from("request"),
            yaml!({
                ["method"]: dsl.method,
                ["url"]: dsl.url,
                ["body"]: dsl.body,
            })
            .into(),
        );
        reqs.push(req.into());
    }

    Ok(Yaml::from(reqs))
}
