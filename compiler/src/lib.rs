#[macro_use]
mod util;
mod compiler;
mod lexer;
mod parser;

pub mod ast;
pub mod error;

use error::Result;
use lexer::{Req, ReqKind};
use serde_yaml::{Mapping, Sequence, Value as Yaml};

pub fn compile(s: &str) -> Result<Mapping> {
    let ast = parser::Parser::new(s).parse_finished::<ast::Dsl>()?;
    compiler::Compiler::new().compile(&ast)
}

fn compile_full(s: &str) -> Result<Mapping> {
    let ast = parser::Parser::new(s).parse_finished::<ast::DslFull>()?;
    compiler::Compiler::new().compile(&ast::Dsl::Full(ast))
}

pub fn compile_requests(s: &str) -> Result<Yaml> {
    _compile_requests(s).map_err(|e| e.with_source(s))
}

fn _compile_requests(s: &str) -> Result<Yaml> {
    let mut opts = Mapping::new();
    let mut reqs = Sequence::new();
    let mut parser = lexer::ReqParser::new(s);

    let mut assertion = String::new();
    let mut curr_req = None::<Req>;
    loop {
        let kind = parser.parse()?;
        if matches!(kind, ReqKind::Req(_) | ReqKind::Eof) {
            if let Some(req) = curr_req.take() {
                // A new request
                let mut req = yaml!({
                    ["request"]: {
                        ["method"]: req.method,
                        ["url"]: req.url,
                        ["body"]: req.body,
                    },
                });
                if !assertion.is_empty() {
                    match compile(&assertion) {
                        Ok(t) => req.extend(t),
                        Err(e) => return Err(e.with_source(assertion)),
                    }
                    assertion.clear();
                }
                reqs.push(req.into());
            } else {
                // Global options
                if !assertion.is_empty() {
                    match compile_full(&assertion) {
                        Ok(t) => {
                            debug_assert!(opts.is_empty());
                            opts.extend(t);
                        }
                        Err(e) => return Err(e.with_source(assertion)),
                    }
                    assertion.clear();
                }
            }
        }
        match kind {
            ReqKind::Req(req) => curr_req = Some(req),
            ReqKind::Comment(t) => assertion.push_str(t),
            ReqKind::Eof => break,
        }
    }

    opts.insert(yaml!("requests"), reqs.into());
    Ok(opts.into())
}
