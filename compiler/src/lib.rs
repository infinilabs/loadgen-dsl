// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
                let req_params = yaml!({
                        ["method"]: req.method,
                        ["url"]: req.url,
                        ["body"]: req.body,
                });
                let req = if !assertion.is_empty() {
                    let mut req = compile(&assertion).map_err(|e| e.with_source(&assertion))?;
                    assertion.clear();
                    match req
                        .entry("request".into())
                        .or_insert_with(|| Mapping::new().into())
                    {
                        Yaml::Mapping(req) => req.extend(req_params),
                        others => *others = req_params.into(),
                    }
                    req
                } else {
                    yaml!({ ["request"]: req_params })
                };
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
