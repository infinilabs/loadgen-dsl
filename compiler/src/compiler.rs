// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
use crate::{
    ast::*,
    error::{Error, Result},
    util::UnpackFrom,
};
use serde_yaml::{Mapping, Sequence, Value as Yaml};
use std::borrow::Cow;

pub(crate) trait Compilable: Spanned {
    fn display() -> &'static str;

    fn compile_value(&self, ctx: &mut Context) -> Result<Yaml> {
        let _ = ctx;
        Err(Error::new(
            self.span(),
            format!("cannot use {} as value", Self::display()),
        ))
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        let _ = (ctx, field);
        Err(Error::new(
            self.span(),
            format!("cannot use {} as assertion", Self::display()),
        ))
    }
}

pub(crate) struct Context {}

impl Context {
    fn compile_brief(
        &mut self,
        status: Option<&LitInteger>,
        body: Option<&ExprObject>,
    ) -> Result<Mapping> {
        let status = status.map(|status| {
            yaml!({
                ["equals"]: {
                    ["_ctx.response.status"]: status.value()
                }
            })
        });
        let body = body
            .map(|body| body.compile_assertion(self, "_ctx.response.body_json"))
            .transpose()?;
        Ok(match (status, body) {
            (Some(status), Some(body)) => yaml!({ ["and"]: [status, body] }),
            (Some(r), _) | (_, Some(r)) => r,
            (None, None) => Mapping::new(),
        })
    }

    fn compile_full<'a>(
        &mut self,
        fields: impl 'a + IntoIterator<Item = &'a Field>,
    ) -> Result<Mapping> {
        fields
            .into_iter()
            .map(|f| {
                let key = f.path.to_field();
                let value = if key == "assert" {
                    let mut expr = &*f.value;
                    loop {
                        break match expr {
                            Expr::Object(expr) => {
                                self.compile_fields_in(expr.fields.items(), None)?
                            }
                            Expr::Tuple(expr) => expr.compile_as_brief_dsl(self)?,
                            Expr::Paren(e) => {
                                expr = &*e.elem;
                                continue;
                            }
                            _ => {
                                return Err(Error::new(
                                    f.value.span(),
                                    format!(
                                        "only {} and {} are supported as assertion",
                                        ExprObject::display(),
                                        ExprTuple::display(),
                                    ),
                                ));
                            }
                        };
                    }
                    .into()
                } else {
                    f.value.compile_value(self)?
                };
                Ok((Yaml::from(key), value))
            })
            .collect()
    }

    fn compile_fields_in<'a>(
        &mut self,
        fields: impl 'a + IntoIterator<Item = &'a Field>,
        root: Option<&str>,
    ) -> Result<Mapping> {
        let assertions = fields
            .into_iter()
            .map(|f| {
                let key = f.path.to_field();
                let key = if let Some(root) = root {
                    format!("{root}.{key}")
                } else {
                    key
                };
                f.value.compile_assertion(self, &key).map(Yaml::from)
            })
            .collect::<Result<Sequence>>()?;
        Ok(yaml!({ ["and"]: assertions }))
    }
}

pub(crate) struct Compiler {
    context: Context,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            context: Context {},
        }
    }

    pub fn compile(&mut self, ast: &Dsl) -> Result<Mapping> {
        match ast {
            Dsl::Brief(ast) => Ok(yaml!({
                ["assert"]: self.context.compile_brief(ast.status.as_ref(), ast.body.as_ref())?,
            })),
            Dsl::Full(ast) => self.context.compile_full(ast.fields.items()),
        }
    }
}

impl Compilable for Expr {
    fn display() -> &'static str {
        "expression"
    }

    fn compile_value(&self, ctx: &mut Context) -> Result<Yaml> {
        match self {
            Self::Array(t) => t.compile_value(ctx),
            Self::Binary(t) => t.compile_value(ctx),
            Self::Funcall(t) => t.compile_value(ctx),
            Self::Lit(t) => t.compile_value(ctx),
            Self::Object(t) => t.compile_value(ctx),
            Self::Paren(t) => t.compile_value(ctx),
            Self::Tuple(t) => t.compile_value(ctx),
            Self::Unary(t) => t.compile_value(ctx),
        }
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        match self {
            Self::Array(t) => t.compile_assertion(ctx, field),
            Self::Binary(t) => t.compile_assertion(ctx, field),
            Self::Funcall(t) => t.compile_assertion(ctx, field),
            Self::Lit(t) => t.compile_assertion(ctx, field),
            Self::Object(t) => t.compile_assertion(ctx, field),
            Self::Paren(t) => t.compile_assertion(ctx, field),
            Self::Tuple(t) => t.compile_assertion(ctx, field),
            Self::Unary(t) => t.compile_assertion(ctx, field),
        }
    }
}

impl Compilable for ExprLit {
    fn display() -> &'static str {
        "literal-expression"
    }

    fn compile_value(&self, _ctx: &mut Context) -> Result<Yaml> {
        Ok(match self {
            Self::Bool(t) => Yaml::from(t.value()),
            Self::Float(t) => Yaml::from(t.value()),
            Self::Integer(t) => Yaml::from(t.value()),
            Self::Null(_) => Yaml::Null,
            Self::Regexp(t) => Yaml::from(t.value()),
            Self::String(t) => Yaml::from(t.value()),
        })
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        Ok(match self {
            Self::Regexp(_) => yaml!({
                ["regexp"]: {
                    [field]: self.compile_value(ctx)?,
                },
            }),
            _ => yaml!({
                ["equals"]: {
                    [field]: self.compile_value(ctx)?,
                },
            }),
        })
    }
}

impl Compilable for ExprArray {
    fn display() -> &'static str {
        "array-expression"
    }

    fn compile_value(&self, ctx: &mut Context) -> Result<Yaml> {
        self.elems
            .items()
            .map(|elem| elem.compile_value(ctx).map(Yaml::from))
            .collect::<Result<Sequence>>()
            .map(Yaml::from)
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        let assertions = self
            .elems
            .items()
            .enumerate()
            .map(|(i, elem)| {
                elem.compile_assertion(ctx, &format!("{field}.{i}"))
                    .map(Yaml::from)
            })
            .collect::<Result<Sequence>>()?;
        Ok(yaml!({ ["and"]: assertions }))
    }
}

impl Compilable for ExprObject {
    fn display() -> &'static str {
        "object-expression"
    }

    fn compile_value(&self, ctx: &mut Context) -> Result<Yaml> {
        self.fields
            .items()
            .map(|f| {
                let key = f.path.to_field();
                let value = f.value.compile_value(ctx)?;
                Ok((Yaml::from(key), value))
            })
            .collect::<Result<Mapping>>()
            .map(Yaml::from)
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        ctx.compile_fields_in(self.fields.items(), Some(field))
    }
}

impl Key {
    fn to_field(&self) -> Cow<str> {
        match self {
            Self::Array(t) => Cow::Owned(t.value().to_string()),
            Self::Ident(t) => Cow::Borrowed(t.value()),
            Self::String(t) => Cow::Borrowed(t.value()),
        }
    }
}

impl Path {
    fn to_field(&self) -> String {
        let mut iter = self.segments.items();
        let Some(first) = iter.next() else {
            return String::new();
        };
        let mut name = first.to_field().into_owned();
        for key in iter {
            name.push('.');
            name.push_str(&key.to_field());
        }
        name
    }
}

impl ExprUnary {
    fn neg(&self, ctx: &mut Context) -> Result<Yaml> {
        match self.elem.compile_value(ctx)? {
            Yaml::Number(n) => Ok(n
                .as_i64()
                .map(|i| Yaml::from(-i))
                .or_else(|| n.as_f64().map(|i| Yaml::from(-i)))
                .unwrap()),
            _ => Err(Error::new(
                self.span(),
                "can only apply negation to numbers",
            )),
        }
    }
}

impl Compilable for ExprUnary {
    fn display() -> &'static str {
        "unary-expression"
    }

    fn compile_value(&self, ctx: &mut Context) -> Result<Yaml> {
        match self.op {
            UnaryOp::Neg(_) => self.neg(ctx),
            _ => Err(Error::new(self.span(), "cannot use assertion as value")),
        }
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        Ok(match self.op {
            UnaryOp::Eq(_) => yaml!({ ["equals"]: self.elem.compile_value(ctx)? }),
            UnaryOp::Neg(_) => yaml!({ ["equals"]: self.neg(ctx)? }),
            UnaryOp::Not(_) => yaml!({ ["not"]: self.elem.compile_assertion(ctx, field)? }),
            _ => {
                let op = match self.op {
                    UnaryOp::Ge(_) => "gte",
                    UnaryOp::Gt(_) => "gt",
                    UnaryOp::Le(_) => "lte",
                    UnaryOp::Lt(_) => "lt",
                    _ => unreachable!(),
                };
                yaml!({ ["range"]: {[field]: {[op]: self.elem.compile_value(ctx)?}} })
            }
        })
    }
}

impl Compilable for ExprBinary {
    fn display() -> &'static str {
        "binary-expression"
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        let op = match self.op {
            BinaryOp::And(_) => "and",
            BinaryOp::Or(_) => "or",
        };
        Ok(yaml!({
            [op]: [
                self.left.compile_assertion(ctx, field)?,
                self.right.compile_assertion(ctx, field)?,
            ],
        }))
    }
}

impl ExprFuncall {
    fn unpack_params<'a, T>(&'a self) -> Result<T>
    where
        T: UnpackFrom<'a>,
    {
        T::unpack(self.params.span(), self.params.items())
    }
}

impl Compilable for ExprFuncall {
    fn display() -> &'static str {
        "function-call-expression"
    }

    fn compile_value(&self, _ctx: &mut Context) -> Result<Yaml> {
        Err(Error::new(
            self.span(),
            format!("{} is not yet supported as value", Self::display()),
        ))
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        let arg = self.unpack_params::<&Expr>()?;
        let f = self.ident.value();
        Ok(yaml!({
            [f]: {
                [field]: arg.compile_value(ctx)?,
            },
        }))
    }
}

impl Compilable for ExprParen {
    fn display() -> &'static str {
        Expr::display()
    }

    fn compile_value(&self, ctx: &mut Context) -> Result<Yaml> {
        self.elem.compile_value(ctx)
    }

    fn compile_assertion(&self, ctx: &mut Context, field: &str) -> Result<Mapping> {
        self.elem.compile_assertion(ctx, field)
    }
}

impl ExprTuple {
    // assert: (status, body)
    fn compile_as_brief_dsl(&self, ctx: &mut Context) -> Result<Mapping> {
        // TODO: make status optional
        let (status, body) = <(&Expr, Option<&Expr>)>::unpack(self.span(), self.elems.items())?;
        if let Some(body) = body {
            let status = match status {
                Expr::Lit(ExprLit::Integer(i)) => i,
                _ => return Err(Error::new(status.span(), "`status` should be an integer")),
            };
            let body = match body {
                Expr::Object(obj) => obj,
                _ => return Err(Error::new(body.span(), "`body` should be an object")),
            };
            ctx.compile_brief(Some(status), Some(body))
        } else {
            let body = match status {
                Expr::Object(obj) => obj,
                _ => return Err(Error::new(status.span(), "`body` should be an object")),
            };
            ctx.compile_brief(None, Some(body))
        }
    }
}

impl Compilable for ExprTuple {
    fn display() -> &'static str {
        "tuple-expression"
    }
}
