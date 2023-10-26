use std::borrow::Cow;

use crate::{
    ast::*,
    error::{Error, Result},
};
use serde_yaml::{Mapping, Sequence, Value as Yaml};

pub(crate) trait Compilable: Spanned {
    fn display() -> &'static str;

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
        let _ = ctx;
        Err(Error::new(
            self.span(),
            format!("cannot use {} as value", Self::display()),
        ))
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        let _ = (ctx, field);
        Err(Error::new(
            self.span(),
            format!("cannot use {} as assertion", Self::display()),
        ))
    }
}

pub(crate) struct Context {}

pub(crate) struct Compiler {
    context: Context,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            context: Context {},
        }
    }

    pub fn compile(&self, ast: &Dsl) -> Result<Mapping> {
        Ok(match ast {
            Dsl::Brief(ast) => {
                let body = ast
                    .body
                    .compile_assertion(&self.context, "_ctx.response.body_json")?;
                let assertion = if let Some(status) = ast.status.as_ref() {
                    let status = yaml!({
                        ["equals"]: {
                            ["_ctx.response.status"]: status.value()
                        }
                    });
                    yaml!({ ["and"]: [status, body] })
                } else {
                    body
                };
                yaml!({ ["assert"]: assertion })
            }
            Dsl::Full(ast) => ast
                .fields
                .items()
                .map(|f| {
                    let key = f.path.to_field();
                    let value = if key == "assert" {
                        if let Expr::Object(expr) = &*f.value {
                            compile_fields_in(expr.fields.items(), &self.context, None)?.into()
                        } else {
                            return Err(Error::new(
                                f.value.span(),
                                format!("only {} is supported as assertion", ExprObject::display()),
                            ));
                        }
                    } else {
                        f.value.compile_value(&self.context)?
                    };
                    Ok((Yaml::from(key), value))
                })
                .collect::<Result<Mapping>>()?,
        })
    }
}

impl Compilable for Expr {
    fn display() -> &'static str {
        "expression"
    }

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
        match self {
            Self::Array(t) => t.compile_value(ctx),
            Self::Binary(t) => t.compile_value(ctx),
            Self::Funcall(t) => t.compile_value(ctx),
            Self::Lit(t) => t.compile_value(ctx),
            Self::Object(t) => t.compile_value(ctx),
            Self::Paren(t) => t.compile_value(ctx),
            Self::Unary(t) => t.compile_value(ctx),
        }
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        match self {
            Self::Array(t) => t.compile_assertion(ctx, field),
            Self::Binary(t) => t.compile_assertion(ctx, field),
            Self::Funcall(t) => t.compile_assertion(ctx, field),
            Self::Lit(t) => t.compile_assertion(ctx, field),
            Self::Object(t) => t.compile_assertion(ctx, field),
            Self::Paren(t) => t.compile_assertion(ctx, field),
            Self::Unary(t) => t.compile_assertion(ctx, field),
        }
    }
}

impl Compilable for ExprLit {
    fn display() -> &'static str {
        "literal-expression"
    }

    fn compile_value(&self, _ctx: &Context) -> Result<Yaml> {
        Ok(match self {
            Self::Bool(t) => Yaml::from(t.value()),
            Self::Float(t) => Yaml::from(t.value()),
            Self::Integer(t) => Yaml::from(t.value()),
            Self::Null(_) => Yaml::Null,
            Self::Regexp(t) => Yaml::from(t.value()),
            Self::String(t) => Yaml::from(t.value()),
        })
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
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

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
        self.elems
            .items()
            .map(|elem| elem.compile_value(ctx).map(Yaml::from))
            .collect::<Result<Sequence>>()
            .map(Yaml::from)
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        let assertions = self
            .elems
            .items()
            .enumerate()
            .map(|(i, elem)| {
                elem.compile_assertion(ctx, &format!("{field}.[{i}]"))
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

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
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

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        compile_fields_in(self.fields.items(), ctx, Some(field))
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
    fn neg(&self, ctx: &Context) -> Result<Yaml> {
        match self.elem.compile_value(ctx)? {
            Yaml::Number(n) => Ok(n
                .as_i64()
                .map(|i| Yaml::from(-i))
                .or_else(|| n.as_f64().map(|i| Yaml::from(-i)))
                .unwrap()),
            _ => Err(Error::new(
                self.span(),
                format!("can only apply negation to numbers"),
            )),
        }
    }
}

impl Compilable for ExprUnary {
    fn display() -> &'static str {
        "unary-expression"
    }

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
        match self.op {
            UnaryOp::Neg(_) => self.neg(ctx),
            _ => Err(Error::new(self.span(), "cannot use assertion as value")),
        }
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
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

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
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

impl Compilable for ExprFuncall {
    fn display() -> &'static str {
        "function-call-expression"
    }

    fn compile_value(&self, _ctx: &Context) -> Result<Yaml> {
        Err(Error::new(
            self.span(),
            format!("{} is not yet supported as value", Self::display()),
        ))
    }

    fn compile_assertion(&self, _ctx: &Context, _field: &str) -> Result<Mapping> {
        Err(Error::new(
            self.span(),
            format!("{} is not yet supported as assertion", Self::display()),
        ))
    }
}

impl Compilable for ExprParen {
    fn display() -> &'static str {
        Expr::display()
    }

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
        self.elem.compile_value(ctx)
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        self.elem.compile_assertion(ctx, field)
    }
}

fn compile_fields_in<'a>(
    fields: impl 'a + IntoIterator<Item = &'a Field>,
    ctx: &Context,
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
            f.value.compile_assertion(ctx, &key).map(Yaml::from)
        })
        .collect::<Result<Sequence>>()?;
    Ok(yaml!({ ["and"]: assertions }))
}
