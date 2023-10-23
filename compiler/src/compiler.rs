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

    pub fn compile(&self, ast: &Expr) -> Result<Yaml> {
        ast.compile_assertion(&self.context, "_ctx.response.body_json")
            .map(Yaml::from)
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
        "literal expression"
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
        "array expression"
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
        "object expression"
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        let assertions = self
            .fields
            .items()
            .map(|f| {
                let key = f.key.to_field();
                f.value
                    .compile_assertion(ctx, &format!("{field}.{key}"))
                    .map(Yaml::from)
            })
            .collect::<Result<Sequence>>()?;
        Ok(yaml!({ ["and"]: assertions }))
    }
}

impl FieldKey {
    fn to_field(&self) -> String {
        match self {
            Self::Path(i) => {
                let mut segments = i.segments.items();
                let mut name = if let Some(first) = segments.next() {
                    first.value().into()
                } else {
                    String::new()
                };
                for seg in segments {
                    name.push('.');
                    name.push_str(seg.value());
                }
                name
            }
            Self::String(s) => s.value().into(),
        }
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
        "unary expression"
    }

    fn compile_value(&self, ctx: &Context) -> Result<Yaml> {
        match self.op {
            UnaryOp::Neg(_) => self.neg(ctx),
            _ => Err(Error::new(self.span(), "cannot use assertion as value")),
        }
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        Ok(match self.op {
            UnaryOp::Eq(_) => yaml!({["equals"]: self.elem.compile_value(ctx)?}),
            UnaryOp::Neg(_) => yaml!({["equals"]: self.neg(ctx)?}),
            UnaryOp::Not(_) => yaml!({["not"]: self.elem.compile_assertion(ctx, field)?}),
            _ => {
                let op = match self.op {
                    UnaryOp::Ge(_) => "gte",
                    UnaryOp::Gt(_) => "gt",
                    UnaryOp::Le(_) => "lte",
                    UnaryOp::Lt(_) => "lt",
                    _ => unreachable!(),
                };
                yaml!({["range"]: {[field]: {[op]: self.elem.compile_value(ctx)?}}})
            }
        })
    }
}

impl Compilable for ExprBinary {
    fn display() -> &'static str {
        "binary expression"
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
        "function call expression"
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
