use crate::ast::*;
use loadgen_dsl_parser::{error::Result, Parser, Span};
use serde_yaml::{Mapping, Sequence, Value as Yaml};

pub trait Spanned {
    fn span(&self) -> Span;
}

pub trait Compilable {
    fn compile_value(&self) -> Result<Yaml> {
        todo!()
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        let _ = (ctx, field);
        todo!()
    }
}

pub struct Context {}

pub struct Compiler {
    context: Context,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            context: Context {},
        }
    }

    pub fn compile(&self, source: &str) -> Result<Yaml> {
        let ast = Parser::new(source).parse::<Expr>()?;
        ast.compile_assertion(&self.context, "_ctx.response.body_json")
            .map(Yaml::from)
    }
}

impl Compilable for Expr {
    fn compile_value(&self) -> Result<Yaml> {
        match self {
            Self::Lit(l) => l.compile_value(),
            Self::Unary(u) => u.compile_value(),
            _ => todo!(),
        }
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        match self {
            Self::Lit(l) => l.compile_assertion(ctx, field),
            Self::Array(a) => a.compile_assertion(ctx, field),
            Self::Object(o) => o.compile_assertion(ctx, field),
            Self::Unary(u) => u.compile_assertion(ctx, field),
            Self::Binary(b) => b.compile_assertion(ctx, field),
            Self::Funcall(f) => f.compile_assertion(ctx, field),
            Self::Paren(p) => p.compile_assertion(ctx, field),
        }
    }
}

impl Compilable for ExprLit {
    fn compile_value(&self) -> Result<Yaml> {
        Ok(match self {
            Self::Null(_) => Yaml::Null,
            Self::Bool(b) => Yaml::from(b.value()),
            Self::Number(n) => Yaml::from(n.value()),
            Self::String(s) => Yaml::from(s.value()),
            Self::Regexp(r) => Yaml::from(r.value()),
        })
    }

    fn compile_assertion(&self, _ctx: &Context, field: &str) -> Result<Mapping> {
        Ok(match self {
            Self::Regexp(_) => yaml!({
                ["regexp"]: {
                    [field]: self.compile_value()?,
                },
            }),
            _ => yaml!({
                ["equals"]: {
                    [field]: self.compile_value()?,
                },
            }),
        })
    }
}

impl Compilable for ExprArray {
    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        let assertions = self
            .elems
            .values()
            .enumerate()
            .map(|(i, elem)| {
                elem.compile_assertion(ctx, &format!("{field}.{i}"))
                    .map(Yaml::from)
            })
            .collect::<Result<Sequence>>()?;
        Ok(yaml!({["and"]: assertions}))
    }
}

impl Compilable for ExprObject {
    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        let assertions = self
            .fields
            .values()
            .map(|f| {
                let key = f.key.to_field();
                f.value
                    .compile_assertion(ctx, &format!("{field}.{key}"))
                    .map(Yaml::from)
            })
            .collect::<Result<Sequence>>()?;
        Ok(yaml!({["and"]: assertions }))
    }
}

impl FieldKey {
    fn to_field(&self) -> String {
        match self {
            Self::Path(i) => {
                let mut segments = i.segments.values();
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

impl Compilable for ExprUnary {
    fn compile_value(&self) -> Result<Yaml> {
        Ok(match self.op {
            UnaryOp::Neg(_) => match self.elem.compile_value()? {
                Yaml::Number(n) => Yaml::from(-n.as_f64().unwrap()),
                _ => todo!(),
            },
            _ => todo!(),
        })
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        Ok(match self.op {
            UnaryOp::Neg(_) => todo!(),
            UnaryOp::Not(_) => yaml!({["not"]: self.elem.compile_assertion(ctx, field)?}),
            UnaryOp::Eq(_) => yaml!({["equals"]: self.elem.compile_value()?}),
            _ => {
                let op = match self.op {
                    UnaryOp::Ge(_) => "gte",
                    UnaryOp::Gt(_) => "gt",
                    UnaryOp::Le(_) => "lte",
                    UnaryOp::Lt(_) => "lt",
                    _ => unreachable!(),
                };
                yaml!({["range"]: {[field]: {[op]: self.elem.compile_value()?}}})
            }
        })
    }
}

impl Compilable for ExprBinary {
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
    fn compile_assertion(&self, _ctx: &Context, _field: &str) -> Result<Mapping> {
        todo!()
    }
}

impl Compilable for ExprParen {
    fn compile_value(&self) -> Result<Yaml> {
        self.elem.compile_value()
    }

    fn compile_assertion(&self, ctx: &Context, field: &str) -> Result<Mapping> {
        self.elem.compile_assertion(ctx, field)
    }
}
