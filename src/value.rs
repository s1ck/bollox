use std::fmt::Display;
use std::{cmp::Ordering, rc::Rc};

use crate::callable::{Builtins, Callables, Class, Instance};
use crate::{
    callable::Function,
    error::RuntimeError,
    expr::{BinaryOp, Literal},
    token::Span,
    Result,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Value<'a> {
    #[default]
    Nil,
    Boolean(bool),
    Number(f64),
    Str(Rc<str>),
    Fn(Rc<Function<'a>>),
    Clazz(&'a Class<'a>),
    Builtin(Rc<Builtins>),
    Instance(Rc<Instance<'a>>),
}

impl<'a> From<Literal<'_>> for Value<'a> {
    fn from(lit: Literal<'_>) -> Self {
        match lit {
            Literal::Nil => Value::Nil,
            Literal::True => Value::Boolean(true),
            Literal::False => Value::Boolean(false),
            Literal::Number(num) => Value::Number(num),
            Literal::String(str) => Value::Str(str.into()),
        }
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => b.fmt(f),
            Value::Number(n) => n.fmt(f),
            Value::Str(s) => s.fmt(f),
            Value::Fn(fun) => fun.fmt(f),
            Value::Clazz(class) => class.fmt(f),
            Value::Instance(instance) => instance.fmt(f),
            Value::Builtin(b) => b.fmt(f),
        }
    }
}

impl<'a> Value<'a> {
    pub(crate) fn not(&self) -> Result<Self> {
        let b = self.as_bool()?;
        Ok((!b).into())
    }

    pub(crate) fn neg(&self, span: Span) -> Result<Self> {
        let n = self.as_num(span)?;
        Ok((-1.0 * n).into())
    }

    pub(crate) fn add(&self, rhs: &Self, span: Span) -> Result<Self> {
        Ok(match self {
            Value::Number(lhs) => {
                let rhs = rhs.as_num(span)?;
                (lhs + rhs).into()
            }
            Value::Str(lhs) => {
                let rhs = rhs.as_str(span)?;
                format!("{lhs}{rhs}").into()
            }
            _ => {
                return Err(RuntimeError::incompatible_types(
                    BinaryOp::Add,
                    self,
                    rhs,
                    span,
                ))
            }
        })
    }

    pub(crate) fn sub(&self, rhs: &Self, span: Span) -> Result<Self> {
        let lhs = self.as_num(span)?;
        let rhs = rhs.as_num(span)?;
        Ok((lhs - rhs).into())
    }

    pub(crate) fn mul(&self, rhs: &Self, span: Span) -> Result<Self> {
        let lhs = self.as_num(span)?;
        let rhs = rhs.as_num(span)?;
        Ok((lhs * rhs).into())
    }

    pub(crate) fn div(&self, rhs: &Self, span: Span) -> Result<Self> {
        let lhs = self.as_num(span)?;
        let rhs = rhs.as_num(span)?;
        Ok((lhs / rhs).into())
    }

    pub(crate) fn eq(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Equal)
            .into())
    }

    pub(crate) fn neq(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Equal)
            .into())
    }

    pub(crate) fn lt(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Less)
            .into())
    }

    pub(crate) fn gt(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Greater)
            .into())
    }

    pub(crate) fn lte(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Greater)
            .into())
    }

    pub(crate) fn gte(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Less)
            .into())
    }

    pub(crate) fn as_bool(&self) -> Result<bool> {
        Ok(match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        })
    }

    fn as_num(&self, span: Span) -> Result<f64> {
        let Value::Number(n) = self else {
           return Err(RuntimeError::non_number(self, span))
        };
        Ok(*n)
    }

    fn as_str(&self, span: Span) -> Result<Rc<str>> {
        let Value::Str(s) = self else {
            return Err(RuntimeError::non_str(self, span))
        };
        Ok(Rc::clone(s))
    }

    pub(crate) fn as_callable(&self, span: Span) -> Result<Callables<'a>> {
        let callable = match self {
            Value::Fn(f) => Callables::Fn(Rc::clone(f)),
            Value::Builtin(b) => Callables::Builtin(Rc::clone(b)),
            Value::Clazz(c) => Callables::Clazz(c),
            v => return Err(RuntimeError::non_callable(v, span)),
        };
        Ok(callable)
    }

    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Value::Boolean(l), Value::Boolean(r)) => l.partial_cmp(r),
            (Value::Number(l), Value::Number(r)) => l.partial_cmp(r),
            (Value::Str(l), Value::Str(r)) => l.partial_cmp(r),
            (Value::Nil, Value::Nil) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl<'a> From<f64> for Value<'a> {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl<'a> From<String> for Value<'a> {
    fn from(s: String) -> Self {
        Value::Str(Rc::from(s))
    }
}

impl<'a> From<Function<'a>> for Value<'a> {
    fn from(f: Function<'a>) -> Self {
        Value::Fn(Rc::from(f))
    }
}

impl<'a> From<&'a mut Class<'a>> for Value<'a> {
    fn from(c: &'a mut Class<'a>) -> Self {
        Value::Clazz(c)
    }
}

impl<'a> From<Instance<'a>> for Value<'a> {
    fn from(i: Instance<'a>) -> Self {
        Value::Instance(Rc::new(i))
    }
}

impl<'a> From<Rc<Instance<'a>>> for Value<'a> {
    fn from(i: Rc<Instance<'a>>) -> Self {
        Value::Instance(i)
    }
}
