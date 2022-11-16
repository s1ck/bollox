use std::{cmp::Ordering, fmt::Display, sync::Arc};

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, UnaryOp},
    error::RuntimeError,
    Result,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Value {
    #[default]
    Nil,
    Boolean(bool),
    Number(f64),
    Str(Arc<str>),
}

impl From<Literal<'_>> for Value {
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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => b.fmt(f),
            Value::Number(n) => n.fmt(f),
            Value::Str(s) => s.fmt(f),
        }
    }
}

pub fn eval(expr: Expr<'_>) -> Result<Value> {
    let value = match *expr.node {
        Node::Literal { lit } => Value::from(lit),
        Node::Group { expr } => eval(expr)?,
        Node::Unary { op, expr } => {
            let expr = eval(expr)?;
            match op {
                UnaryOp::Neg => expr.neg()?,
                UnaryOp::Not => expr.not()?,
            }
        }
        Node::Binary { lhs, op, rhs } => {
            let lhs = eval(lhs)?;
            let rhs = eval(rhs)?;
            match op {
                BinaryOp::Equals => lhs.eq(&rhs)?,
                BinaryOp::NotEquals => lhs.neq(&rhs)?,
                BinaryOp::LessThan => lhs.lt(&rhs)?,
                BinaryOp::LessThanOrEqual => lhs.lte(&rhs)?,
                BinaryOp::GreaterThan => lhs.gt(&rhs)?,
                BinaryOp::GreaterThanOrEqual => lhs.gte(&rhs)?,
                BinaryOp::Add => lhs.add(&rhs)?,
                BinaryOp::Sub => lhs.sub(&rhs)?,
                BinaryOp::Mul => lhs.mul(&rhs)?,
                BinaryOp::Div => lhs.div(&rhs)?,
            }
        }
    };

    Ok(value)
}

impl Value {
    fn as_bool(&self) -> Result<bool> {
        Ok(match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        })
    }

    fn as_num(&self) -> Result<f64> {
        let Value::Number(n) = self else {
           return Err(RuntimeError::non_number(self))
        };
        Ok(*n)
    }

    fn as_str(&self) -> Result<Arc<str>> {
        let Value::Str(s) = self else {
            return Err(RuntimeError::non_str(self))
        };
        Ok(Arc::clone(s))
    }

    fn not(&self) -> Result<Self> {
        let b = self.as_bool()?;
        Ok((!b).into())
    }

    fn neg(&self) -> Result<Self> {
        let n = self.as_num()?;
        Ok((-1.0 * n).into())
    }

    fn add(&self, rhs: &Self) -> Result<Self> {
        Ok(match self {
            Value::Number(lhs) => {
                let rhs = rhs.as_num()?;
                (lhs + rhs).into()
            }
            Value::Str(lhs) => {
                let rhs = rhs.as_str()?;
                format!("{}{}", lhs, rhs).into()
            }
            _ => return Err(RuntimeError::incompatible_types(BinaryOp::Add, self, rhs)),
        })
    }

    fn sub(&self, rhs: &Self) -> Result<Self> {
        let lhs = self.as_num()?;
        let rhs = rhs.as_num()?;
        Ok((lhs - rhs).into())
    }

    fn mul(&self, rhs: &Self) -> Result<Self> {
        let lhs = self.as_num()?;
        let rhs = rhs.as_num()?;
        Ok((lhs * rhs).into())
    }

    fn div(&self, rhs: &Self) -> Result<Self> {
        let lhs = self.as_num()?;
        let rhs = rhs.as_num()?;
        Ok((lhs / rhs).into())
    }

    fn eq(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Equal)
            .into())
    }

    fn neq(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Equal)
            .into())
    }

    fn lt(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Less)
            .into())
    }

    fn gt(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Greater)
            .into())
    }

    fn lte(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Greater)
            .into())
    }

    fn gte(&self, rhs: &Self) -> Result<Self> {
        Ok(self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Less)
            .into())
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

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Str(Arc::from(s))
    }
}
