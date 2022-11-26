use std::{cmp::Ordering, fmt::Display, sync::Arc};

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, Stmt, UnaryOp},
    error::RuntimeError,
    token::Span,
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

pub fn eval_stmt(stmt: Stmt<'_>) -> Result<()> {
    match stmt {
        Stmt::Expression(expr) => eval(expr).map(|_| Ok(()))?,
        Stmt::Print(expr) => eval(expr).map(|v| {
            println!("{v}");
            Ok(())
        })?,
    }
}

pub fn eval(expr: Expr<'_>) -> Result<Value> {
    let value = match (*expr.node, expr.span) {
        (Node::Literal { lit }, _) => Value::from(lit),
        (Node::Group { expr }, _) => eval(expr)?,
        (Node::Unary { op, expr }, span) => {
            let val = eval(expr)?;
            match op {
                UnaryOp::Neg => val.neg(span)?,
                UnaryOp::Not => val.not(span)?,
            }
        }
        (Node::Binary { lhs, op, rhs }, span) => {
            let lhs_val = eval(lhs)?;
            let rhs_val = eval(rhs)?;
            match op {
                BinaryOp::Equals => lhs_val.eq(&rhs_val)?,
                BinaryOp::NotEquals => lhs_val.neq(&rhs_val)?,
                BinaryOp::LessThan => lhs_val.lt(&rhs_val)?,
                BinaryOp::LessThanOrEqual => lhs_val.lte(&rhs_val)?,
                BinaryOp::GreaterThan => lhs_val.gt(&rhs_val)?,
                BinaryOp::GreaterThanOrEqual => lhs_val.gte(&rhs_val)?,
                BinaryOp::Add => lhs_val.add(&rhs_val, span)?,
                BinaryOp::Sub => lhs_val.sub(&rhs_val, span)?,
                BinaryOp::Mul => lhs_val.mul(&rhs_val, span)?,
                BinaryOp::Div => lhs_val.div(&rhs_val, span)?,
            }
        }
    };

    Ok(value)
}

impl Value {
    fn as_bool(&self, _span: Span) -> Result<bool> {
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

    fn as_str(&self, span: Span) -> Result<Arc<str>> {
        let Value::Str(s) = self else {
            return Err(RuntimeError::non_str(self, span))
        };
        Ok(Arc::clone(s))
    }

    fn not(&self, span: Span) -> Result<Self> {
        let b = self.as_bool(span)?;
        Ok((!b).into())
    }

    fn neg(&self, span: Span) -> Result<Self> {
        let n = self.as_num(span)?;
        Ok((-1.0 * n).into())
    }

    fn add(&self, rhs: &Self, span: Span) -> Result<Self> {
        Ok(match self {
            Value::Number(lhs) => {
                let rhs = rhs.as_num(span)?;
                (lhs + rhs).into()
            }
            Value::Str(lhs) => {
                let rhs = rhs.as_str(span)?;
                format!("{}{}", lhs, rhs).into()
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

    fn sub(&self, rhs: &Self, span: Span) -> Result<Self> {
        let lhs = self.as_num(span)?;
        let rhs = rhs.as_num(span)?;
        Ok((lhs - rhs).into())
    }

    fn mul(&self, rhs: &Self, span: Span) -> Result<Self> {
        let lhs = self.as_num(span)?;
        let rhs = rhs.as_num(span)?;
        Ok((lhs * rhs).into())
    }

    fn div(&self, rhs: &Self, span: Span) -> Result<Self> {
        let lhs = self.as_num(span)?;
        let rhs = rhs.as_num(span)?;
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
