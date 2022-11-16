use std::{cmp::Ordering, fmt::Display, rc::Rc};

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, UnaryOp},
    Result,
};

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Value {
    #[default]
    Nil,
    Boolean(bool),
    Number(f64),
    Str(Rc<str>),
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
                UnaryOp::Neg => expr.neg(),
                UnaryOp::Not => expr.not(),
            }
        }
        Node::Binary { lhs, op, rhs } => {
            let lhs = eval(lhs)?;
            let rhs = eval(rhs)?;
            match op {
                BinaryOp::Equals => lhs.eq(&rhs),
                BinaryOp::NotEquals => lhs.neq(&rhs),
                BinaryOp::LessThan => lhs.lt(&rhs),
                BinaryOp::LessThanOrEqual => lhs.lte(&rhs),
                BinaryOp::GreaterThan => lhs.gt(&rhs),
                BinaryOp::GreaterThanOrEqual => lhs.gte(&rhs),
                BinaryOp::Add => lhs.add(&rhs),
                BinaryOp::Sub => lhs.sub(&rhs),
                BinaryOp::Mul => lhs.mul(&rhs),
                BinaryOp::Div => lhs.div(&rhs),
            }
        }
    };

    Ok(value)
}

impl Value {
    fn as_bool(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    fn as_num(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            Value::Str(s) => s.parse::<f64>().unwrap(),
            _ => panic!("Expected number literal, got {self:?}"),
        }
    }

    fn as_str(&self) -> Rc<str> {
        let Value::Str(s) = &self else {
            panic!("Expected string literal, got {self:?}");
        };
        Rc::clone(s)
    }

    fn not(&self) -> Self {
        Value::Boolean(!self.as_bool())
    }

    fn neg(&self) -> Self {
        Value::Number(-1.0 * self.as_num())
    }

    fn add(&self, rhs: &Self) -> Self {
        match self {
            Value::Number(lhs) => Value::Number(lhs + rhs.as_num()),
            Value::Str(lhs) => Value::Str(Rc::from(format!("{lhs}{}", rhs.as_str()))),
            _ => panic!("Expected number or string, got {self:?}"),
        }
    }

    fn sub(&self, rhs: &Self) -> Self {
        Value::Number(self.as_num() - rhs.as_num())
    }

    fn mul(&self, rhs: &Self) -> Self {
        Value::Number(self.as_num() * rhs.as_num())
    }

    fn div(&self, rhs: &Self) -> Self {
        Value::Number(self.as_num() / rhs.as_num())
    }

    fn eq(&self, rhs: &Self) -> Self {
        let b = self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Equal);
        Value::Boolean(b)
    }

    fn neq(&self, rhs: &Self) -> Self {
        let b = self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Equal);
        Value::Boolean(b)
    }

    fn lt(&self, rhs: &Self) -> Self {
        let b = self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Less);
        Value::Boolean(b)
    }

    fn gt(&self, rhs: &Self) -> Self {
        let b = self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord == Ordering::Greater);
        Value::Boolean(b)
    }

    fn lte(&self, rhs: &Self) -> Self {
        let b = self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Greater);
        Value::Boolean(b)
    }

    fn gte(&self, rhs: &Self) -> Self {
        let b = self
            .partial_cmp(rhs)
            .map_or(false, |ord| ord != Ordering::Less);
        Value::Boolean(b)
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
