use std::{cell::RefCell, cmp::Ordering, fmt::Display, rc::Rc, sync::Arc};

use crate::{
    ast::{BinaryOp, Expr, Literal, Node, Stmt, UnaryOp},
    env::Environment,
    error::{RuntimeError, SyntaxError},
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

pub fn interpreter<'a, I>(statements: I) -> Interpreter<'a, I::IntoIter>
where
    I: IntoIterator<Item = Stmt<'a>>,
{
    Interpreter::new(statements.into_iter())
}

pub struct Interpreter<'a, I: Iterator<Item = Stmt<'a>>> {
    env: Rc<RefCell<Environment<'a>>>,
    statements: I,
}

impl<'a, I: Iterator<Item = Stmt<'a>>> Interpreter<'a, I> {
    fn new(statements: I) -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
            statements,
        }
    }
}

impl<'a, I: Iterator<Item = Stmt<'a>>> Interpreter<'a, I> {
    fn eval_stmt(&mut self, stmt: Stmt<'a>) -> Result<()> {
        match stmt {
            Stmt::Block(stmts) => {
                let enc_env = self.env.clone();
                let new_env = Environment::with_enclosing(enc_env);
                self.eval_block(stmts, Rc::new(RefCell::new(new_env)))
            }
            Stmt::Expression(expr) => self.eval_expr(expr).map(|_| Ok(()))?,
            Stmt::Print(expr) => self.eval_expr(expr).map(|v| {
                println!("{v}");
                Ok(())
            })?,
            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Nil,
                };
                self.env.borrow_mut().define(name, value);
                Ok(())
            }
        }
    }

    fn eval_block(
        &mut self,
        stmts: Vec<Stmt<'a>>,
        env: Rc<RefCell<Environment<'a>>>,
    ) -> Result<()> {
        let prev = std::mem::replace(&mut self.env, env);
        // simulate try-catch
        let res = || -> Result<()> {
            for stmt in stmts {
                self.eval_stmt(stmt)?;
            }
            Ok(())
        };
        let res = res();
        // finally
        self.env = prev;
        res
    }

    fn eval_expr(&mut self, expr: Expr<'a>) -> Result<Value> {
        let value = match (*expr.node, expr.span) {
            (Node::Variable { name }, span) => match self.env.borrow().get(name) {
                Some(value) => value,
                None => return Err(SyntaxError::undefined_variable(name, span)),
            },
            (Node::Assign { name, expr }, span) => {
                let value = self.eval_expr(expr)?;
                match self.env.borrow_mut().assign(name, value.clone()) {
                    Some(_) => value,
                    None => return Err(SyntaxError::undefined_variable(name, span)),
                }
            }
            (Node::Literal { lit }, _) => Value::from(lit),
            (Node::Group { expr }, _) => self.eval_expr(expr)?,
            (Node::Unary { op, expr }, span) => {
                let val = self.eval_expr(expr)?;
                match op {
                    UnaryOp::Neg => val.neg(span)?,
                    UnaryOp::Not => val.not(span)?,
                }
            }
            (Node::Binary { lhs, op, rhs }, span) => {
                let lhs_val = self.eval_expr(lhs)?;
                let rhs_val = self.eval_expr(rhs)?;
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
}

impl<'a, I: Iterator<Item = Stmt<'a>>> Iterator for Interpreter<'a, I> {
    type Item = Result<()>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements
            .next()
            .map(|stmt| self.eval_stmt(stmt))
            .or(None)
    }
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
