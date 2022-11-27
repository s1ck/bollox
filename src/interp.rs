use crate::{
    ast::{BinaryOp, Expr, Node, Stmt, UnaryOp},
    env::{Environment, EnvironmentRef},
    error::SyntaxError,
    value::Value,
    Result,
};

pub fn interpreter<'a, I>(statements: I) -> Interpreter<'a, I::IntoIter>
where
    I: IntoIterator<Item = Stmt<'a>>,
{
    Interpreter::new(statements.into_iter())
}

pub struct Interpreter<'a, I: Iterator<Item = Stmt<'a>>> {
    environment: EnvironmentRef<'a>,
    statements: I,
}

impl<'a, I: Iterator<Item = Stmt<'a>>> Interpreter<'a, I> {
    fn new(statements: I) -> Self {
        Self {
            environment: Environment::new().into(),
            statements,
        }
    }
}

impl<'a, I: Iterator<Item = Stmt<'a>>> Interpreter<'a, I> {
    fn eval_stmt(&mut self, stmt: Stmt<'a>) -> Result<()> {
        match stmt {
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
                self.environment.borrow_mut().define(name, value);
                Ok(())
            }
            Stmt::Block(stmts) => {
                let new_env = Environment::with_enclosing(self.environment.clone());
                self.eval_block(stmts, new_env.into())
            }
        }
    }

    fn eval_block(&mut self, stmts: Vec<Stmt<'a>>, env: EnvironmentRef<'a>) -> Result<()> {
        let prev = std::mem::replace(&mut self.environment, env);
        // try-catch
        let eval_stmts = || -> Result<()> {
            for stmt in stmts {
                self.eval_stmt(stmt)?;
            }
            Ok(())
        };
        let res = eval_stmts();
        // finally
        self.environment = prev;
        res
    }

    fn eval_expr(&mut self, expr: Expr<'a>) -> Result<Value> {
        let value = match (*expr.node, expr.span) {
            (Node::Variable { name }, span) => match self.environment.borrow().get(name) {
                Some(value) => value,
                None => return Err(SyntaxError::undefined_variable(name, span)),
            },
            (Node::Assign { name, expr }, span) => {
                let value = self.eval_expr(expr)?;
                match self.environment.borrow_mut().assign(name, value.clone()) {
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
