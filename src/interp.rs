use crate::{
    env::{Environment, EnvironmentRef},
    error::SyntaxError,
    expr::{BinaryOp, Expr, ExprNode, LogicalOp, UnaryOp},
    stmt::{Stmt, StmtNode},
    value::Value,
    Result,
};

pub fn interpreter<'a, I>(statements: I) -> Interpreter<'a, I::IntoIter>
where
    I: IntoIterator<Item = StmtNode<'a>>,
{
    Interpreter::new(statements.into_iter())
}

pub struct Interpreter<'a, I: Iterator<Item = StmtNode<'a>>> {
    environment: EnvironmentRef<'a>,
    statements: I,
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Interpreter<'a, I> {
    fn new(statements: I) -> Self {
        Self {
            environment: Environment::new().into(),
            statements,
        }
    }
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Interpreter<'a, I> {
    fn eval_stmt(&mut self, stmt: StmtNode<'a>) -> Result<()> {
        match *stmt.item {
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
                self.environment.borrow_mut().define(name.item, value);
                Ok(())
            }
            Stmt::Block(stmts) => {
                let new_env = Environment::with_enclosing(self.environment.clone());
                self.eval_block(stmts, new_env.into())
            }
            Stmt::If(condition, then_branch, else_branch) => {
                if self.eval_expr(condition)?.as_bool()? {
                    self.eval_stmt(then_branch)?
                } else if let Some(else_branch) = else_branch {
                    self.eval_stmt(else_branch)?
                }
                Ok(())
            }
        }
    }

    fn eval_block(&mut self, stmts: Vec<StmtNode<'a>>, env: EnvironmentRef<'a>) -> Result<()> {
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

    fn eval_expr(&mut self, expr: ExprNode<'a>) -> Result<Value> {
        let span = expr.span;
        let value = match *expr.item {
            Expr::Variable { name } => match self.environment.borrow().get(name) {
                Some(value) => value,
                None => return Err(SyntaxError::undefined_variable(name, span)),
            },
            Expr::Assign { name, expr } => {
                let value = self.eval_expr(expr)?;
                match self.environment.borrow_mut().assign(name, value.clone()) {
                    Some(_) => value,
                    None => return Err(SyntaxError::undefined_variable(name, span)),
                }
            }
            Expr::Literal { lit } => Value::from(lit),
            Expr::Group { expr } => self.eval_expr(expr)?,
            Expr::Unary { op, expr } => {
                let val = self.eval_expr(expr)?;
                match op {
                    UnaryOp::Neg => val.neg(span)?,
                    UnaryOp::Not => val.not()?,
                }
            }
            Expr::Binary { lhs, op, rhs } => {
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
            Expr::Logical { lhs, op, rhs } => {
                let lhs_val = self.eval_expr(lhs)?;
                match op {
                    LogicalOp::Or if lhs_val.as_bool()? => lhs_val,
                    LogicalOp::And if !lhs_val.as_bool()? => lhs_val,
                    _ => self.eval_expr(rhs)?,
                }
            }
        };

        Ok(value)
    }
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Iterator for Interpreter<'a, I> {
    type Item = Result<()>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements
            .next()
            .map(|stmt| self.eval_stmt(stmt))
            .or(None)
    }
}
