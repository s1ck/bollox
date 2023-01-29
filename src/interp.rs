use crate::{
    callable::{Callable, Function},
    env::{Environment, EnvironmentRef},
    error::SyntaxError,
    expr::{BinaryOp, Expr, ExprNode, LogicalOp, UnaryOp},
    stmt::{FunctionKind, Stmt, StmtNode},
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
    context: InterpreterContext<'a>,
    statements: I,
}

pub(crate) struct InterpreterContext<'a> {
    pub(crate) globals: EnvironmentRef<'a>,
    pub(crate) environment: EnvironmentRef<'a>,
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Interpreter<'a, I> {
    fn new(statements: I) -> Self {
        let globals: EnvironmentRef = Environment::new().into();

        let context = InterpreterContext {
            globals: globals.clone(),
            environment: globals,
        };

        Self {
            context,
            statements,
        }
    }
}

pub(crate) struct InterpreterOps;

impl InterpreterOps {
    pub(crate) fn eval_stmts<'a>(
        context: &mut InterpreterContext<'a>,
        stmts: &[StmtNode<'a>],
        env: EnvironmentRef<'a>,
    ) -> Result<()> {
        let prev = std::mem::replace(&mut context.environment, env);
        // try-catch
        let mut eval_stmts = || -> Result<()> {
            for stmt in stmts {
                Self::eval_stmt(context, stmt)?;
            }
            Ok(())
        };
        let res = eval_stmts();
        // finally
        context.environment = prev;
        res
    }

    fn eval_stmt<'a>(context: &mut InterpreterContext<'a>, stmt: &StmtNode<'a>) -> Result<()> {
        match &*stmt.item {
            Stmt::Expression(expr) => Self::eval_expr(context, expr).map(|_| Ok(()))?,
            Stmt::Print(expr) => Self::eval_expr(context, expr).map(|v| {
                println!("{v}");
                Ok(())
            })?,
            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(expr) => Self::eval_expr(context, expr)?,
                    None => Value::Nil,
                };
                context.environment.borrow_mut().define(name.item, value);
                Ok(())
            }
            Stmt::Block(stmts) => {
                let new_env = Environment::with_enclosing(context.environment.clone());
                Self::eval_stmts(context, stmts, new_env.into())
            }
            Stmt::If(condition, then_branch, else_branch) => {
                if Self::eval_expr(context, condition)?.as_bool()? {
                    Self::eval_stmt(context, then_branch)?
                } else if let Some(else_branch) = else_branch {
                    Self::eval_stmt(context, else_branch)?
                }
                Ok(())
            }
            Stmt::While(condition, statement) => {
                while Self::eval_expr(context, condition)?.as_bool()? {
                    Self::eval_stmt(context, statement)?
                }
                Ok(())
            }
            Stmt::Function(declaration) => {
                let fun = Function::new(declaration);
                context
                    .environment
                    .borrow_mut()
                    .define(declaration.name.item, fun.into());
                Ok(())
            }
        }
    }

    fn eval_expr<'a>(
        context: &mut InterpreterContext<'a>,
        expr: &ExprNode<'a>,
    ) -> Result<Value<'a>> {
        let span = expr.span;
        let value = match &*expr.item {
            Expr::Variable { name } => match context.environment.borrow().get(name) {
                Some(value) => value,
                None => return Err(SyntaxError::undefined_variable(*name, span)),
            },
            Expr::Assign { name, expr } => {
                let value = Self::eval_expr(context, expr)?;
                match context.environment.borrow_mut().assign(name, value.clone()) {
                    Some(_) => value,
                    None => return Err(SyntaxError::undefined_variable(*name, span)),
                }
            }
            Expr::Literal { lit } => Value::from(*lit),
            Expr::Group { expr } => Self::eval_expr(context, expr)?,
            Expr::Unary { op, expr } => {
                let val = Self::eval_expr(context, expr)?;
                match op {
                    UnaryOp::Neg => val.neg(span)?,
                    UnaryOp::Not => val.not()?,
                }
            }
            Expr::Binary { lhs, op, rhs } => {
                let lhs_val = Self::eval_expr(context, lhs)?;
                let rhs_val = Self::eval_expr(context, rhs)?;
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
                let lhs_val = Self::eval_expr(context, lhs)?;
                match op {
                    LogicalOp::Or if lhs_val.as_bool()? => lhs_val,
                    LogicalOp::And if !lhs_val.as_bool()? => lhs_val,
                    _ => Self::eval_expr(context, rhs)?,
                }
            }
            Expr::Call { kind, callee, args } => {
                let span = callee.span;
                let callee = Self::eval_expr(context, callee)?;
                let callable = match kind {
                    FunctionKind::Function => callee.as_function(span)?,
                };

                let args = args
                    .iter()
                    .map(|arg| Self::eval_expr(context, arg))
                    .collect::<Result<Vec<_>>>()?;

                if args.len() != callable.arity() {
                    todo!("runtime error arity mismatch");
                }

                callable.call(context, &args, span)?
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
            .map(|stmt| InterpreterOps::eval_stmt(&mut self.context, &stmt))
            .or(None)
    }
}
