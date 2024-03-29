use std::collections::HashMap;

use crate::{
    callable::{Class, Function},
    env::{Environment, EnvironmentRef},
    error::{BolloxError, RuntimeError, SyntaxError},
    expr::{BinaryOp, Expr, ExprNode, LogicalOp, UnaryOp},
    stmt::{Stmt, StmtNode},
    token::Span,
    value::Value,
    Result,
};

pub fn interpreter<'a, I>(
    statements: I,
    context: InterpreterContext<'a>,
) -> Interpreter<'a, I::IntoIter>
where
    I: IntoIterator<Item = StmtNode<'a>>,
{
    Interpreter::new(statements.into_iter(), context)
}

pub struct Interpreter<'a, I: Iterator<Item = StmtNode<'a>>> {
    context: InterpreterContext<'a>,
    statements: I,
}

pub struct InterpreterContext<'a> {
    pub(crate) environment: EnvironmentRef<'a>,
    pub(crate) locals: HashMap<Span, usize>,
}

impl<'a> Default for InterpreterContext<'a> {
    fn default() -> Self {
        Self {
            environment: Environment::default().into(),
            locals: HashMap::new(),
        }
    }
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Interpreter<'a, I> {
    fn new(statements: I, context: InterpreterContext<'a>) -> Self {
        Self {
            context,
            statements,
        }
    }
}

type InterpreterResult<'a> = Result<(), InterpreterError<'a>>;

pub(crate) struct InterpreterOps;

impl InterpreterOps {
    pub(crate) fn eval_stmts<'a>(
        context: &mut InterpreterContext<'a>,
        stmts: &[StmtNode<'a>],
        env: EnvironmentRef<'a>,
    ) -> InterpreterResult<'a> {
        let prev = std::mem::replace(&mut context.environment, env);
        // try-catch
        let mut eval_stmts = || -> Result<(), InterpreterError<'a>> {
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

    fn eval_stmt<'a>(
        context: &mut InterpreterContext<'a>,
        stmt: &StmtNode<'a>,
    ) -> InterpreterResult<'a> {
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
            Stmt::Func(declaration) => {
                let fun = Function::new(declaration, context.environment.clone(), false);
                context
                    .environment
                    .borrow_mut()
                    .define(declaration.name.item, fun.into());
                Ok(())
            }
            Stmt::Return(value) => match value {
                Some(value) => {
                    let value = Self::eval_expr(context, value)?;
                    Err(InterpreterError::Return(value))
                }
                None => Err(InterpreterError::Return(Value::Nil)),
            },
            Stmt::Class(declaration) => {
                let superclass = match &declaration.superclass {
                    Some(superclass) => match Self::eval_expr(context, superclass)? {
                        Value::Clazz(c) => Some(c),
                        _ => return Err(RuntimeError::super_no_class(stmt.span).into()),
                    },
                    None => None,
                };

                let class_name = declaration.name.item;
                context
                    .environment
                    .borrow_mut()
                    .define(class_name, Value::Nil);

                if let Some(c) = superclass {
                    // We create a new environment that contains a reference
                    // to the super class. That environment will be the one
                    // to execute the current class methods with so they can
                    // find `super`.
                    context.environment =
                        Environment::with_enclosing(context.environment.clone()).into();
                    context
                        .environment
                        .borrow_mut()
                        .define("super", Value::Clazz(c));
                }

                let methods = declaration
                    .methods
                    .iter()
                    .map(|declaration| {
                        (
                            declaration.item.name.item,
                            Function::new(
                                &declaration.item,
                                context.environment.clone(),
                                declaration.item.name.item == "init",
                            ),
                        )
                    })
                    .collect::<HashMap<_, _>>();

                let class =
                    Class::new(class_name, methods, superclass, context.environment.clone());
                let class = Box::new(class);
                let class = Box::leak(class);

                if superclass.is_some() {
                    let enclosing = context
                        .environment
                        .borrow_mut()
                        .enclosing
                        .clone()
                        .expect("Superclass without subclass environment.");
                    context.environment = enclosing;
                }
                context
                    .environment
                    .borrow_mut()
                    .assign(class_name, class.into());

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
            Expr::Variable { name } => Self::get_var(context, expr, name, 0)?,
            Expr::Assign { name, expr } => Self::assign_var(context, expr, name)?,
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
            Expr::Call { callee, args } => {
                let span = callee.span;
                let callee = Self::eval_expr(context, callee)?;
                let callable = callee.as_callable(span)?;

                let args = args
                    .iter()
                    .map(|arg| Self::eval_expr(context, arg))
                    .collect::<Result<Vec<_>>>()?;

                if args.len() != callable.arity() {
                    return Err(RuntimeError::args_len(callable.arity(), args.len(), span));
                }

                callable.call(context, &args, span)?
            }
            Expr::Get { object, name } => match Self::eval_expr(context, object)? {
                Value::Instance(instance) => match instance.get(name.item) {
                    Some(value) => value,
                    None => return Err(RuntimeError::undefined_property(name.item, name.span)),
                },
                _ => return Err(RuntimeError::invalid_property_call(name.span)),
            },
            Expr::Set {
                object,
                name,
                value,
            } => match Self::eval_expr(context, object)? {
                Value::Instance(instance) => {
                    let value = Self::eval_expr(context, value)?;
                    instance.set(name.item, value.clone());
                    value
                }
                _ => return Err(RuntimeError::invalid_property_call(name.span)),
            },
            Expr::This { keyword } => Self::get_var(context, expr, keyword.item, 0)?,
            Expr::Super { keyword, method } => {
                let distance = context
                    .locals
                    .get(&expr.span)
                    .expect("`super` has not been resolved correctly.")
                    - 1;

                let super_class = context
                    .environment
                    .borrow()
                    .get_at(keyword.item, distance)
                    .and_then(|value| match value {
                        Value::Clazz(c) => Some(c),
                        _ => None,
                    })
                    .expect("Value must be a Clazz");

                let object = context
                    .environment
                    .borrow()
                    .get_at("this", distance - 1)
                    .and_then(|value| match value {
                        Value::Instance(i) => Some(i),
                        _ => None,
                    })
                    .expect("Value must be an Instance");

                match super_class.get_method(method.item) {
                    Some(method) => method.bind(object.clone()).into(),
                    None => return Err(RuntimeError::undefined_property(method.item, method.span)),
                }
            }
            Expr::Lambda { declaration } => {
                Function::new(declaration, context.environment.clone(), false).into()
            }
        };

        Ok(value)
    }

    // Resolves a var expression either from local or global scope.
    fn get_var<'a>(
        context: &mut InterpreterContext<'a>,
        expr: &ExprNode<'a>,
        name: &'a str,
        offset: usize,
    ) -> Result<Value<'a>> {
        context
            .locals
            .get(&expr.span)
            .and_then(|distance| {
                context
                    .environment
                    .borrow()
                    .get_at(name, *distance + offset)
            })
            .or_else(|| context.environment.borrow().get_global(name))
            .ok_or(SyntaxError::undefined_variable(name, expr.span))
    }

    fn assign_var<'a>(
        context: &mut InterpreterContext<'a>,
        expr: &ExprNode<'a>,
        name: &'a str,
    ) -> Result<Value<'a>> {
        let value = Self::eval_expr(context, expr)?;
        context
            .locals
            .get(&expr.span)
            .and_then(|distance| {
                context
                    .environment
                    .borrow_mut()
                    .assign_at(name, value.clone(), *distance)
            })
            .or_else(|| {
                context
                    .environment
                    .borrow_mut()
                    .assign_global(name, value.clone())
            })
            .ok_or(SyntaxError::undefined_variable(name, expr.span))
    }
}

impl<'a, I: Iterator<Item = StmtNode<'a>>> Iterator for Interpreter<'a, I> {
    type Item = Result<()>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statements.next().map(|stmt| {
            match InterpreterOps::eval_stmt(&mut self.context, &stmt) {
                Ok(()) => Ok(()),
                Err(InterpreterError::Return(value)) => {
                    println!("{value}");
                    Ok(())
                }
                Err(InterpreterError::Err(e)) => Err(e),
            }
        })
    }
}

#[derive(Clone, Debug)]
pub(crate) enum InterpreterError<'a> {
    Return(Value<'a>),
    Err(BolloxError),
}

impl<'a> From<RuntimeError> for InterpreterError<'a> {
    fn from(e: RuntimeError) -> Self {
        Self::Err(e.into())
    }
}

impl<'a> From<Value<'a>> for InterpreterError<'a> {
    fn from(value: Value<'a>) -> Self {
        Self::Return(value)
    }
}

impl<'a> From<BolloxError> for InterpreterError<'a> {
    fn from(error: BolloxError) -> Self {
        Self::Err(error)
    }
}
