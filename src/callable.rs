use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::env::Environment;
use crate::interp::{InterpreterContext, InterpreterError, InterpreterOps};
use crate::node::Node;
use crate::stmt::{FunctionDeclaration, StmtNode};
use crate::token::Span;
use crate::value::Value;
use crate::Result;

pub(crate) trait Callable<'a> {
    fn name(&self) -> &str;

    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>>;

    fn arity(&self) -> usize;
}

#[derive(Clone, Debug)]
pub(crate) enum Callables<'a> {
    Fn(Rc<Function<'a>>),
    Builtin(Rc<Builtins>),
}

impl<'a> Callable<'a> for Callables<'a> {
    fn name(&self) -> &str {
        match self {
            Callables::Fn(f) => f.name(),
            Callables::Builtin(b) => b.name(),
        }
    }

    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>> {
        match self {
            Callables::Fn(f) => f.call(context, args, span),
            Callables::Builtin(b) => b.call(context, args, span),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Callables::Fn(f) => f.arity(),
            Callables::Builtin(b) => b.arity(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
    name: &'a str,
    params: Rc<[Node<&'a str>]>,
    body: Rc<[StmtNode<'a>]>,
}

impl<'a> Function<'a> {
    pub(crate) fn new(declaration: &FunctionDeclaration<'a>) -> Self {
        Self {
            name: declaration.name.item,
            params: Rc::clone(&declaration.params),
            body: Rc::clone(&declaration.body),
        }
    }
}

impl<'a> Callable<'a> for Function<'a> {
    fn name(&self) -> &str {
        self.name
    }

    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        _span: Span,
    ) -> Result<Value<'a>> {
        // each function call needs it's own environment to support recursion
        let mut fun_environment = Environment::with_enclosing(context.globals.clone());

        // register param -> arg mappings in the local environment
        self.params
            .iter()
            .zip(args.iter())
            .for_each(|(param, arg)| {
                fun_environment.define(param.item, arg.clone());
            });

        match InterpreterOps::eval_stmts(context, &self.body, fun_environment.into()) {
            Ok(()) => Ok(Value::Nil),
            Err(e) => match e {
                InterpreterError::Return(value) => Ok(value),
                InterpreterError::Err(e) => Err(e),
            },
        }
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}

impl<'a> Display for Function<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Builtins {
    Clock(Clock),
}

impl<'a> Callable<'a> for Builtins {
    fn name(&self) -> &str {
        match self {
            Builtins::Clock(c) => c.name(),
        }
    }

    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>> {
        match self {
            Builtins::Clock(c) => c.call(context, args, span),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Builtins::Clock(c) => c.arity(),
        }
    }
}

impl Display for Builtins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Builtins::Clock(c) => c.name(),
        };
        write!(f, "<native fn {name}>")
    }
}

// Built-in function (the only one really)
#[derive(Copy, Clone, PartialEq)]
pub struct Clock;

impl<'a> Callable<'a> for Clock {
    fn name(&self) -> &str {
        "clock"
    }

    fn call(
        &self,
        _: &mut InterpreterContext<'a>,
        _args: &[Value<'a>],
        _span: Span,
    ) -> Result<Value<'a>> {
        Ok(SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_or(0.0, |d| d.as_secs_f64())
            .into())
    }

    fn arity(&self) -> usize {
        0
    }
}

impl Debug for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
