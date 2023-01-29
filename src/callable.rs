use std::fmt::{Debug, Display};
use std::time::SystemTime;

use crate::env::Environment;
use crate::interp::{InterpreterContext, InterpreterOps};
use crate::node::Node;
use crate::stmt::{FunctionDeclaration, StmtNode};
use crate::token::Span;
use crate::value::Value;
use crate::Result;

pub(crate) trait Callable<'a> {
    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>>;

    fn arity(&self) -> usize;
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    name: &'a str,
    params: Vec<Node<&'a str>>,
    body: Vec<StmtNode<'a>>,
}

impl<'a> Function<'a> {
    pub(crate) fn new(declaration: &FunctionDeclaration<'a>) -> Self {
        Self {
            name: declaration.name.item,
            // TODO: make params and body Rc<>
            params: declaration.params.clone(),
            body: declaration.body.clone(),
        }
    }
}

impl<'a> Callable<'a> for Function<'a> {
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

        InterpreterOps::eval_stmts(context, &self.body, fun_environment.into())?;

        Ok(Value::Nil)
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

// Built-in function (the only one really)
#[derive(Copy, Clone)]
pub struct Clock;

impl<'a> Callable<'a> for Clock {
    fn call(
        &self,
        _: &mut InterpreterContext<'a>,
        _args: &[Value<'a>],
        _span: Span,
    ) -> Result<Value<'a>> {
        Ok(SystemTime::now()
            .elapsed()
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
