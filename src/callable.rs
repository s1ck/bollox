use std::fmt::Debug;
use std::time::SystemTime;

use crate::env::Environment;
use crate::interp::{InterpreterContext, InterpreterOps};
use crate::stmt::FunctionDeclaration;
use crate::token::Span;
use crate::value::Value;
use crate::Result;

pub(crate) trait Callable<'a> {
    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value],
        span: Span,
    ) -> Result<Value>;

    fn arity(&self) -> usize;
}

pub(crate) struct Function<'a> {
    declaration: FunctionDeclaration<'a>,
}

impl<'a> Callable<'a> for Function<'a> {
    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value],
        _span: Span,
    ) -> Result<Value> {
        // each function call needs it's own environment to support recursion
        let mut fun_environment = Environment::with_enclosing(context.globals.clone());

        // register param -> arg mappings in the local environment
        self.declaration
            .params
            .iter()
            .zip(args.iter())
            .for_each(|(param, arg)| {
                fun_environment.define(param.item, arg.clone());
            });

        InterpreterOps::eval_stmts(context, &self.declaration.body, fun_environment.into())?;

        Ok(Value::Nil)
    }

    fn arity(&self) -> usize {
        todo!()
    }
}

// Built-in function (the only one really)
#[derive(Copy, Clone)]
pub struct Clock;

impl<'a> Callable<'a> for Clock {
    fn call(&self, _: &mut InterpreterContext<'a>, _args: &[Value], _span: Span) -> Result<Value> {
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
