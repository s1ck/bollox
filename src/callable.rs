use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::env::{Environment, EnvironmentRef};
use crate::error::RuntimeError;
use crate::interp::{InterpreterContext, InterpreterError, InterpreterOps};
use crate::node::Node;
use crate::stmt::{FunctionDeclaration, StmtNode};
use crate::token::Span;
use crate::value::Value;
use crate::Result;

#[derive(Clone, Debug)]
pub(crate) enum Callables<'a> {
    Fn(Rc<Function<'a>>),
    Builtin(Rc<Builtins>),
    Clazz(&'a Class<'a>),
}

impl<'a> Callables<'a> {
    pub(crate) fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>> {
        match self {
            Callables::Fn(f) => f.call(context, args, span),
            Callables::Builtin(b) => b.call(context, args, span),
            Callables::Clazz(c) => c.call(context, args, span),
        }
    }

    pub(crate) fn arity(&self) -> usize {
        match self {
            Callables::Fn(f) => f.arity(),
            Callables::Builtin(b) => b.arity(),
            Callables::Clazz(c) => c.arity(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
    name: &'a str,
    params: Rc<[Node<&'a str>]>,
    body: Rc<[StmtNode<'a>]>,
    closure: EnvironmentRef<'a>,
    is_init: bool,
}

impl<'a> Function<'a> {
    pub(crate) fn new(
        declaration: &FunctionDeclaration<'a>,
        closure: EnvironmentRef<'a>,
        is_init: bool,
    ) -> Self {
        Self {
            name: declaration.name.item,
            params: Rc::clone(&declaration.params),
            body: Rc::clone(&declaration.body),
            closure,
            is_init,
        }
    }

    pub(crate) fn bind(&self, instance: Rc<Instance<'a>>) -> Self {
        let environment = self.closure.clone();
        environment
            .borrow_mut()
            .define("this", Value::Instance(instance));
        Self {
            name: self.name,
            params: Rc::clone(&self.params),
            body: Rc::clone(&self.body),
            closure: environment,
            is_init: self.is_init,
        }
    }
}

impl<'a> Function<'a> {
    fn call(
        &self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>> {
        // If the function is an initializer, we override the actual return
        // value and foribly return `this`.
        if self.is_init {
            return self
                .closure
                .borrow()
                .get_at("this", 0)
                .ok_or_else(|| RuntimeError::internal("`this` not found in environment", span));
        }
        // Each function call needs it's own environment to support recursion.
        // The enclosing environment is the one that is active when the method
        // is declared. This also gives access to globals in order to call funcs.
        let mut fun_environment = Environment::with_enclosing(self.closure.clone());

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
                InterpreterError::Return(value) => {
                    if self.is_init {
                        self.closure.borrow().get_at("this", 0).ok_or_else(|| {
                            RuntimeError::internal("`this` not found in environment", span)
                        })
                    } else {
                        Ok(value)
                    }
                }
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
pub struct Class<'a> {
    name: &'a str,
    methods: HashMap<&'a str, Function<'a>>,
    superclass: Option<&'a Class<'a>>,
    closure: EnvironmentRef<'a>,
}

impl<'a> Class<'a> {
    pub(crate) fn new(
        name: &'a str,
        methods: HashMap<&'a str, Function<'a>>,
        superclass: Option<&'a Class<'a>>,
        closure: EnvironmentRef<'a>,
    ) -> Self {
        Self {
            name,
            methods,
            superclass,
            closure,
        }
    }

    pub(crate) fn get_method(&self, name: &'a str) -> Option<Function<'a>> {
        self.methods.get(name).cloned()
    }
}

impl<'a> Class<'a> {
    fn call(
        &'a self,
        context: &mut InterpreterContext<'a>,
        args: &[Value<'a>],
        span: Span,
    ) -> Result<Value<'a>> {
        let instance = Rc::new(Instance::new(self));
        if let Some(initializer) = self.get_method("init") {
            let _ = initializer
                .bind(instance.clone())
                .call(context, args, span)?;
        }
        Ok(instance.into())
    }

    fn arity(&self) -> usize {
        match self.get_method("init") {
            Some(method) => method.arity(),
            None => 0,
        }
    }
}

impl<'a> Display for Class<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<clazz {}>", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance<'a> {
    clazz: &'a Class<'a>,
    fields: RefCell<HashMap<&'a str, Value<'a>>>,
}

impl<'a> Instance<'a> {
    fn new(clazz: &'a Class<'a>) -> Self {
        Self {
            clazz,
            fields: RefCell::new(HashMap::new()),
        }
    }

    pub(crate) fn get(&self, name: &'a str) -> Option<Value<'a>> {
        match self.fields.borrow().get(name) {
            Some(field) => Some(field.clone()),
            None => self
                .clazz
                .get_method(name)
                .map(|method| method.bind(Rc::new(self.clone())).into()),
        }
    }

    pub(crate) fn set(&self, name: &'a str, value: Value<'a>) {
        self.fields.borrow_mut().insert(name, value);
    }
}

impl<'a> Display for Instance<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance {}>", self.clazz)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Builtins {
    Clock(Clock),
}

impl<'a> Builtins {
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

impl<'a> Clock {
    pub(crate) fn name(&self) -> &str {
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
