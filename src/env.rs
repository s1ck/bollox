use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    callable::{Builtins, Clock},
    value::Value,
};

pub(crate) type EnvironmentRef<'a> = Rc<RefCell<Environment<'a>>>;

#[derive(Debug, PartialEq)]
pub(crate) struct Environment<'a> {
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
    values: HashMap<&'a str, Value<'a>>,
}

impl<'a> Environment<'a> {
    pub(crate) fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub(crate) fn with_enclosing(enclosing: EnvironmentRef<'a>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub(crate) fn define(&mut self, name: &'a str, value: Value<'a>) {
        let _ = self.values.insert(name, value);
    }

    pub(crate) fn get(&self, name: &'a str) -> Option<Value<'a>> {
        if self.values.contains_key(name) {
            return self.values.get(name).cloned();
        }

        match &self.enclosing {
            Some(enc) => enc.borrow().get(name),
            None => None,
        }
    }

    pub(crate) fn get_global(&self, name: &'a str) -> Option<Value<'a>> {
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow().get_global(name),
            None => self.get(name),
        }
    }

    pub(crate) fn get_at(&self, name: &'a str, distance: usize) -> Option<Value<'a>> {
        match distance {
            0 => self.get(name),
            d => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get_at(name, d - 1),
                None => None,
            },
        }
    }

    pub(crate) fn assign(&mut self, name: &'a str, value: Value<'a>) -> Option<Value<'a>> {
        if self.values.contains_key(name) {
            return self.values.insert(name, value);
        }

        match &self.enclosing {
            Some(enc) => enc.borrow_mut().assign(name, value),
            None => None,
        }
    }

    pub(crate) fn assign_global(&mut self, name: &'a str, value: Value<'a>) -> Option<Value<'a>> {
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow_mut().assign_global(name, value),
            None => self.assign(name, value),
        }
    }

    pub(crate) fn assign_at(
        &mut self,
        name: &'a str,
        value: Value<'a>,
        distance: usize,
    ) -> Option<Value<'a>> {
        match distance {
            0 => self.assign(name, value),
            d => match &self.enclosing {
                Some(enclosing) => enclosing.borrow_mut().assign_at(name, value, d - 1),
                None => None,
            },
        }
    }
}

impl<'a> From<Environment<'a>> for EnvironmentRef<'a> {
    fn from(env: Environment<'a>) -> Self {
        Rc::new(RefCell::new(env))
    }
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        let mut globals = Environment::new();
        globals.define(
            Clock.name(),
            Value::Builtin(Rc::new(Builtins::Clock(Clock))),
        );
        globals
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let mut env = Environment::new();

        assert_eq!(env.get("foo"), None);
        env.define("foo", Value::Number(42.0));
        assert_eq!(env.get("foo"), Some(Value::Number(42.0)));
        env.define("foo", Value::Nil);
        assert_eq!(env.get("foo"), Some(Value::Nil));
    }

    #[test]
    fn test_assign() {
        let mut env = Environment::new();

        assert_eq!(env.assign("foo", Value::Boolean(true)), None);
        env.define("foo", Value::Boolean(false));
        assert_eq!(
            env.assign("foo", Value::Boolean(true)),
            Some(Value::Boolean(false))
        );
    }

    #[test]
    fn test_define_enclosed() {
        let mut outer_env = Environment::new();
        outer_env.define("foo", Value::Boolean(true));

        let inner_env = Environment::with_enclosing(Rc::new(RefCell::new(outer_env)));
        assert_eq!(inner_env.get("foo"), Some(Value::Boolean(true)));
    }

    #[test]
    fn test_define_shadow_enclosed() {
        let mut outer_env = Environment::new();
        outer_env.define("foo", Value::Boolean(true));

        let outer_env = Rc::new(RefCell::new(outer_env));
        let mut inner_env = Environment::with_enclosing(outer_env.clone());

        assert_eq!(inner_env.get("foo"), Some(Value::Boolean(true)));
        inner_env.define("foo", Value::Nil); // shadow "foo"
        assert_eq!(inner_env.get("foo"), Some(Value::Nil));
        assert_eq!(outer_env.borrow().get("foo"), Some(Value::Boolean(true)));
    }
}
