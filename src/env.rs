use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::eval::Value;

pub(crate) struct Environment<'a> {
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
    values: HashMap<&'a str, Value>,
}

impl<'a> Environment<'a> {
    pub(crate) fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub(crate) fn with_enclosing(enclosing: Rc<RefCell<Environment<'a>>>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub(crate) fn define(&mut self, name: &'a str, value: Value) {
        let _ = self.values.insert(name, value);
    }

    pub(crate) fn get(&self, name: &'a str) -> Option<Value> {
        if self.values.contains_key(name) {
            return self.values.get(name).cloned();
        }

        match &self.enclosing {
            Some(enc) => enc.borrow().get(name),
            None => None,
        }
    }

    pub(crate) fn assign(&mut self, name: &'a str, value: Value) -> Option<Value> {
        if self.values.contains_key(name) {
            return self.values.insert(name, value);
        }

        match &self.enclosing {
            Some(enc) => enc.borrow_mut().assign(name, value),
            None => None,
        }
    }
}
