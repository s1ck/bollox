use std::collections::HashMap;

use crate::eval::Value;

pub(crate) struct Environment<'a> {
    values: HashMap<&'a str, Value>,
}

impl<'a> Environment<'a> {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(crate) fn define(&mut self, name: &'a str, value: Value) {
        let _ = self.values.insert(name, value);
    }

    pub(crate) fn get(&self, name: &'a str) -> Option<&Value> {
        self.values.get(name)
    }

    pub(crate) fn assign(&mut self, name: &'a str, value: Value) -> Option<Value> {
        self.values.insert(name, value)
    }
}
