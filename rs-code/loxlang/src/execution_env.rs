use crate::syntax::VarName;
use std::{collections::HashMap, mem};

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
}

pub struct EvalEnv<'a> {
    values: HashMap<VarName<'a>, Value>,
}
pub struct NotFound;

impl<'a> EvalEnv<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
    pub fn lookup(&self, name: &VarName) -> Option<Value> {
        self.values.get(name).cloned()
    }
    pub fn is_defined(&self, name: &VarName<'a>) -> bool {
        self.values.contains_key(name)
    }
    pub fn set(&mut self, name: VarName<'a>, value: Value) {
        self.values.insert(name, value);
    }
}

pub struct Stack<'src> {
    env: EvalEnv<'src>,
    parent: Option<Box<Stack<'src>>>,
}
impl<'src> Stack<'src> {
    pub fn new() -> Self {
        Self {
            env: EvalEnv::new(),
            parent: None,
        }
    }
    pub fn get_env<'a>(&'a self) -> &'a EvalEnv<'src> {
        &self.env
    }
    pub fn get_mut_env<'a>(&'a mut self) -> &'a mut EvalEnv<'src> {
        &mut self.env
    }
    pub fn run_in_local_env<F, U>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut Stack<'src>) -> U,
    {
        // TODO this is screaming for RAII
        self.go_to_local_env();
        let v = f(self);
        self.go_to_parent_env();
        v
    }
    fn go_to_local_env(&mut self) {
        // No huh huh
        // TODO make this less awful
        let old = mem::replace(self, Self::new());
        let new = Self {
            env: EvalEnv::new(),
            parent: Some(Box::new(old)),
        };
        *self = new;
    }
    fn go_to_parent_env(&mut self) {
        // No huh huh
        *self = *self.parent.take().unwrap();
    }
    pub fn lookup(&self, name: &VarName<'src>) -> Option<Value> {
        if let Some(x) = self.get_env().lookup(name) {
            Some(x)
        } else if let Some(parent) = self.parent.as_ref() {
            parent.lookup(name)
        } else {
            None
        }
    }
    pub fn assign(&mut self, name: VarName<'src>, value: Value) -> Result<(), NotFound> {
        if self.get_env().is_defined(&name) {
            self.get_mut_env().set(name, value);
            Ok(())
        } else if let Some(parent) = self.parent.as_mut() {
            parent.assign(name, value)
        } else {
            Err(NotFound)
        }
    }
    pub fn declare(&mut self, name: VarName<'src>, value: Value) {
        self.get_mut_env().set(name, value);
    }
}
