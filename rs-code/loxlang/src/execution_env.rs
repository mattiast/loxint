use crate::syntax::VarName;
use std::collections::HashMap;

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

pub enum Stack<'src, 'scope> {
    GlobalEnv(EvalEnv<'src>),
    LocalEnv(EvalEnv<'src>, &'scope mut Stack<'src, 'scope>),
}
impl<'src, 'scope> Stack<'src, 'scope> {
    pub fn new() -> Self {
        Self::GlobalEnv(EvalEnv::new())
    }
    pub fn get_env<'a>(&'a self) -> &'a EvalEnv<'src> {
        match self {
            Self::GlobalEnv(e) => e,
            Self::LocalEnv(e, _) => e,
        }
    }
    pub fn get_mut_env<'a>(&'a mut self) -> &'a mut EvalEnv<'src> {
        match self {
            Self::GlobalEnv(e) => e,
            Self::LocalEnv(e, _) => e,
        }
    }
    pub fn create_local_env<'a>(&'a mut self) -> Stack<'src, 'a>
    where
        'scope: 'a,
    {
        Self::LocalEnv(EvalEnv::new(), self)
    }
    pub fn increase_scope<'a>(&'a mut self) -> &'a mut Stack<'src, 'a>
    where
        //'scope: 'a,
        'a: 'scope,
    {
        self
    }
    // pub fn reduce_scope<'a, 'b>(&'b mut self) -> &'b mut Stack<'src, 'a>
    // where
    //     'scope: 'a,
    //     // 'a: 'scope,
    // {
    //     self
    // }
    pub fn lookup(&self, name: &VarName<'src>) -> Option<Value> {
        if let Some(x) = self.get_env().lookup(name) {
            Some(x)
        } else if let Self::LocalEnv(_, parent) = self {
            parent.lookup(name)
        } else {
            None
        }
    }
    pub fn assign(&mut self, name: VarName<'src>, value: Value) -> Result<(), NotFound> {
        if self.get_env().is_defined(&name) {
            self.get_mut_env().set(name, value);
            Ok(())
        } else if let Self::LocalEnv(_, parent) = self {
            parent.assign(name, value)
        } else {
            Err(NotFound)
        }
    }
    pub fn declare(&mut self, name: VarName<'src>, value: Value) {
        self.get_mut_env().set(name, value);
    }
}
