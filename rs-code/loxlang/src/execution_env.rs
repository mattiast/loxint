use crate::eval_expr::Value;
use crate::syntax::VarName;
use std::collections::HashMap;

pub struct EvalEnv<'a> {
    values: HashMap<VarName<'a>, Value>,
}

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
    LocalEnv(EvalEnv<'src>, &'scope Stack<'src, 'scope>),
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
    pub fn create_local_env(&'scope self) -> Self {
        Self::LocalEnv(EvalEnv::new(), self)
    }
    // pub fn lookup(&self, name: &VarName<'src>) -> Option<Value> {
    //     for env in self.stack.iter().rev() {
    //         if let Some(x) = env.lookup(name) {
    //             return Some(x);
    //         }
    //     }
    //     None
    // }
    // pub fn assign(&mut self, name: VarName<'src>, value: Value) -> Result<(), ()> {
    //     for env in self.stack.iter_mut().rev() {
    //         if env.is_defined(&name) {
    //             env.set(name, value);
    //             return Ok(());
    //         }
    //     }
    //     Err(())
    // }
    // pub fn declare(&mut self, name: VarName<'src>, value: Value) {
    //     // TODO this unwrap could be avoided if Stack was guaranteed to have at least one environment
    //     self.stack.last_mut().unwrap().set(name, value);
    // }
}
