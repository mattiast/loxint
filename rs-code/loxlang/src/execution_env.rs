use crate::syntax::{Statement, VarName};
use std::{collections::HashMap, fmt::Debug, mem, sync::Arc};

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'src> {
    // TODO: an own type for "atomic types", i.e. number, boolean, string, nil
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
    // Callable(Box<dyn Fn(&mut EvalEnv) -> Result<Value, Error>>),
    // Must also take a list of arguments
    // And have an "arity" u8
    // Native functions are a separate type, they are a function item
    NativeFunction(NativeFunc),
    Function(LoxFunction<'src>),
}
#[derive(Clone)]
pub struct LoxFunction<'src> {
    pub arguments: Vec<VarName<'src>>,
    pub body: Statement<'src>,
    pub env: Arc<Stack<'src>>,
}
impl Debug for LoxFunction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxFunction")
            .field("arguments", &self.arguments)
            .field("body", &self.body)
            .finish()
    }
}
impl PartialEq for LoxFunction<'_> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
#[derive(PartialEq, Debug, Clone)]
pub enum NativeFunc {
    Clock,
}

pub struct EvalEnv<'a> {
    values: HashMap<VarName<'a>, Value<'a>>,
}
pub struct NotFound;

impl<'a> EvalEnv<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
    pub fn lookup(&self, name: &VarName<'a>) -> Option<Value<'a>> {
        self.values.get(name).cloned()
    }
    pub fn is_defined(&self, name: &VarName<'a>) -> bool {
        self.values.contains_key(name)
    }
    pub fn set(&mut self, name: VarName<'a>, value: Value<'a>) {
        self.values.insert(name, value);
    }
}

pub struct Stack<'src> {
    env: EvalEnv<'src>,
    parent: Option<Box<Stack<'src>>>,
}
impl<'src> Stack<'src> {
    pub fn new() -> Self {
        let mut global_env = EvalEnv::new();
        global_env.set(VarName("clock"), Value::NativeFunction(NativeFunc::Clock));

        Self {
            env: global_env,
            parent: None,
        }
    }
    pub fn get_env<'a>(&'a self) -> &'a EvalEnv<'src> {
        &self.env
    }
    pub fn get_mut_env<'a>(&'a mut self) -> &'a mut EvalEnv<'src> {
        &mut self.env
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
    pub fn lookup(&self, name: &VarName<'src>) -> Option<Value<'src>> {
        if let Some(x) = self.get_env().lookup(name) {
            Some(x)
        } else if let Some(parent) = self.parent.as_ref() {
            parent.lookup(name)
        } else {
            None
        }
    }
    pub fn assign(&mut self, name: VarName<'src>, value: Value<'src>) -> Result<(), NotFound> {
        if self.get_env().is_defined(&name) {
            self.get_mut_env().set(name, value);
            Ok(())
        } else if let Some(parent) = self.parent.as_mut() {
            parent.assign(name, value)
        } else {
            Err(NotFound)
        }
    }
    pub fn declare(&mut self, name: VarName<'src>, value: Value<'src>) {
        self.get_mut_env().set(name, value);
    }
}

/// Runtime dependencies for lox programs
pub trait Deps {
    fn print<'src>(&mut self, value: Value<'src>);
    fn clock(&mut self) -> f64;
}

pub struct DefaultDeps;
impl Deps for DefaultDeps {
    fn print(&mut self, value: Value) {
        println!("{:?}", value);
    }
    fn clock(&mut self) -> f64 {
        use std::time::{SystemTime, UNIX_EPOCH};

        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs_f64()
    }
}

pub struct ExecEnv<'src, Dep: Deps> {
    stack: Stack<'src>,
    deps: Dep,
}

impl<'src> ExecEnv<'src, DefaultDeps> {
    pub fn new_default() -> Self {
        Self {
            stack: Stack::new(),
            deps: DefaultDeps,
        }
    }
}
impl<'src, Dep: Deps> ExecEnv<'src, Dep> {
    pub fn new(deps: Dep) -> Self {
        Self {
            stack: Stack::new(),
            deps,
        }
    }
    pub fn into_deps(self) -> Dep {
        self.deps
    }
    pub fn get_stack(&self) -> &Stack<'src> {
        &self.stack
    }
    pub fn get_stack_mut(&mut self) -> &mut Stack<'src> {
        &mut self.stack
    }
    pub fn print(&mut self, value: Value) {
        self.deps.print(value);
    }
    pub fn clock(&mut self) -> f64 {
        self.deps.clock()
    }
    pub fn run_in_substack<F, U>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut ExecEnv<'src, Dep>) -> U,
    {
        // TODO this is screaming for RAII
        self.stack.go_to_local_env();
        let v = f(self);
        self.stack.go_to_parent_env();
        v
    }
}
