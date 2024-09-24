use crate::syntax::{Statement, VResolution, VarId, VariableDecl};
use std::{
    collections::HashMap,
    fmt::Debug,
    sync::{Arc, Mutex},
};

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
    pub arguments: Vec<VariableDecl<VarId>>,
    pub body: Statement<'src, VResolution, VarId>,
    pub env: Stack<'src>,
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

struct StackFrame<'a> {
    values: HashMap<VarId, Value<'a>>,
}
pub struct NotFound;

impl<'a> StackFrame<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
    pub fn lookup(&self, name: &VarId) -> Option<Value<'a>> {
        self.values.get(name).cloned()
    }
    pub fn set(&mut self, name: VarId, value: Value<'a>) {
        self.values.insert(name, value);
    }
}

/// A stack of mutable environments
/// The environments have interior mutability
#[derive(Clone)]
pub struct Stack<'src> {
    head: StackRef<'src>,
}
type StackRef<'src> = Arc<Node<'src>>;
struct Node<'src> {
    env: Mutex<StackFrame<'src>>,
    parent: Option<StackRef<'src>>,
}
impl<'src> Stack<'src> {
    pub fn new() -> Self {
        let mut global_env = StackFrame::new();
        // TODO How to handle builtin functions?
        global_env.set(1, Value::NativeFunction(NativeFunc::Clock));

        let node = Node {
            env: Mutex::new(global_env),
            parent: None,
        };
        Self {
            head: Arc::new(node),
        }
    }
    fn get_env<'a>(&'a self) -> &'a Mutex<StackFrame<'src>> {
        &self.head.env
    }
    fn go_to_local_env(&mut self) {
        // Can we get rid of clone?
        // The old head is copied but also thrown away
        self.head = Arc::new(Node {
            env: Mutex::new(StackFrame::new()),
            parent: Some(self.head.clone()),
        });
    }
    fn go_to_parent_env(&mut self) {
        // No huh huh
        // TODO this unwrap doesn't need to be here
        self.head = self.head.as_ref().parent.clone().unwrap();
    }
    pub fn lookup(&self, name: VResolution) -> Option<Value<'src>> {
        let mut node = self.head.as_ref();
        let (var_id, depth) = name;
        for _ in 0..depth {
            node = node.parent.as_ref()?;
        }
        node.env.lock().unwrap().lookup(&var_id)
    }
    pub fn assign(&self, name: VResolution, value: Value<'src>) -> Result<(), NotFound> {
        let mut node = self.head.as_ref();
        let (var_id, depth) = name;
        for _ in 0..depth {
            node = node.parent.as_ref().ok_or(NotFound)?;
        }
        let mut g = node.env.lock().unwrap();
        g.set(var_id, value);
        Ok(())
    }
    pub fn declare(&mut self, var_id: VarId, value: Value<'src>) {
        self.get_env().lock().unwrap().set(var_id, value);
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
    pub stack: Stack<'src>,
    deps: Dep,
}

impl<'src> ExecEnv<'src, DefaultDeps> {
    pub fn new_default() -> Self {
        Self::new(DefaultDeps)
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
    pub fn print(&mut self, value: Value) {
        self.deps.print(value);
    }
    pub fn clock(&mut self) -> f64 {
        self.deps.clock()
    }
    pub fn lookup(&self, name: VResolution) -> Option<Value<'src>> {
        self.stack.lookup(name)
    }
    pub fn assign(&mut self, name: VResolution, value: Value<'src>) -> Result<(), NotFound> {
        self.stack.assign(name, value)
    }
    pub fn declare(&mut self, name: VarId, value: Value<'src>) {
        self.stack.declare(name, value)
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
