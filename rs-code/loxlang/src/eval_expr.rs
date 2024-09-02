use std::collections::HashMap;

use crate::syntax::{BOperator, Declaration, Expression, Statement, UOperator, VarName};

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
}

type EvalError = ();

pub struct EvalEnv<'a> {
    values: HashMap<VarName<'a>, Value>,
}
pub struct NotFound;

impl<'a> EvalEnv<'a> {
    pub fn new_global_env() -> Self {
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

pub struct Stack<'a> {
    // TODO ideally we don't want to allow expression evaluation to pop and push into the stack
    // But only to modify the environments in the stack
    stack: Vec<EvalEnv<'a>>,
}
impl<'a> Stack<'a> {
    pub fn new() -> Self {
        Self {
            stack: vec![EvalEnv::new_global_env()],
        }
    }
    pub fn lookup(&self, name: &VarName) -> Option<Value> {
        for env in self.stack.iter().rev() {
            if let Some(x) = env.lookup(name) {
                return Some(x);
            }
        }
        None
    }
    pub fn assign(&mut self, name: VarName<'a>, value: Value) -> Result<(), NotFound> {
        for env in self.stack.iter_mut().rev() {
            if env.is_defined(&name) {
                env.set(name, value);
                return Ok(());
            }
        }
        Err(NotFound)
    }
    pub fn declare(&mut self, name: VarName<'a>, value: Value) {
        // TODO this unwrap could be avoided if Stack was guaranteed to have at least one environment
        self.stack.last_mut().unwrap().set(name, value);
    }
}
// TODO add a type/trait for "execution environment", which has stack operations, and operations for print/clock
// Creating a new scope could be done in RAII fashion, and when the "scope goes out of scope", it will pop the environment

pub fn eval<'a>(e: &Expression<'a>, stack: &mut Stack<'a>) -> Result<Value, EvalError> {
    match e {
        Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
        Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
        Expression::StringLiteral(s) => Ok(Value::String(s.to_string())),
        Expression::Nil => Ok(Value::Nil),
        Expression::Identifier(var) => match stack.lookup(var) {
            Some(v) => Ok(v),
            None => Err(()),
        },
        Expression::Unary { operator, right } => {
            let right = eval(right, stack)?;
            match (operator, right) {
                (UOperator::MINUS, Value::Number(n)) => Ok(Value::Number(-n)),
                (UOperator::BANG, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                (UOperator::BANG, Value::Nil) => Ok(Value::Boolean(true)),
                // TODO Truthiness?
                _ => Err(()),
            }
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(left, stack)?;
            let right = eval(right, stack)?;
            match (left, operator, right) {
                (Value::Number(l), BOperator::PLUS, Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), BOperator::PLUS, Value::String(r)) => Ok(Value::String(l + &r)),
                (Value::Number(l), BOperator::MINUS, Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(l), BOperator::STAR, Value::Number(r)) => Ok(Value::Number(l * r)),
                (Value::Number(l), BOperator::SLASH, Value::Number(r)) => Ok(Value::Number(l / r)), // div by zero?
                (Value::Number(l), BOperator::LESS, Value::Number(r)) => Ok(Value::Boolean(l < r)),
                (Value::Number(l), BOperator::LessEqual, Value::Number(r)) => {
                    Ok(Value::Boolean(l <= r))
                }
                (Value::Number(l), BOperator::GREATER, Value::Number(r)) => {
                    Ok(Value::Boolean(l > r))
                }
                (Value::Number(l), BOperator::GreaterEqual, Value::Number(r)) => {
                    Ok(Value::Boolean(l >= r))
                }
                (l, BOperator::EqualEqual, r) => Ok(Value::Boolean(l == r)),
                (l, BOperator::BangEqual, r) => Ok(Value::Boolean(l != r)),
                _ => Err(()),
            }
        }
        Expression::Assignment(name, value) => {
            let val = eval(value, stack)?;
            match stack.assign(name.clone(), val.clone()) {
                Ok(()) => Ok(val),
                Err(NotFound) => Err(()),
            }
        }
    }
}

pub fn run_statement<'a>(s: &Declaration<'a>, stack: &mut Stack<'a>) -> Result<(), EvalError> {
    match s {
        Declaration::Statement(Statement::Expression(e)) => {
            let _ = eval(e, stack)?;
            Ok(())
        }
        Declaration::Statement(Statement::Print(e)) => {
            let v = eval(e, stack)?;
            // TODO print should use dependency injection from "execution environment"
            println!("{:?}", v);
            Ok(())
        }
        Declaration::Var(s, e) => {
            let v = eval(e.as_ref().unwrap_or(&Expression::Nil), stack)?;
            stack.declare(s.clone(), v);
            Ok(())
        }
        Declaration::Statement(Statement::Block(decls)) => {
            stack.stack.push(EvalEnv::new_global_env()); // TODO lol rename
            for d in decls {
                run_statement(d, stack)?;
            }
            stack.stack.pop();
            Ok(())
        }
    }
}
