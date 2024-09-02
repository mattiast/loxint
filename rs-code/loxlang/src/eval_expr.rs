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

impl EvalEnv<'_> {
    pub fn new_global_env() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}

pub fn eval<'a>(e: &Expression<'a>, env: &mut EvalEnv<'a>) -> Result<Value, EvalError> {
    match e {
        Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
        Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
        Expression::StringLiteral(s) => Ok(Value::String(s.to_string())),
        Expression::Nil => Ok(Value::Nil),
        Expression::Identifier(VarName(s)) => match env.values.get(&VarName(s)) {
            Some(v) => Ok(v.clone()),
            None => Err(()),
        },
        Expression::Unary { operator, right } => {
            let right = eval(right, env)?;
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
            let left = eval(left, env)?;
            let right = eval(right, env)?;
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
            let val = eval(value, env)?;
            if let Some(value_ref) = env.values.get_mut(name) {
                // TODO Can we do something to avoid clone?
                *value_ref = val.clone();
                Ok(val)
            } else {
                Err(()) // Undefined variable
            }
        }
    }
}

pub fn run_statement<'a>(s: &Declaration<'a>, env: &mut EvalEnv<'a>) -> Result<(), EvalError> {
    match s {
        Declaration::Statement(Statement::Expression(e)) => {
            let _ = eval(e, env)?;
            Ok(())
        }
        Declaration::Statement(Statement::Print(e)) => {
            let v = eval(e, env)?;
            // TODO print should use dependency injection from "execution environment"
            println!("{:?}", v);
            Ok(())
        }
        Declaration::Var(s, e) => {
            let v = eval(e.as_ref().unwrap_or(&Expression::Nil), env)?;
            env.values.insert(s.clone(), v);
            Ok(())
        }
    }
}
