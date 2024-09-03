use crate::execution_env::{NotFound, Stack, Value};

use crate::syntax::{BOperator, Declaration, Expression, Statement, UOperator};

type EvalError = ();

// TODO add a type/trait for "execution environment", which has stack operations, and operations for print/clock
// Creating a new scope could be done in RAII fashion, and when the "scope goes out of scope", it will pop the environment

pub fn eval<'src, 'scope>(
    e: &Expression<'src>,
    stack: &mut Stack<'src, 'scope>,
) -> Result<Value, EvalError> {
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

pub fn run_statement<'a, 'src, 'scope>(
    s: &Declaration<'src>,
    stack: &'scope mut Stack<'src, 'scope>,
) -> Result<(), EvalError>
where
    'a: 'scope,
{
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
            stack.run_in_local_env(|stack| {
                for d in decls {
                    // TODO error handling?
                    run_statement(d, stack);
                }
            });
            Ok(())
        }
    }
}
