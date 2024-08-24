use crate::syntax::{BOperator, Expression};

enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Nil,
}

fn eval(e: &Expression) -> Result<Value, ()> {
    match e {
        Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
        Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
        Expression::StringLiteral(s) => Ok(Value::String(s.to_string())),
        Expression::Nil => Ok(Value::Nil),
        Expression::Unary { operator, right } => {
            let right = eval(right)?;
            match operator {
                UOperator::MINUS => match right {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    _ => Err(()),
                },
                UOperator::BANG => match right {
                    Value::Boolean(b) => Ok(Value::Boolean(!b)),
                    _ => Err(()),
                },
            }
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(left)?;
            let right = eval(right)?;
            match (left, operator, right) {
                (Value::Number(l), BOperator::PLUS, Value::Number(r)) => Ok(Value::Number(l + r)),
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
            }
        }
    }
}
