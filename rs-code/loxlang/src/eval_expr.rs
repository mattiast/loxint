use crate::execution_env::{Deps, ExecEnv, NotFound, Stack, Value};

use crate::syntax::{BOperator, Declaration, Expression, Statement, UOperator};

type EvalError = ();

// TODO Creating a new scope could be done in RAII fashion, and when the "scope goes out of scope", it will pop the environment

pub fn eval<'src, 'scope>(
    e: &Expression<'src>,
    stack: &mut Stack<'src>,
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
            match (left, operator) {
                // First check some cases where we may shor-circuit and not evaluate the right side
                (Value::Boolean(l), BOperator::AND) => {
                    if !l {
                        Ok(Value::Boolean(false))
                    } else {
                        eval(right, stack)
                    }
                }
                (Value::Boolean(l), BOperator::OR) => {
                    if l {
                        Ok(Value::Boolean(true))
                    } else {
                        eval(right, stack)
                    }
                }
                (left, _) => {
                    // In rest of the cases we always need to evaluate both operands
                    let right = eval(right, stack)?;
                    match (left, operator, right) {
                        (Value::Number(l), BOperator::PLUS, Value::Number(r)) => {
                            Ok(Value::Number(l + r))
                        }
                        (Value::String(l), BOperator::PLUS, Value::String(r)) => {
                            Ok(Value::String(l + &r))
                        }
                        (Value::Number(l), BOperator::MINUS, Value::Number(r)) => {
                            Ok(Value::Number(l - r))
                        }
                        (Value::Number(l), BOperator::STAR, Value::Number(r)) => {
                            Ok(Value::Number(l * r))
                        }
                        (Value::Number(l), BOperator::SLASH, Value::Number(r)) => {
                            Ok(Value::Number(l / r))
                        } // div by zero?
                        (Value::Number(l), BOperator::LESS, Value::Number(r)) => {
                            Ok(Value::Boolean(l < r))
                        }
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

pub fn run_statement<'src, Dep: Deps>(
    s: &Statement<'src>,
    env: &mut ExecEnv<'src, Dep>,
) -> Result<(), EvalError> {
    match s {
        Statement::Expression(e) => {
            let _ = eval(e, env.get_stack_mut())?;
            Ok(())
        }
        Statement::Print(e) => {
            let v = eval(e, env.get_stack_mut())?;
            env.print(v);
            Ok(())
        }
        Statement::Block(decls) => env.run_in_substack(|env| {
            for d in decls {
                run_declaration(d, env)?;
            }
            Ok(())
        }),
        Statement::If(cond, stmt_then, stmt_else) => {
            let cond_val = eval(cond, env.get_stack_mut())?;
            match cond_val {
                Value::Boolean(true) => run_statement(stmt_then, env),
                Value::Boolean(false) => {
                    if let Some(stmt_else) = stmt_else {
                        run_statement(stmt_else, env)
                    } else {
                        Ok(())
                    }
                }
                _ => Err(()),
            }
        }
    }
}

pub fn run_declaration<'src, Dep: Deps>(
    s: &Declaration<'src>,
    env: &mut ExecEnv<'src, Dep>,
) -> Result<(), EvalError> {
    match s {
        Declaration::Var(s, e) => {
            let v = eval(e.as_ref().unwrap_or(&Expression::Nil), env.get_stack_mut())?;
            env.get_stack_mut().declare(s.clone(), v);
            Ok(())
        }
        Declaration::Statement(stmt) => run_statement(stmt, env),
    }
}

#[cfg(test)]
mod tests {
    use crate::execution_env::Deps;
    use crate::execution_env::ExecEnv;
    use crate::parser;
    use crate::scanner::parse_tokens;

    use super::*;
    struct TestDeps {
        printed: Vec<Value>,
    }
    impl Deps for TestDeps {
        fn print(&mut self, v: Value) {
            self.printed.push(v);
        }
        fn clock(&mut self) -> f64 {
            0.0
        }
    }

    #[test]
    fn run_program() {
        let deps = TestDeps {
            printed: Vec::new(),
        };
        let mut env = ExecEnv::new(deps);
        // Define program as a multiline string, and parse it
        let source = r#"
            var a = "hi";
            var b = 3;
            7;
            {
            print a;
            print b+1;
            }
        "#;
        let (rest, tokens) = parse_tokens(source).unwrap();
        assert_eq!(rest, "");
        let mut parser = parser::Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        for stmt in program.decls {
            run_declaration(&stmt, &mut env).unwrap();
        }
        let deps = env.into_deps();
        assert_eq!(
            deps.printed,
            vec![Value::String("hi".to_string()), Value::Number(4.0)]
        );
    }
}
