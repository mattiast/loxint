use std::mem;

use miette::Diagnostic;
use thiserror::Error;

use crate::execution_env::{Deps, ExecEnv, LoxFunction, NativeFunc, NotFound, Value};

use crate::parser::ByteSpan;
use crate::resolution::{ResolvedDeclaration, ResolvedExpression, ResolvedStatement};
use crate::syntax::{
    BOperator, Declaration, Expression, Statement, UOperator, Variable, VariableDecl,
};

#[derive(Error, Debug, Diagnostic)]
#[error("runtime_error")]
pub struct RuntimeError {
    // #[source_code]
    // pub src: &'static str,
    #[label("Something wrong here")]
    pub source_offset: miette::SourceSpan,
    pub msg: &'static str,
}

fn err(msg: &'static str, span: ByteSpan) -> RuntimeError {
    RuntimeError {
        source_offset: span.into(),
        msg,
    }
}

// TODO Creating a new scope could be done in RAII fashion, and when the "scope goes out of scope", it will pop the environment

pub fn eval<'src, Dep: Deps>(
    e: &ResolvedExpression<'src>,
    stack: &mut ExecEnv<'src, Dep>,
) -> Result<Value<'src>, RuntimeError> {
    let span = e.annotation;
    match &e.value {
        Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
        Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
        Expression::StringLiteral(s) => Ok(Value::String(s.to_string())),
        Expression::Nil => Ok(Value::Nil),
        Expression::Identifier(Variable(var)) => match stack.lookup(*var) {
            Some(v) => Ok(v),
            None => Err(err("Identifier not found", span)), // should not happen after resolution
        },
        Expression::Unary { operator, right } => {
            let right = eval(right.as_ref(), stack)?;
            match (operator, right) {
                (UOperator::MINUS, Value::Number(n)) => Ok(Value::Number(-n)),
                (UOperator::BANG, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                (UOperator::BANG, Value::Nil) => Ok(Value::Boolean(true)),
                // TODO Truthiness?
                _ => Err(err("Unary operator not supported for this type", span)),
            }
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(left.as_ref(), stack)?;
            match (left, operator) {
                // First check some cases where we may shor-circuit and not evaluate the right side
                (Value::Boolean(l), BOperator::AND) => {
                    if !l {
                        Ok(Value::Boolean(false))
                    } else {
                        eval(right.as_ref(), stack)
                    }
                }
                (Value::Boolean(l), BOperator::OR) => {
                    if l {
                        Ok(Value::Boolean(true))
                    } else {
                        eval(right.as_ref(), stack)
                    }
                }
                (left, _) => {
                    // In rest of the cases we always need to evaluate both operands
                    let right = eval(right.as_ref(), stack)?;
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
                        _ => Err(err("Binary not supported for these types", span)),
                    }
                }
            }
        }
        Expression::Assignment(Variable(name), value) => {
            let val = eval(value.as_ref(), stack)?;
            match stack.assign(*name, val.clone()) {
                Ok(()) => Ok(val),
                Err(NotFound) => Err(err(
                    "Variable not found (should not happen after resolution)",
                    span,
                )),
            }
        }
        Expression::FunctionCall(f, args) => {
            let f = eval(f.as_ref(), stack)?;
            let args = args
                .iter()
                .map(|a| eval(a, stack))
                .collect::<Result<Vec<_>, _>>()?;
            match f {
                Value::Function(f) => {
                    if args.len() != f.arguments.len() {
                        return Err(err(
                            "Wrong number of arguments in function application",
                            span,
                        ));
                    }
                    // We need to temporarily replace the stack with the function's environment,
                    // evaluate the function body, and then restore the old stack.
                    // TODO: no huh huh, this should use some sort of RAII or another callback thing
                    let old_stack = mem::replace(&mut stack.stack, f.env.clone());

                    let result = stack.run_in_substack(|stack| {
                        for (VariableDecl(arg_name), arg_value) in
                            f.arguments.iter().zip(args.into_iter())
                        {
                            stack.declare(*arg_name, arg_value);
                        }
                        run_statement(&f.body, stack)?;
                        // TODO return values
                        // Should run_statement take a Continuation as a parameter??
                        Ok(Value::Nil)
                    });
                    stack.stack = old_stack;
                    result
                }
                Value::NativeFunction(NativeFunc::Clock) => {
                    if args.is_empty() {
                        Ok(Value::Number(stack.clock()))
                    } else {
                        Err(err(
                            "Wrong number of arguments in function application",
                            span,
                        ))
                    }
                }
                _ => Err(err("Not a function", span)),
            }
        }
    }
}

pub fn run_statement<'src, Dep: Deps>(
    s: &ResolvedStatement<'src>,
    env: &mut ExecEnv<'src, Dep>,
) -> Result<(), RuntimeError> {
    match s {
        Statement::Expression(e) => {
            let _ = eval(e, env)?;
            Ok(())
        }
        Statement::Print(e) => {
            let v = eval(e, env)?;
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
            let span = cond.annotation;
            let cond_val = eval(cond, env)?;
            match cond_val {
                Value::Boolean(true) => run_statement(stmt_then, env),
                Value::Boolean(false) => {
                    if let Some(stmt_else) = stmt_else {
                        run_statement(stmt_else, env)
                    } else {
                        Ok(())
                    }
                }
                _ => Err(err("Condition value not a boolean", span)),
            }
        }
        Statement::While(cond, body) => loop {
            let span = cond.annotation;
            let cond_val = eval(cond, env)?;
            match cond_val {
                Value::Boolean(true) => run_statement(body, env)?,
                Value::Boolean(false) => {
                    return Ok(());
                }
                _ => {
                    return Err(err("Condition value not a boolean", span));
                }
            }
        },
        Statement::For(loopdef, body) => {
            // We need to create a new stack frame for the loop, where the declaration is executed
            let start = loopdef
                .start
                .as_ref()
                .map(|e| eval(e, env))
                .transpose()?
                .unwrap_or(Value::Nil);
            env.run_in_substack(|env| {
                if let Some(VariableDecl(var_name)) = loopdef.var_name {
                    env.declare(var_name, start);
                }
                loop {
                    // Evaluate the condition
                    let cond_val = if let Some(ref cond) = loopdef.cond {
                        let span = cond.annotation;
                        let cond_val = eval(cond, env)?;
                        match cond_val {
                            Value::Boolean(x) => x,
                            _ => {
                                return Err(err("Condition value not a boolean", span));
                            }
                        }
                    } else {
                        true
                    };
                    // If the condition is false, end the loop
                    if !cond_val {
                        return Ok(());
                    }
                    // Evaluate the body
                    run_statement(body, env)?;
                    // Evaluate the increment
                    if let Some(ref inc) = loopdef.increment {
                        eval(inc, env)?;
                    }
                }
            })
        }
    }
}

pub fn run_declaration<'src, Dep: Deps>(
    s: &ResolvedDeclaration<'src>,
    env: &mut ExecEnv<'src, Dep>,
) -> Result<(), RuntimeError> {
    match s {
        Declaration::Var(VariableDecl(s), e) => {
            let v = if let Some(e) = e {
                eval(e, env)?
            } else {
                Value::Nil
            };
            env.declare(*s, v);
            Ok(())
        }
        Declaration::Statement(stmt) => run_statement(stmt, env),
        Declaration::Function { name, args, body } => {
            let func = LoxFunction {
                arguments: args.clone(),
                body: body.clone(),
                env: env.stack.clone(),
            };
            env.declare(name.0, Value::Function(func));
            Ok(())
        }
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
        printed: Vec<String>,
        time: f64,
    }
    impl Deps for TestDeps {
        fn print<'src>(&mut self, v: Value<'src>) {
            self.printed.push(format!("{v:?}"));
        }
        fn clock(&mut self) -> f64 {
            let time = self.time;
            self.time += 1.0;
            time
        }
    }

    fn get_output(source: &str) -> Vec<String> {
        let deps = TestDeps {
            printed: Vec::new(),
            time: 0.0,
        };
        let mut env = ExecEnv::new(deps);
        let tokens = parse_tokens(source).unwrap();
        let parser = parser::Parser::new(source, &tokens);
        let program = parser.parse_program().unwrap();
        let program = crate::resolution::resolve(program, source).unwrap();
        for stmt in program.decls {
            run_declaration(&stmt, &mut env).unwrap();
        }
        env.into_deps().printed
    }

    #[test]
    fn run_program() {
        let source = r#"
            var a = "hi";
            var b = 3;
            7;
            {
            print a;
            print b+1;
            }
        "#;
        let output = get_output(source);
        assert_eq!(output, vec!["String(\"hi\")", "Number(4.0)"]);
    }
    #[test]
    fn for_loop() {
        let source = r#"
            fun f() {
                for(var a = 1; a <= 4; a = a+1) {
                    var t = clock();
                    print a+t;
                }
            }
            f();
        "#;
        let output = get_output(source);
        assert_eq!(
            output,
            vec!["Number(1.0)", "Number(3.0)", "Number(5.0)", "Number(7.0)"]
        );
    }
    #[test]
    fn closure() {
        let source = r#"
            var f = 0;
            {
              var a = 0;
              fun g() {
                print a;
                a = a + 1;
              }
              f = g;
            }
            f();f();f();
        "#;
        let output = get_output(source);
        assert_eq!(output, vec!["Number(0.0)", "Number(1.0)", "Number(2.0)"]);
    }
    #[test]
    fn resolution() {
        let source = r#"
            var a = 1;
            {
                fun f() {
                    print a;
                }
                f();
                var a = 2;
                f();
            }
        "#;
        let output = get_output(source);
        assert_eq!(output, vec!["Number(1.0)", "Number(1.0)"]);
    }
}
