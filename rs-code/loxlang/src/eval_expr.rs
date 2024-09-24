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
    #[source_code]
    pub src: String,
    #[label("Something wrong here")]
    pub source_offset: miette::SourceSpan,
    pub msg: &'static str,
}

pub struct Runtime<'src, Dep: Deps> {
    env: ExecEnv<'src, Dep>,
    src: String,
}

impl<'src, Dep: Deps> Runtime<'src, Dep> {
    pub fn new(src: String, env: ExecEnv<'src, Dep>) -> Self {
        Runtime { env, src }
    }
    pub fn into_deps(self) -> Dep {
        self.env.into_deps()
    }
    fn err(&self, msg: &'static str, span: ByteSpan) -> RuntimeError {
        RuntimeError {
            source_offset: span.into(),
            msg,
            src: self.src.clone(),
        }
    }

    fn run_in_substack<F, U>(&mut self, f: F) -> U
    where
        F: FnOnce(&mut Runtime<'src, Dep>) -> U,
    {
        // TODO Gosh
        self.env.stack.go_to_local_env();
        let v = f(self);
        self.env.stack.go_to_parent_env();
        v
    }
    pub fn eval(&mut self, e: &ResolvedExpression<'src>) -> Result<Value<'src>, RuntimeError> {
        let span = e.annotation;
        match &e.value {
            Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
            Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
            Expression::StringLiteral(s) => Ok(Value::String(s.to_string())),
            Expression::Nil => Ok(Value::Nil),
            Expression::Identifier(Variable(var)) => match self.env.lookup(*var) {
                Some(v) => Ok(v),
                None => Err(self.err("Identifier not found", span)), // should not happen after resolution
            },
            Expression::Unary { operator, right } => {
                let right = self.eval(right.as_ref())?;
                match (operator, right) {
                    (UOperator::MINUS, Value::Number(n)) => Ok(Value::Number(-n)),
                    (UOperator::BANG, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                    (UOperator::BANG, Value::Nil) => Ok(Value::Boolean(true)),
                    // TODO Truthiness?
                    _ => Err(self.err("Unary operator not supported for this type", span)),
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.eval(left.as_ref())?;
                match (left, operator) {
                    // First check some cases where we may shor-circuit and not evaluate the right side
                    (Value::Boolean(l), BOperator::AND) => {
                        if !l {
                            Ok(Value::Boolean(false))
                        } else {
                            self.eval(right.as_ref())
                        }
                    }
                    (Value::Boolean(l), BOperator::OR) => {
                        if l {
                            Ok(Value::Boolean(true))
                        } else {
                            self.eval(right.as_ref())
                        }
                    }
                    (left, _) => {
                        // In rest of the cases we always need to evaluate both operands
                        let right = self.eval(right.as_ref())?;
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
                            _ => Err(self.err("Binary not supported for these types", span)),
                        }
                    }
                }
            }
            Expression::Assignment(Variable(name), value) => {
                let val = self.eval(value.as_ref())?;
                match self.env.assign(*name, val.clone()) {
                    Ok(()) => Ok(val),
                    Err(NotFound) => Err(self.err(
                        "Variable not found (should not happen after resolution)",
                        span,
                    )),
                }
            }
            Expression::FunctionCall(f, args) => {
                let f = self.eval(f.as_ref())?;
                let args = args
                    .iter()
                    .map(|a| self.eval(a))
                    .collect::<Result<Vec<_>, _>>()?;
                match f {
                    Value::Function(f) => {
                        if args.len() != f.arguments.len() {
                            return Err(
                                self.err("Wrong number of arguments in function application", span)
                            );
                        }
                        // We need to temporarily replace the stack with the function's environment,
                        // evaluate the function body, and then restore the old stack.
                        // TODO: no huh huh, this should use some sort of RAII or another callback thing
                        let old_stack = mem::replace(&mut self.env.stack, f.env.clone());

                        let result = self.run_in_substack(|runtime| {
                            for (VariableDecl(arg_name), arg_value) in
                                f.arguments.iter().zip(args.into_iter())
                            {
                                runtime.env.declare(*arg_name, arg_value);
                            }
                            runtime.run_statement(&f.body)?;
                            // TODO return values
                            // Should run_statement take a Continuation as a parameter??
                            Ok(Value::Nil)
                        });
                        self.env.stack = old_stack;
                        result
                    }
                    Value::NativeFunction(NativeFunc::Clock) => {
                        if args.is_empty() {
                            Ok(Value::Number(self.env.clock()))
                        } else {
                            Err(self.err("Wrong number of arguments in function application", span))
                        }
                    }
                    _ => Err(self.err("Not a function", span)),
                }
            }
        }
    }

    pub fn run_statement(&mut self, s: &ResolvedStatement<'src>) -> Result<(), RuntimeError> {
        match s {
            Statement::Expression(e) => {
                let _ = self.eval(e)?;
                Ok(())
            }
            Statement::Print(e) => {
                let v = self.eval(e)?;
                self.env.print(v);
                Ok(())
            }
            Statement::Block(decls) => self.run_in_substack(|runtime| {
                for d in decls {
                    runtime.run_declaration(d)?;
                }
                Ok(())
            }),
            Statement::If(cond, stmt_then, stmt_else) => {
                let span = cond.annotation;
                let cond_val = self.eval(cond)?;
                match cond_val {
                    Value::Boolean(true) => self.run_statement(stmt_then),
                    Value::Boolean(false) => {
                        if let Some(stmt_else) = stmt_else {
                            self.run_statement(stmt_else)
                        } else {
                            Ok(())
                        }
                    }
                    _ => Err(self.err("Condition value not a boolean", span)),
                }
            }
            Statement::While(cond, body) => loop {
                let span = cond.annotation;
                let cond_val = self.eval(cond)?;
                match cond_val {
                    Value::Boolean(true) => self.run_statement(body)?,
                    Value::Boolean(false) => {
                        return Ok(());
                    }
                    _ => {
                        return Err(self.err("Condition value not a boolean", span));
                    }
                }
            },
            Statement::For(loopdef, body) => {
                // We need to create a new stack frame for the loop, where the declaration is executed
                let start = loopdef
                    .start
                    .as_ref()
                    .map(|e| self.eval(e))
                    .transpose()?
                    .unwrap_or(Value::Nil);
                self.run_in_substack(|runtime| {
                    if let Some(VariableDecl(var_name)) = loopdef.var_name {
                        runtime.env.declare(var_name, start);
                    }
                    loop {
                        // Evaluate the condition
                        let cond_val = if let Some(ref cond) = loopdef.cond {
                            let span = cond.annotation;
                            let cond_val = runtime.eval(cond)?;
                            match cond_val {
                                Value::Boolean(x) => x,
                                _ => {
                                    return Err(runtime.err("Condition value not a boolean", span));
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
                        runtime.run_statement(body)?;
                        // Evaluate the increment
                        if let Some(ref inc) = loopdef.increment {
                            runtime.eval(inc)?;
                        }
                    }
                })
            }
        }
    }

    pub fn run_declaration(&mut self, s: &ResolvedDeclaration<'src>) -> Result<(), RuntimeError> {
        match s {
            Declaration::Var(VariableDecl(s), e) => {
                let v = if let Some(e) = e {
                    self.eval(e)?
                } else {
                    Value::Nil
                };
                self.env.declare(*s, v);
                Ok(())
            }
            Declaration::Statement(stmt) => self.run_statement(stmt),
            Declaration::Function { name, args, body } => {
                let func = LoxFunction {
                    arguments: args.clone(),
                    body: body.clone(),
                    env: self.env.stack.clone(),
                };
                self.env.declare(name.0, Value::Function(func));
                Ok(())
            }
        }
    }
}

// TODO Creating a new scope could be done in RAII fashion, and when the "scope goes out of scope", it will pop the environment

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
        let env = ExecEnv::new(deps);
        let mut runtime = Runtime::new(source.to_owned(), env);
        let tokens = parse_tokens(source).unwrap();
        let parser = parser::Parser::new(source, &tokens);
        let program = parser.parse_program().unwrap();
        let program = crate::resolution::resolve(program, source).unwrap();
        for stmt in program.decls {
            runtime.run_declaration(&stmt).unwrap();
        }
        runtime.into_deps().printed
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
