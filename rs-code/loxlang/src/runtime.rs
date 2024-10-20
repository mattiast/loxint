use std::mem;

use miette::Diagnostic;
use thiserror::Error;

use crate::execution_env::{AtomicValue, Deps, ExecEnv, LoxFunction, NativeFunc, NotFound, Value};

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
    src: &'src str,
}

#[derive(Debug)]
pub enum Interrupt<'src> {
    Return(Value<'src>),
    Break,
    Continue,
}

impl<'src, Dep: Deps> Runtime<'src, Dep> {
    pub fn new(src: &'src str, env: ExecEnv<'src, Dep>) -> Self {
        Runtime { env, src }
    }
    pub fn into_deps(self) -> Dep {
        self.env.into_deps()
    }
    fn err(&self, msg: &'static str, span: ByteSpan) -> RuntimeError {
        RuntimeError {
            source_offset: span.into(),
            msg,
            src: self.src.to_owned(),
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
            Expression::NumberLiteral(n) => Ok(Value::Atomic(AtomicValue::Number(*n))),
            Expression::BooleanLiteral(b) => Ok(Value::Atomic(AtomicValue::Boolean(*b))),
            Expression::StringLiteral(s) => Ok(Value::Atomic(AtomicValue::String(s.to_string()))),
            Expression::Nil => Ok(Value::Atomic(AtomicValue::Nil)),
            Expression::Identifier(Variable(var)) => match self.env.lookup(*var) {
                Some(v) => Ok(v),
                None => Err(self.err("Identifier not found", span)), // should not happen after resolution
            },
            Expression::Unary { operator, right } => {
                let right = self.eval(right.as_ref())?;
                match (operator, right) {
                    (UOperator::MINUS, Value::Atomic(AtomicValue::Number(n))) => {
                        Ok(Value::Atomic(AtomicValue::Number(-n)))
                    }
                    (UOperator::BANG, Value::Atomic(AtomicValue::Boolean(b))) => {
                        Ok(Value::Atomic(AtomicValue::Boolean(!b)))
                    }
                    (UOperator::BANG, Value::Atomic(AtomicValue::Nil)) => {
                        Ok(Value::Atomic(AtomicValue::Boolean(true)))
                    }
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
                    (Value::Atomic(AtomicValue::Boolean(l)), BOperator::AND) => {
                        if !l {
                            Ok(Value::Atomic(AtomicValue::Boolean(false)))
                        } else {
                            self.eval(right.as_ref())
                        }
                    }
                    (Value::Atomic(AtomicValue::Boolean(l)), BOperator::OR) => {
                        if l {
                            Ok(Value::Atomic(AtomicValue::Boolean(true)))
                        } else {
                            self.eval(right.as_ref())
                        }
                    }
                    (left, _) => {
                        // In rest of the cases we always need to evaluate both operands
                        let right = self.eval(right.as_ref())?;
                        match (left, operator, right) {
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::PLUS,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Number(l + r))),
                            (
                                Value::Atomic(AtomicValue::String(l)),
                                BOperator::PLUS,
                                Value::Atomic(AtomicValue::String(r)),
                            ) => Ok(Value::Atomic(AtomicValue::String(l + &r))),
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::MINUS,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Number(l - r))),
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::STAR,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Number(l * r))),
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::SLASH,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Number(l / r))), // div by zero?
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::LESS,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Boolean(l < r))),
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::LessEqual,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Boolean(l <= r))),
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::GREATER,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Boolean(l > r))),
                            (
                                Value::Atomic(AtomicValue::Number(l)),
                                BOperator::GreaterEqual,
                                Value::Atomic(AtomicValue::Number(r)),
                            ) => Ok(Value::Atomic(AtomicValue::Boolean(l >= r))),
                            (l, BOperator::EqualEqual, r) => {
                                Ok(Value::Atomic(AtomicValue::Boolean(l == r)))
                            }
                            (l, BOperator::BangEqual, r) => {
                                Ok(Value::Atomic(AtomicValue::Boolean(l != r)))
                            }
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
                            let res = runtime.run_statement(&f.body)?;
                            match res {
                                Ok(()) => Ok(Value::Atomic(AtomicValue::Nil)),
                                Err(Interrupt::Return(v)) => Ok(v),
                                Err(_) => Err(runtime.err("Not in a loop", span)),
                            }
                        });
                        self.env.stack = old_stack;
                        result
                    }
                    Value::NativeFunction(NativeFunc::Clock) => {
                        if args.is_empty() {
                            Ok(Value::Atomic(AtomicValue::Number(self.env.clock())))
                        } else {
                            Err(self.err("Wrong number of arguments in function application", span))
                        }
                    }
                    _ => Err(self.err("Not a function", span)),
                }
            }
        }
    }

    // Continuation to handle return value
    fn run_statement(
        &mut self,
        s: &ResolvedStatement<'src>,
    ) -> Result<Result<(), Interrupt<'src>>, RuntimeError> {
        match s {
            Statement::Expression(e) => {
                let _ = self.eval(e)?;
                Ok(Ok(()))
            }
            Statement::Print(e) => {
                let v = self.eval(e)?;
                self.env.print(v);
                Ok(Ok(()))
            }
            Statement::Block(decls) => self.run_in_substack(|runtime| {
                for d in decls {
                    let res = runtime.run_declaration(d)?;
                    if let Err(i) = res {
                        return Ok(Err(i));
                    }
                }
                Ok(Ok(()))
            }),
            Statement::If(cond, stmt_then, stmt_else) => {
                let span = cond.annotation;
                let cond_val = self.eval(cond)?;
                match cond_val {
                    Value::Atomic(AtomicValue::Boolean(b)) => {
                        if b {
                            self.run_statement(stmt_then)
                        } else {
                            if let Some(stmt_else) = stmt_else {
                                self.run_statement(stmt_else)
                            } else {
                                Ok(Ok(()))
                            }
                        }
                    }
                    _ => Err(self.err("Condition value not a boolean", span)),
                }
            }
            Statement::While(cond, body) => loop {
                let span = cond.annotation;
                let cond_val = self.eval(cond)?;
                match cond_val {
                    Value::Atomic(AtomicValue::Boolean(b)) => {
                        if b {
                            let res = self.run_statement(body)?;
                            if let Err(i) = res {
                                return Ok(Err(i));
                            }
                        } else {
                            return Ok(Ok(()));
                        }
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
                    .unwrap_or(Value::Atomic(AtomicValue::Nil));
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
                                Value::Atomic(AtomicValue::Boolean(x)) => x,
                                _ => {
                                    return Err(runtime.err("Condition value not a boolean", span));
                                }
                            }
                        } else {
                            true
                        };
                        // If the condition is false, end the loop
                        if !cond_val {
                            return Ok(Ok(()));
                        }
                        // Evaluate the body
                        let res = runtime.run_statement(body)?;
                        if let Err(i) = res {
                            return Ok(Err(i));
                        }
                        // Evaluate the increment
                        if let Some(ref inc) = loopdef.increment {
                            runtime.eval(inc)?;
                        }
                    }
                })
            }
            Statement::Return(expr) => {
                let value = self.eval(expr)?;
                Ok(Err(Interrupt::Return(value)))
            }
        }
    }

    // TODO make this private, and add `run_program` that checks that no interrupts are returned
    pub fn run_declaration(
        &mut self,
        s: &ResolvedDeclaration<'src>,
    ) -> Result<Result<(), Interrupt<'src>>, RuntimeError> {
        match s {
            Declaration::Var(VariableDecl(s), e) => {
                let v = if let Some(e) = e {
                    self.eval(e)?
                } else {
                    Value::Atomic(AtomicValue::Nil)
                };
                self.env.declare(*s, v);
                Ok(Ok(()))
            }
            Declaration::Statement(stmt) => self.run_statement(stmt),
            Declaration::Function { name, args, body } => {
                let func = LoxFunction {
                    arguments: args.clone(),
                    body: body.clone(),
                    env: self.env.stack.clone(),
                };
                self.env.declare(name.0, Value::Function(func));
                Ok(Ok(()))
            }
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
            self.printed.push(format!("{v}"));
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
        let mut runtime = Runtime::new(source, env);
        let tokens = parse_tokens(source).unwrap();
        let parser = parser::Parser::new(source, &tokens);
        let program = parser.parse_program().unwrap();
        let program = crate::resolution::resolve(program, source).unwrap();
        for stmt in program.decls {
            runtime.run_declaration(&stmt).unwrap().unwrap();
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
        assert_eq!(output, vec!["hi", "4"]);
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
        assert_eq!(output, vec!["1", "3", "5", "7"]);
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
        assert_eq!(output, vec!["0", "1", "2"]);
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
        assert_eq!(output, vec!["1", "1"]);
    }
    #[test]
    fn fn_return_value() {
        let source = r#"
            fun f(x, y) {
                return x+y;
            }
            print(f(1, 2));
            print(f(3, 4));
        "#;
        let output = get_output(source);
        assert_eq!(output, vec!["3", "7"]);
    }
}
