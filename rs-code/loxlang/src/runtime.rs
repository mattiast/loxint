use std::mem;

use miette::Diagnostic;
use thiserror::Error;

use crate::execution_env::{AtomicValue, Deps, ExecEnv, LoxFunction, NativeFunc, NotFound, Value};

use crate::parse::ByteSpan;
use crate::resolution::{
    ResolvedDeclaration, ResolvedExpression, ResolvedProgram, ResolvedStatement,
};
use crate::syntax::{
    BOperator, Declaration, Expression, Statement, UOperator, Variable, VariableDecl,
};

#[derive(Error, Debug, Diagnostic)]
#[error("runtime_error")]
pub struct RuntimeError {
    #[label("Something wrong here")]
    pub source_offset: miette::SourceSpan,
    #[help]
    pub msg: &'static str,
}

pub struct Runtime<'src, Dep: Deps> {
    env: ExecEnv<'src, Dep>,
}

#[derive(Debug)]
pub enum Interrupt<'src> {
    Return(Value<'src>),
    Break,
    Continue,
}

impl<'src, Dep: Deps> Runtime<'src, Dep> {
    pub fn new(env: ExecEnv<'src, Dep>) -> Self {
        Runtime { env }
    }
    pub fn into_deps(self) -> Dep {
        self.env.into_deps()
    }
    fn err(&self, msg: &'static str, span: ByteSpan) -> RuntimeError {
        RuntimeError {
            source_offset: span.into(),
            msg,
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
        use AtomicValue::*;
        use Expression::*;
        use Value::Atomic;
        match &e.value {
            NumberLiteral(n) => Ok(Atomic(Number(*n))),
            BooleanLiteral(b) => Ok(Atomic(Boolean(*b))),
            StringLiteral(s) => Ok(Atomic(String(s.to_string()))),
            Expression::Nil => Ok(Value::Atomic(AtomicValue::Nil)),
            Identifier(Variable(var)) => match self.env.lookup(*var) {
                Some(v) => Ok(v),
                None => Err(self.err("Identifier not found", span)), // should not happen after resolution
            },
            Unary { operator, right } => {
                let right = self.eval(right.as_ref())?;
                match (operator, right) {
                    (UOperator::MINUS, Atomic(Number(n))) => {
                        Ok(Value::Atomic(AtomicValue::Number(-n)))
                    }
                    (UOperator::BANG, Atomic(Boolean(b))) => {
                        Ok(Value::Atomic(AtomicValue::Boolean(!b)))
                    }
                    (UOperator::BANG, Atomic(AtomicValue::Nil)) => Ok(Atomic(Boolean(true))),
                    _ => Err(self.err("Unary operator not supported for this type", span)),
                }
            }
            Binary {
                left,
                operator,
                right,
            } => {
                use BOperator::*;
                let left = self.eval(left.as_ref())?;
                match (left, operator) {
                    // First check some cases where we may shor-circuit and not evaluate the right side
                    (Atomic(Boolean(l)), AND) => {
                        if !l {
                            Ok(Atomic(Boolean(false)))
                        } else {
                            self.eval(right.as_ref())
                        }
                    }
                    (Atomic(Boolean(l)), OR) => {
                        if l {
                            Ok(Atomic(Boolean(true)))
                        } else {
                            self.eval(right.as_ref())
                        }
                    }
                    (left, _) => {
                        // In rest of the cases we always need to evaluate both operands
                        let right = self.eval(right.as_ref())?;
                        match (left, right) {
                            (Atomic(left), Atomic(right)) => {
                                match (left, operator, right) {
                                    (Number(l), PLUS, Number(r)) => Ok(Number(l + r)),
                                    (String(l), PLUS, String(r)) => Ok(String(l + &r)),
                                    (Number(l), MINUS, Number(r)) => Ok(Number(l - r)),
                                    (Number(l), STAR, Number(r)) => Ok(Number(l * r)),
                                    (Number(l), SLASH, Number(r)) => Ok(Number(l / r)), // div by zero?
                                    (Number(l), LESS, Number(r)) => Ok(Boolean(l < r)),
                                    (Number(l), LessEqual, Number(r)) => Ok(Boolean(l <= r)),
                                    (Number(l), GREATER, Number(r)) => Ok(Boolean(l > r)),
                                    (Number(l), GreaterEqual, Number(r)) => Ok(Boolean(l >= r)),
                                    (l, EqualEqual, r) => Ok(Boolean(l == r)),
                                    (l, BangEqual, r) => Ok(Boolean(l != r)),
                                    _ => Err(self
                                        .err("Binary not supported for these atomic types", span)),
                                }
                            }
                            .map(Atomic),
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
        match &s.value {
            Statement::Expression(e) => {
                let _ = self.eval(&e)?;
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
                    _ => Err(self.err("Condition value not a boolean", cond.annotation)),
                }
            }
            Statement::While(cond, body) => {
                // This is a loop
                loop {
                    let cond_val = self.eval(cond)?;
                    match cond_val {
                        Value::Atomic(AtomicValue::Boolean(b)) => {
                            if b {
                                let res = self.run_statement(body)?;
                                match res {
                                    Ok(()) | Err(Interrupt::Continue) => {}
                                    Err(Interrupt::Break) => return Ok(Ok(())),
                                    Err(i) => return Ok(Err(i)), // This is a "return interrupt"
                                }
                            } else {
                                return Ok(Ok(()));
                            }
                        }
                        _ => {
                            return Err(self.err("Condition value not a boolean", cond.annotation));
                        }
                    }
                }
            }
            Statement::Return(expr) => {
                let value = self.eval(expr)?;
                Ok(Err(Interrupt::Return(value)))
            }
        }
    }

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
    pub fn run_program(&mut self, program: &ResolvedProgram<'src>) -> Result<(), RuntimeError> {
        for stmt in &program.decls {
            if let Err(i) = self.run_declaration(stmt)? {
                return Err(match i {
                    Interrupt::Return(_) => self.err(
                        "return outside a function",
                        ByteSpan { start: 0, end: 0 }, // TODO
                    ),
                    Interrupt::Break => self.err(
                        "break outside a loop",
                        ByteSpan { start: 0, end: 0 }, // TODO
                    ),
                    Interrupt::Continue => self.err(
                        "continue outside a loop",
                        ByteSpan { start: 0, end: 0 }, // TODO
                    ),
                });
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::execution_env::Deps;
    use crate::execution_env::ExecEnv;
    use crate::parse;

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
        use chumsky::Parser;

        let deps = TestDeps {
            printed: Vec::new(),
            time: 0.0,
        };
        let env = ExecEnv::new(deps);
        let mut runtime = Runtime::new(env);

        // Parse using chumsky
        let program = parse::chumsky_parser::program_parser()
            .parse(source)
            .into_result()
            .unwrap();
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
