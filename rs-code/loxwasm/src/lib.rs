use loxlang::execution_env::AtomicValue;
use loxlang::execution_env::Deps;
use loxlang::execution_env::Value;
use loxlang::parse;
use loxlang::parse::chumsky_parser;
use loxlang::resolution::resolve;
use loxlang::resolution::resolve_expr_no_var;
use loxlang::LoxError;
use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::wasm_bindgen;

/// Span information indicating where an error occurred in the source code
#[derive(Debug, Clone, Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct ErrorSpan {
    /// The byte offset where the error starts
    pub start: usize,
    /// The byte offset where the error ends
    pub end: usize,
}

/// Structured error information with location details
#[derive(Debug, Clone, Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct ErrorInfo {
    /// The error message
    pub message: String,
    /// The span in the source code where the error occurred
    pub span: ErrorSpan,
    /// The type of error (e.g., "lexical", "resolution", "runtime")
    pub error_type: String,
}

impl From<LoxError> for ErrorInfo {
    fn from(error: LoxError) -> Self {
        let error_type = match &error {
            LoxError::LexicalError(_) => "lexical",
            LoxError::ParseError(_) => "parse",
            LoxError::ResolutionError(_) => "resolution",
            LoxError::RuntimeError(_) => "runtime",
        };
        let span = error.span();

        ErrorInfo {
            message: error.message(),
            span: ErrorSpan {
                start: span.offset(),
                end: span.offset() + span.len(),
            },
            error_type: error_type.to_string(),
        }
    }
}

/// Result type for Lox operations
#[derive(Debug, Clone, Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type", content = "value")]
pub enum LoxResult<T> {
    Success(T),
    Error(ErrorInfo),
}

#[wasm_bindgen]
pub fn eval_expr(src: String) -> LoxResult<f64> {
    match eval_expr_inner(&src) {
        Ok(Value::Atomic(AtomicValue::Number(x))) => LoxResult::Success(x),
        Ok(Value::Atomic(x)) => LoxResult::Error(ErrorInfo {
            message: format!("Expected number, got another value {}", x),
            span: ErrorSpan { start: 0, end: src.len() },
            error_type: "type".to_string(),
        }),
        Ok(Value::Function(_)) | Ok(Value::NativeFunction(_)) => LoxResult::Error(ErrorInfo {
            message: "Expected number, got a function".to_string(),
            span: ErrorSpan { start: 0, end: src.len() },
            error_type: "type".to_string(),
        }),
        Err(e) => LoxResult::Error(e.into()),
    }
}

fn eval_expr_inner(src: &str) -> Result<Value<'_>, LoxError> {
    let tokens = parse::scanner::parse_tokens(&src)?;
    let mut p = parse::Parser::new(&src, &tokens);
    let e = p.parse_expr()?;
    let e = resolve_expr_no_var(e, &src)?;

    if !p.done() {
        return Err(LoxError::ParseError(parse::ParseError::UnexpectedEnd {
            span: src.len().into(),
            src: src.to_string(),
            help: "Unparsed tokens remaining".to_string(),
        }));
    }

    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(env);

    let x = runtime.eval(&e)?;
    Ok(x)
}

/// Evaluate an expression using the chumsky parser
#[wasm_bindgen]
pub fn eval_expr_chumsky(src: String) -> LoxResult<f64> {
    match eval_expr_chumsky_inner(&src) {
        Ok(Value::Atomic(AtomicValue::Number(x))) => LoxResult::Success(x),
        Ok(Value::Atomic(x)) => LoxResult::Error(ErrorInfo {
            message: format!("Expected number, got another value {}", x),
            span: ErrorSpan { start: 0, end: src.len() },
            error_type: "type".to_string(),
        }),
        Ok(Value::Function(_)) | Ok(Value::NativeFunction(_)) => LoxResult::Error(ErrorInfo {
            message: "Expected number, got a function".to_string(),
            span: ErrorSpan { start: 0, end: src.len() },
            error_type: "type".to_string(),
        }),
        Err(e) => LoxResult::Error(e.into()),
    }
}

fn eval_expr_chumsky_inner(src: &str) -> Result<Value<'_>, LoxError> {
    use chumsky::Parser;

    // Lex the source code
    let tokens = chumsky_parser::lexer()
        .parse(src)
        .into_result()
        .map_err(|errors| {
            // Take the first error
            let err = errors.into_iter().next().unwrap();
            let span = err.span();
            LoxError::LexicalError(parse::scanner::LexicalError {
                src: src.to_string(),
                source_offset: span.start.into(),
            })
        })?;

    // Parse the tokens into an AST
    let e = chumsky_parser::expr_parser()
        .parse(&tokens)
        .into_result()
        .map_err(|errors| {
            // Take the first error
            let err = errors.into_iter().next().unwrap();
            let span = err.span();
            LoxError::ParseError(parse::ParseError::UnexpectedToken {
                src: src.to_string(),
                span: miette::SourceSpan::new(span.start.into(), span.end - span.start),
                help: format!("Unexpected token while parsing expression"),
            })
        })?;

    // Resolve variables
    let e = resolve_expr_no_var(e, &src)?;

    // Execute
    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(env);

    let x = runtime.eval(&e)?;
    Ok(x)
}

#[wasm_bindgen]
pub fn run_program(src: String) -> LoxResult<Vec<String>> {
    match run_program_inner(&src) {
        Ok(out) => LoxResult::Success(out),
        Err(e) => LoxResult::Error(e.into()),
    }
}

fn run_program_inner(src: &str) -> Result<Vec<String>, LoxError> {
    let tokens = parse::scanner::parse_tokens(&src)?;
    let program = parse::Parser::new(&src, &tokens).parse_program()?;
    let program = resolve(program, &src)?;

    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(env);

    runtime.run_program(&program)?;
    Ok(runtime.into_deps().printed)
}

struct TestDeps {
    printed: Vec<String>,
}
impl Deps for TestDeps {
    fn print(&mut self, v: Value) {
        self.printed.push(format!("{:?}", v));
    }
    fn clock(&mut self) -> f64 {
        0.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_expr_chumsky_literals() {
        match eval_expr_chumsky("42".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 42.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr_chumsky("3.14".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 3.14),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_chumsky_arithmetic() {
        match eval_expr_chumsky("2 + 3".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 5.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr_chumsky("10 - 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 6.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr_chumsky("3 * 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 12.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr_chumsky("20 / 5".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 4.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_chumsky_precedence() {
        // 2 + 3 * 4 should be 2 + 12 = 14
        match eval_expr_chumsky("2 + 3 * 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 14.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        // (2 + 3) * 4 should be 5 * 4 = 20
        match eval_expr_chumsky("(2 + 3) * 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 20.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_chumsky_unary() {
        match eval_expr_chumsky("-5".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, -5.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr_chumsky("-(2 + 3)".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, -5.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_chumsky_complex() {
        // (2 + 3) * 4 - 5 / 2 should be 5 * 4 - 2.5 = 20 - 2.5 = 17.5
        match eval_expr_chumsky("(2 + 3) * 4 - 5 / 2".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 17.5),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_comparison() {
        // Old parser vs new parser - should produce same results
        let test_exprs = vec!["42", "2 + 3", "10 * 2", "(5 + 3) * 2"];

        for expr in test_exprs {
            let old_result = eval_expr(expr.to_string());
            let new_result = eval_expr_chumsky(expr.to_string());

            match (old_result, new_result) {
                (LoxResult::Success(old), LoxResult::Success(new)) => {
                    assert_eq!(old, new, "Results differ for expression: {}", expr);
                }
                _ => panic!("One parser failed for expression: {}", expr),
            }
        }
    }
}
