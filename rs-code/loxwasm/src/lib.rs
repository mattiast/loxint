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

/// Evaluate an expression using the chumsky parser
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
    use chumsky::Parser;

    // Parse the source directly into an AST
    let e = chumsky_parser::expr_parser()
        .parse(src)
        .into_result()
        .map_err(|errors| {
            // Take the first error
            let err = errors.into_iter().next().unwrap();
            let span = err.span();

            LoxError::ParseError(parse::ParseError::UnexpectedToken {
                src: src.to_string(),
                span: miette::SourceSpan::new(span.start.into(), span.end - span.start),
                help: format!("Unexpected input while parsing expression"),
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
    use chumsky::Parser;

    // Parse the program directly from source
    let program = chumsky_parser::program_parser()
        .parse(src)
        .into_result()
        .map_err(|errors| {
            // Take the first error
            let err = errors.into_iter().next().unwrap();
            let span = err.span();

            LoxError::ParseError(parse::ParseError::UnexpectedToken {
                src: src.to_string(),
                span: miette::SourceSpan::new(span.start.into(), span.end - span.start),
                help: format!("Unexpected input while parsing program"),
            })
        })?;

    // Resolve variables
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
    fn test_eval_expr_literals() {
        match eval_expr("42".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 42.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr("3.14".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 3.14),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_arithmetic() {
        match eval_expr("2 + 3".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 5.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr("10 - 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 6.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr("3 * 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 12.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr("20 / 5".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 4.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_precedence() {
        // 2 + 3 * 4 should be 2 + 12 = 14
        match eval_expr("2 + 3 * 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 14.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        // (2 + 3) * 4 should be 5 * 4 = 20
        match eval_expr("(2 + 3) * 4".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 20.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_unary() {
        match eval_expr("-5".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, -5.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }

        match eval_expr("-(2 + 3)".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, -5.0),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_eval_expr_complex() {
        // (2 + 3) * 4 - 5 / 2 should be 5 * 4 - 2.5 = 20 - 2.5 = 17.5
        match eval_expr("(2 + 3) * 4 - 5 / 2".to_string()) {
            LoxResult::Success(x) => assert_eq!(x, 17.5),
            LoxResult::Error(e) => panic!("Failed to evaluate: {:?}", e),
        }
    }

    #[test]
    fn test_parse_error_span_is_character_based() {
        // Test that parse error spans point to character positions, not token indices
        // Create a parse error: "1 + 2 + )" - the closing paren is unexpected
        let src = "1 + 2 + )";
        match eval_expr(src.to_string()) {
            LoxResult::Success(_) => panic!("Expected parse error but got success"),
            LoxResult::Error(e) => {
                // The error should be at the ')' character
                // Position: "1 + 2 + )" -> index 8
                // If it was using token indices, it would point to much earlier (token 6)
                assert_eq!(e.error_type, "parse");
                assert_eq!(e.span.start, 8, "Parse error should point to character 8 (the ')'), not token index");
            }
        }
    }
}
