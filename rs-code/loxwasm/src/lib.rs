use loxlang::execution_env::AtomicValue;
use loxlang::execution_env::Deps;
use loxlang::execution_env::Value;
use loxlang::parse;
use loxlang::resolution::resolve;
use loxlang::resolution::resolve_expr_no_var;
use loxlang::LoxError;
use miette::Diagnostic;
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
            message: error
                .help()
                .map_or("Error".to_string(), |e| format!("{}", e)),
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
