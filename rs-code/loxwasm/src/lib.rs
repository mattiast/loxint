use loxlang::execution_env::AtomicValue;
use loxlang::execution_env::Deps;
use loxlang::execution_env::Value;
use loxlang::parse;
use loxlang::resolution::resolve;
use loxlang::resolution::resolve_expr_no_var;
use miette::Diagnostic;
use miette::NarratableReportHandler;
use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

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

/// Result type for Lox operations
#[derive(Debug, Clone, Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type", content = "value")]
pub enum LoxResult<T> {
    Success(T),
    Error(ErrorInfo),
}

#[derive(Debug)]
pub struct LoxError(String);

impl From<LoxError> for JsValue {
    fn from(error: LoxError) -> Self {
        JsValue::from_str(error.0.as_str())
    }
}

impl<T: Diagnostic> From<T> for LoxError {
    fn from(error: T) -> Self {
        let mut output = String::new();
        NarratableReportHandler::new()
            .render_report(&mut output, &error)
            .unwrap();
        LoxError(output)
    }
}

/// Helper function to convert a Diagnostic error into ErrorInfo
fn diagnostic_to_error_info<T: Diagnostic>(error: T, error_type: &str) -> ErrorInfo {
    let mut output = String::new();
    NarratableReportHandler::new()
        .render_report(&mut output, &error)
        .unwrap();

    // Extract span information from the error's labels
    let span = if let Some(labels) = error.labels() {
        if let Some(label) = labels.into_iter().next() {
            let span = label.inner();
            ErrorSpan {
                start: span.offset(),
                end: span.offset() + span.len(),
            }
        } else {
            ErrorSpan { start: 0, end: 0 }
        }
    } else {
        ErrorSpan { start: 0, end: 0 }
    };

    ErrorInfo {
        message: output,
        span,
        error_type: error_type.to_string(),
    }
}

#[wasm_bindgen]
pub fn eval_expr(src: String) -> LoxResult<f64> {
    let tokens = match parse::scanner::parse_tokens(&src) {
        Ok(tokens) => tokens,
        Err(e) => return LoxResult::Error(diagnostic_to_error_info(e, "lexical")),
    };

    let mut p = parse::Parser::new(&src, &tokens);
    let e = match p.parse_expr() {
        Ok(e) => e,
        Err(e) => return LoxResult::Error(diagnostic_to_error_info(e, "parse")),
    };

    let e = match resolve_expr_no_var(e, &src) {
        Ok(e) => e,
        Err(e) => return LoxResult::Error(diagnostic_to_error_info(e, "resolution")),
    };

    if !p.done() {
        return LoxResult::Error(ErrorInfo {
            message: "Unparsed tokens remaining".to_string(),
            span: ErrorSpan { start: 0, end: 0 },
            error_type: "parse".to_string(),
        });
    }

    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(env);

    match runtime.eval(&e) {
        Ok(Value::Atomic(AtomicValue::Number(x))) => LoxResult::Success(x),
        Err(e) => LoxResult::Error(diagnostic_to_error_info(e, "runtime")),
        _ => LoxResult::Error(ErrorInfo {
            message: "Expected number, got non-number value".to_string(),
            span: ErrorSpan { start: 0, end: 0 },
            error_type: "type".to_string(),
        }),
    }
}

#[wasm_bindgen]
pub fn run_program(src: String) -> LoxResult<Vec<String>> {
    let tokens = match parse::scanner::parse_tokens(&src) {
        Ok(tokens) => tokens,
        Err(e) => return LoxResult::Error(diagnostic_to_error_info(e, "lexical")),
    };

    let program = match parse::Parser::new(&src, &tokens).parse_program() {
        Ok(program) => program,
        Err(e) => return LoxResult::Error(diagnostic_to_error_info(e, "parse")),
    };

    let program = match resolve(program, &src) {
        Ok(program) => program,
        Err(e) => return LoxResult::Error(diagnostic_to_error_info(e, "resolution")),
    };

    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(env);

    match runtime.run_program(&program) {
        Ok(_) => LoxResult::Success(runtime.into_deps().printed),
        Err(e) => LoxResult::Error(diagnostic_to_error_info(e, "runtime")),
    }
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
