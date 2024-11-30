use loxlang::execution_env::AtomicValue;
use loxlang::execution_env::Deps;
use loxlang::execution_env::Value;
use loxlang::parse;
use loxlang::resolution::resolve;
use loxlang::resolution::resolve_expr_no_var;
use miette::Diagnostic;
use miette::NarratableReportHandler;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

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

#[wasm_bindgen]
pub fn eval_expr(src: String) -> Result<f64, LoxError> {
    let tokens = parse::scanner::parse_tokens(&src)?;
    let mut p = parse::Parser::new(&src, &tokens);
    let e = p.parse_expr()?;
    let e = resolve_expr_no_var(e, &src)?;
    if !p.done() {
        return Err(LoxError("Unparsed tokens remaining".to_string()));
    }
    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(&src, env);
    match runtime.eval(&e) {
        Ok(Value::Atomic(AtomicValue::Number(x))) => Ok(x),
        Err(e) => Err(LoxError(format!("Evaluation error: {:?}", e))),
        _ => Err(LoxError(
            "Expected number, got non-number value".to_string(),
        )),
    }
}

#[wasm_bindgen]
pub fn run_program(src: String) -> Result<Vec<String>, LoxError> {
    let tokens = parse::scanner::parse_tokens(&src)?;
    let program = parse::Parser::new(&src, &tokens).parse_program()?;
    let program = resolve(program, &src)?;
    let deps = TestDeps {
        printed: Vec::new(),
    };
    let env = loxlang::execution_env::ExecEnv::new(deps);
    let mut runtime = loxlang::runtime::Runtime::new(&src, env);
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
