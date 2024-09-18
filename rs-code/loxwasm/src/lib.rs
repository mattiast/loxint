use loxlang::execution_env::Deps;
use loxlang::execution_env::Value;
use loxlang::parser;
use loxlang::resolution::resolve;
use loxlang::resolution::resolve_expr_no_var;
use loxlang::scanner;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[derive(Debug)]
pub struct LoxError(String);

impl From<LoxError> for JsValue {
    fn from(error: LoxError) -> Self {
        JsValue::from_str(&error.0)
    }
}

#[wasm_bindgen]
pub fn eval_expr(src: String) -> Result<f64, LoxError> {
    let tokens = scanner::parse_tokens(&src)
        .map_err(|e| LoxError(format!("Failed to parse tokens: {}", e)))?;
    let mut p = parser::Parser::new(&src, &tokens);
    let e = p
        .parse_expr()
        .map_err(|e| LoxError(format!("Failed to parse expression: {:?}", e)))?;
    let e = resolve_expr_no_var(e).unwrap();
    if !p.done() {
        return Err(LoxError("Unparsed tokens remaining".to_string()));
    }
    let deps = TestDeps {
        printed: Vec::new(),
    };
    let mut env = loxlang::execution_env::ExecEnv::new(deps);
    match loxlang::eval_expr::eval(&e, &mut env) {
        Ok(Value::Number(x)) => Ok(x),
        Err(e) => Err(LoxError(format!("Evaluation error: {:?}", e))),
        _ => Err(LoxError(
            "Expected number, got non-number value".to_string(),
        )),
    }
}

#[wasm_bindgen]
pub fn run_program(src: String) -> Result<Vec<String>, LoxError> {
    let tokens = scanner::parse_tokens(&src)
        .map_err(|e| LoxError(format!("Failed to parse tokens: {}", e)))?;
    let mut p = parser::Parser::new(&src, &tokens);
    let program = p
        .parse_program()
        .map_err(|e| LoxError(format!("Failed to parse program: {:?}", e)))?;
    let program = resolve(program).unwrap();
    if !p.done() {
        return Err(LoxError("Unparsed tokens remaining".to_string()));
    }
    let deps = TestDeps {
        printed: Vec::new(),
    };
    let mut env = loxlang::execution_env::ExecEnv::new(deps);
    for stmt in program.decls {
        loxlang::eval_expr::run_declaration(&stmt, &mut env)
            .map_err(|e| LoxError(format!("{:?}", e)))?;
    }
    Ok(env.into_deps().printed)
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
