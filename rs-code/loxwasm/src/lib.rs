use loxlang::execution_env::Deps;
use loxlang::execution_env::Value;
use loxlang::parser;
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
    let (rest, tokens) = scanner::parse_tokens(&src)
        .map_err(|e| LoxError(format!("Failed to parse tokens: {}", e)))?;
    if rest != "" {
        return Err(LoxError("Unparsed input remaining".to_string()));
    }
    let mut p = parser::Parser::new(&tokens);
    let e = p
        .parse_expr()
        .map_err(|e| LoxError(format!("Failed to parse expression: {:?}", e)))?;
    if !p.done() {
        return Err(LoxError("Unparsed tokens remaining".to_string()));
    }
    let mut stack = loxlang::execution_env::Stack::new();
    match loxlang::eval_expr::eval(&e, &mut stack) {
        Ok(Value::Number(x)) => Ok(x),
        Err(e) => Err(LoxError(format!("Evaluation error: {:?}", e))),
        Ok(Value::Boolean(_))
        | Ok(Value::String(_))
        | Ok(Value::Nil)
        | Ok(Value::NativeFunction(_)) => Err(LoxError(
            "Expected number, got non-number value".to_string(),
        )),
    }
}

#[wasm_bindgen]
pub fn run_program(src: String) -> Result<Vec<String>, LoxError> {
    let (rest, tokens) = scanner::parse_tokens(&src)
        .map_err(|e| LoxError(format!("Failed to parse tokens: {}", e)))?;
    if rest != "" {
        return Err(LoxError("Unparsed input remaining".to_string()));
    }
    let mut p = parser::Parser::new(&tokens);
    let program = p
        .parse_program()
        .map_err(|e| LoxError(format!("Failed to parse program: {:?}", e)))?;
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
    let deps = env.into_deps();
    let output = deps.printed.iter().map(|v| format!("{:?}", v)).collect();
    Ok(output)
}

struct TestDeps {
    printed: Vec<Value>,
}
impl Deps for TestDeps {
    fn print(&mut self, v: Value) {
        self.printed.push(v);
    }
    fn clock(&mut self) -> f64 {
        0.0
    }
}
