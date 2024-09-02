use loxlang::eval_expr::Value;
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
pub fn evalexpr(src: String) -> Result<f64, LoxError> {
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
    let mut env = loxlang::eval_expr::EvalEnv::new_global_env();
    match loxlang::eval_expr::eval(&e, &mut env) {
        Ok(Value::Number(x)) => Ok(x),
        Err(e) => Err(LoxError(format!("Evaluation error: {:?}", e))),
        Ok(Value::Boolean(_)) | Ok(Value::String(_)) | Ok(Value::Nil) => Err(LoxError(
            "Expected number, got non-number value".to_string(),
        )),
    }
}
