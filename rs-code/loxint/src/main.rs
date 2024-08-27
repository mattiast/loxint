use std::io::Write;

use loxlang::parser;
use loxlang::scanner::parse_tokens;

fn main() {
    print!("> ");
    std::io::stdout().flush().unwrap();
    // Read a line from stdin
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let (rest, tokens) = parse_tokens(&input).unwrap();
    if rest != "" {
        eprintln!("Unparsed input: {}", rest);
    }
    let mut p = parser::Parser::new(&tokens);
    let e = p.parse_expr().unwrap();
    if !p.done() {
        eprintln!("Unparsed tokens");
    }
    println!("{:?}", e);
    let env = rs_code::eval_expr::EvalEnv {
        values: std::collections::HashMap::new(),
    };
    println!("{:?}", loxlang::eval_expr::eval(&e, &env));
}
