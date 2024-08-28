use loxlang::parser;
use loxlang::parser::Parser;
use loxlang::scanner::parse_tokens;
use std::io::Write;
use std::path::PathBuf;

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
    let env = loxlang::eval_expr::EvalEnv {
        values: std::collections::HashMap::new(),
    };
    println!("{:?}", loxlang::eval_expr::eval(&e, &env));

    if false {
        let args = Opts::parse();
        // read a file into a string and parse a program
        let source = std::fs::read_to_string(&args.file).unwrap();
        // parse a program from source
        let (rest, tokens) = parse_tokens(&source).unwrap();
        assert_eq!(rest, "");
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
    }
}

// Use clap to parse command line arguments
use clap::Parser as ClapParser;
#[derive(ClapParser)]
struct Opts {
    file: PathBuf,
}
