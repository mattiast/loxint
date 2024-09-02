use loxlang::eval_expr::run_statement;
use loxlang::parser;
use loxlang::scanner::parse_tokens;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let args = Cli::parse();
    match args.command {
        Commands::Run { file } => {
            // read a file into a string and parse a program
            let source = std::fs::read_to_string(&file).unwrap();
            // parse a program from source
            let (rest, tokens) = parse_tokens(&source).unwrap();
            assert_eq!(rest, "");
            let mut parser = parser::Parser::new(&tokens);
            let program = parser.parse_program().unwrap();
            let mut stack = loxlang::eval_expr::Stack::new();
            for stmt in program.decls {
                run_statement(&stmt, &mut stack).unwrap();
            }
        }
        Commands::Repl => {
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
            let mut stack = loxlang::eval_expr::Stack::new();
            println!("{:?}", loxlang::eval_expr::eval(&e, &mut stack));
        }
    }
}

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Start the REPL (Read-Eval-Print Loop)
    Repl,
    /// Run a Lox script from a file
    Run {
        /// The path to the Lox script file
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
}
