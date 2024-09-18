use loxlang::parser;
use loxlang::scanner::parse_tokens;
use loxlang::{eval_expr::run_declaration, resolution::resolve_expr_no_var};
use miette::Result;
use std::io::Write;
use std::path::PathBuf;

fn main() -> Result<()> {
    let args = Cli::parse();
    match args.command {
        Commands::Run { file } => {
            // read a file into a string and parse a program
            let source = std::fs::read_to_string(&file).unwrap();
            // parse a program from source
            let tokens = parse_tokens(&source)?;
            let mut parser = parser::Parser::new(&source, &tokens);
            let program = parser.parse_program()?;
            let program = loxlang::resolution::resolve(program).unwrap();
            let mut env = loxlang::execution_env::ExecEnv::new_default();
            for stmt in program.decls {
                run_declaration(&stmt, &mut env).unwrap();
            }
            Ok(())
        }
        Commands::Repl => {
            print!("> ");
            std::io::stdout().flush().unwrap();
            // Read a line from stdin
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            let tokens = parse_tokens(&input).unwrap();
            let mut p = parser::Parser::new(&input, &tokens);
            let e = p.parse_expr()?;
            let e = resolve_expr_no_var(e).unwrap();
            if !p.done() {
                eprintln!("Unparsed tokens");
            }
            println!("{:?}", e);
            let mut env = loxlang::execution_env::ExecEnv::new_default();
            println!("{:?}", loxlang::eval_expr::eval(&e, &mut env));
            Ok(())
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
