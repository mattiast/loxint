use clap::{Parser, Subcommand};
use loxlang::parser;
use loxlang::resolution::{resolve_expr_no_var, Resolver};
use loxlang::scanner::parse_tokens;
use loxlang::syntax::{Declaration, Statement};
use miette::Result;
use std::io::Write;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Start the REPL (Read-Eval-Print Loop)
    Repl {
        #[arg(long, action)]
        debug: bool,
    },
    /// Run a Lox script from a file
    Run {
        /// The path to the Lox script file
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
}

fn main() -> Result<()> {
    let args = match Cli::try_parse() {
        Ok(args) => args,
        Err(e) => e.exit(),
    };
    match args.command {
        Commands::Run { file } => {
            // read a file into a string and parse a program
            let source = std::fs::read_to_string(&file).unwrap();
            // parse a program from source
            let program = loxlang::parse_program(&source)?;
            let env = loxlang::execution_env::ExecEnv::new_default();
            let mut runtime = loxlang::runtime::Runtime::new(&source, env);
            for stmt in program.decls {
                runtime.run_declaration(&stmt)?;
            }
            Ok(())
        }
        Commands::Repl { debug } => {
            let env = loxlang::execution_env::ExecEnv::new_default();
            let mut runtime = loxlang::runtime::Runtime::new("", env);
            let mut resolver = Resolver::new("");
            loop {
                print!("> ");
                std::io::stdout().flush().unwrap();
                // Read a line from stdin
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).unwrap();
                if input.is_empty() {
                    break;
                }
                // NOTE: We have to leak the string to make lifetimes work.
                // The runtime expects shared parts of the code to have same lifetime as the runtime
                // itself, and the string we have won't life long enough.
                let s: &'static str = Box::leak(input.into_boxed_str());
                let tokens = parse_tokens(&s)?;
                let mut p = parser::Parser::new(&s, &tokens);

                // NOTE: for expression, you need a semicolon at the end
                let decl = p.parse_declaration()?;
                let decl = resolver.resolve_declaration(decl)?;
                if !p.done() {
                    eprintln!("Unparsed tokens");
                }
                if debug {
                    println!("Input expression: {:?}", decl);
                }
                match decl {
                    Declaration::Statement(Statement::Expression(e)) => {
                        println!("{}", runtime.eval(&e)?);
                    }
                    decl => {
                        let res = runtime.run_declaration(&decl)?;
                        if let Err(_i) = res {
                            miette::bail!("bad return/break/continue");
                        }
                    }
                }
            }
            Ok(())
        }
    }
}
