use clap::{Parser, Subcommand};
use loxlang::parse;
use loxlang::resolution::Resolver;
use loxlang::syntax::{Annotated, Declaration, Statement};
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
            let mut runtime = loxlang::runtime::Runtime::new(env);
            runtime
                .run_program(&program)
                .map_err(|err| miette::Error::from(err).with_source_code(source))?;
            Ok(())
        }
        Commands::Repl { debug } => {
            let env = loxlang::execution_env::ExecEnv::new_default();
            let mut runtime = loxlang::runtime::Runtime::new(env);
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

                use chumsky::Parser as ChumskyParser;

                // Lex the input
                let tokens = parse::chumsky_parser::lexer()
                    .parse(s)
                    .into_result()
                    .map_err(|errors| {
                        let err = errors.into_iter().next().unwrap();
                        let span = err.span();
                        parse::LexicalError {
                            src: s.to_string(),
                            source_offset: span.start.into(),
                        }
                    })?;

                // Parse a single declaration
                let decl = parse::chumsky_parser::decl_parser()
                    .parse(&tokens)
                    .into_result()
                    .map_err(|errors| {
                        let err = errors.into_iter().next().unwrap();
                        let token_span = err.span();

                        // Map token indices to character positions
                        let char_span = if token_span.start < tokens.len() {
                            let start_char = tokens[token_span.start].1.start;
                            let end_char = if token_span.end <= tokens.len() && token_span.end > 0 {
                                tokens[token_span.end - 1].1.end
                            } else {
                                tokens[token_span.start].1.end
                            };
                            miette::SourceSpan::new(start_char.into(), end_char - start_char)
                        } else {
                            miette::SourceSpan::new(s.len().into(), 0)
                        };

                        parse::ParseError::UnexpectedToken {
                            src: s.to_string(),
                            span: char_span,
                            help: format!("Unexpected token while parsing declaration"),
                        }
                    })?;

                let decl = resolver.resolve_declaration(decl)?;
                if debug {
                    println!("Input expression: {:?}", decl);
                }
                match decl {
                    Declaration::Statement(Annotated {
                        value: Statement::Expression(e),
                        annotation: _,
                    }) => {
                        println!(
                            "{}",
                            runtime
                                .eval(&e)
                                .map_err(|err| miette::Error::from(err).with_source_code(s))?
                        );
                    }
                    decl => {
                        let res = runtime.run_declaration(&decl)?;
                        if let Err(_i) = res {
                            // TODO can we get the span of the interrupt?
                            miette::bail!("bad return/break/continue");
                        }
                    }
                }
            }
            Ok(())
        }
    }
}
