pub mod execution_env;
pub mod parse;
pub mod resolution;
pub mod runtime;
pub mod syntax;

use miette::Diagnostic;
pub use resolution::ResolvedProgram as RunnableProgram;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum LoxError {
    #[error("lexical error")]
    #[diagnostic(transparent)]
    LexicalError(#[from] parse::scanner::LexicalError),
    #[error("syntax error")]
    #[diagnostic(transparent)]
    ParseError(#[from] parse::ParseError),
    #[error("variable resolution error")]
    #[diagnostic(transparent)]
    ResolutionError(#[from] resolution::ResolutionError),
    #[error("runtime error")]
    #[diagnostic(transparent)]
    RuntimeError(#[from] runtime::RuntimeError),
}

pub fn parse_program(source: &str) -> Result<RunnableProgram<'_>, LoxError> {
    let tokens = parse::scanner::parse_tokens(source)?;
    let program = parse::Parser::new(source, &tokens).parse_program()?;
    let program = resolution::resolve(program, source)?;
    Ok(program)
}
