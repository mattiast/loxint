pub mod execution_env;
pub mod parser;
pub mod resolution;
pub mod runtime;
pub mod scanner;
pub mod syntax;

pub use resolution::ResolvedProgram as RunnableProgram;

pub fn parse_program(source: &str) -> Result<RunnableProgram, miette::Error> {
    let tokens = scanner::parse_tokens(source)?;
    let program = parser::Parser::new(source, &tokens).parse_program()?;
    let program = resolution::resolve(program, source)?;
    Ok(program)
}
// TODO One error type to include all errors
// Type alias for Resolved program
