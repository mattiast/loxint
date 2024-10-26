pub mod execution_env;
pub mod parse;
pub mod resolution;
pub mod runtime;
pub mod syntax;

pub use resolution::ResolvedProgram as RunnableProgram;

pub fn parse_program(source: &str) -> Result<RunnableProgram, miette::Error> {
    let tokens = parse::scanner::parse_tokens(source)?;
    let program = parse::Parser::new(source, &tokens).parse_program()?;
    let program = resolution::resolve(program, source)?;
    Ok(program)
}
// TODO One error type to include all errors
