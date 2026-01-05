pub mod execution_env;
pub mod parse;
pub mod resolution;
pub mod runtime;
pub mod syntax;
pub mod test_runner;

use miette::Diagnostic;
pub use resolution::ResolvedProgram as RunnableProgram;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum LoxError {
    #[error("lexical error")]
    #[diagnostic(transparent)]
    LexicalError(#[from] parse::LexicalError),
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

impl LoxError {
    pub fn span(&self) -> miette::SourceSpan {
        match self {
            LoxError::LexicalError(e) => e.source_offset.into(),
            LoxError::ParseError(parse::ParseError::UnexpectedToken { span, .. }) => *span,
            LoxError::ParseError(parse::ParseError::UnexpectedEnd { span, .. }) => (*span).into(),
            LoxError::ResolutionError(e) => e.span,
            LoxError::RuntimeError(e) => e.source_offset.into(),
        }
    }
    pub fn message(&self) -> String {
        match self {
            LoxError::LexicalError(_) => format!("No valid token"),
            LoxError::ParseError(parse::ParseError::UnexpectedToken { help, .. }) => {
                format!("Parse error:\n{}", help)
            }
            LoxError::ParseError(parse::ParseError::UnexpectedEnd { help, .. }) => {
                format!("Unexpected end of input:\n{}", help)
            }
            LoxError::ResolutionError(e) => format!("{}", e.help),
            LoxError::RuntimeError(e) => format!("{}", e.msg),
        }
    }
}

pub fn parse_program(source: &str) -> Result<RunnableProgram<'_>, LoxError> {
    use chumsky::Parser;

    let program = parse::chumsky_parser::program_parser()
        .parse(source)
        .into_result()
        .map_err(|errors| {
            let err = errors.into_iter().next().unwrap();
            let span = err.span();

            parse::ParseError::UnexpectedToken {
                src: source.to_string(),
                span: miette::SourceSpan::new(span.start.into(), span.end - span.start),
                help: format!("Unexpected input while parsing program"),
            }
        })?;

    let program = resolution::resolve(program, source)?;
    Ok(program)
}
