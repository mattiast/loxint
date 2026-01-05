pub mod chumsky_parser;
use std::ops;

use miette::{Diagnostic, SourceOffset};
use thiserror::Error;

use crate::syntax::{
    AnnotatedExpression, AnnotatedStatement, Declaration, Expression, Program, Statement,
    VariableDecl,
};

#[derive(Error, Debug, Diagnostic)]
#[error("lexical_error")]
pub struct LexicalError {
    #[source_code]
    pub src: String,
    #[label("No valid token here")]
    pub source_offset: SourceOffset,
}

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("unexpected token")]
    UnexpectedToken {
        #[source_code]
        src: String,
        #[label("Unexpected token")]
        span: miette::SourceSpan,
        #[help]
        help: String,
    },
    #[error("unexpected end of input")]
    UnexpectedEnd {
        #[source_code]
        src: String,
        #[label("Input ends here")]
        span: SourceOffset,
        #[help]
        help: String,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct ByteSpan {
    pub start: usize,
    pub end: usize,
}

impl ByteSpan {
    fn len(&self) -> usize {
        self.end - self.start
    }
}

impl From<std::ops::Range<usize>> for ByteSpan {
    fn from(value: std::ops::Range<usize>) -> Self {
        ByteSpan {
            start: value.start,
            end: value.end,
        }
    }
}

impl ops::BitOr<ByteSpan> for ByteSpan {
    type Output = ByteSpan;

    fn bitor(self, rhs: ByteSpan) -> Self::Output {
        ByteSpan {
            start: std::cmp::min(self.start, rhs.start),
            end: std::cmp::max(self.end, rhs.end),
        }
    }
}

impl From<ByteSpan> for miette::SourceSpan {
    fn from(value: ByteSpan) -> Self {
        (value.start, value.len()).into()
    }
}

pub type ParsedExpression<'src> = AnnotatedExpression<'src, &'src str, ByteSpan>;
pub type ParsedStatement<'src> = AnnotatedStatement<'src, &'src str, &'src str, ByteSpan>;
pub type ParsedDeclaration<'src> = Declaration<'src, &'src str, &'src str, ByteSpan>;
pub type ParsedProgram<'src> = Program<'src, &'src str, &'src str, ByteSpan>;

/// Combination of `var_name` and `start` has 4 cases:
/// 1. var_name is Some and start is Some: this is `var x = 0;` case, the most typical one
/// 2. var_name is Some and start is None: this is `var x;` case, it is pretty weird but could happen I guess
/// 3. var_name is None and start is Some: this is `x = 0;` case where an existing variable is used, and the expression is typically an assignment
/// 4. Both are none: here the first part is empty `for(;...)`, initialization is done outside the loop
#[derive(Debug, Clone)]
pub struct ForLoopDef<'a, VR, VD, Ann> {
    pub var_name: Option<VariableDecl<VD>>,
    pub start: Option<AnnotatedExpression<'a, VR, Ann>>,
    pub cond: Option<AnnotatedExpression<'a, VR, Ann>>,
    pub increment: Option<AnnotatedExpression<'a, VR, Ann>>,
}

pub fn for_loop_into_while_loop<'src>(
    loopdef: ForLoopDef<'src, &'src str, &'src str, ByteSpan>,
    body: ParsedStatement<'src>,
) -> ParsedStatement<'src> {
    /*
    Case 1: Variable declaration
    for (var x = start ; cond ; incr) {body}

    Translates to

    {
        var x = start;
        while (cond) {
            {body}
            incr
        }
    }

    Case 2: No variable declaration
    for (start ; cond ; incr) {body}

    Translates to

    {
        start;
        while (cond) {
            {body}
            incr
        }
    }
    */
    let init_decl: Option<ParsedDeclaration<'src>> = match loopdef.var_name {
        Some(var_decl) => Some(Declaration::Var(var_decl, loopdef.start)),
        None => loopdef.start.map(|e| {
            let span = e.annotation;
            Declaration::Statement(Statement::Expression(e).annotate(span))
        }),
    };
    let cond = if let Some(cond) = loopdef.cond {
        cond
    } else {
        Expression::BooleanLiteral(true).annotate(
            // TODO get the span of the loopdef
            ByteSpan { start: 0, end: 0 },
        )
    };
    let mut decls = Vec::new();
    if let Some(decl) = init_decl {
        decls.push(decl);
    }
    let body_span = body.annotation;
    let span = body_span; // TODO find full span of the for loop
    decls.push(Declaration::Statement(
        Statement::While(
            cond,
            Box::new(
                Statement::Block({
                    let mut while_body = Vec::new();
                    while_body.push(Declaration::Statement(body));
                    if let Some(incr) = loopdef.increment {
                        let span = incr.annotation;
                        while_body.push(Declaration::Statement(
                            Statement::Expression(incr).annotate(span),
                        ));
                    }
                    while_body
                })
                .annotate(body_span),
            ),
        )
        .annotate(span),
    ));
    Statement::Block(decls).annotate(span)
}
