pub mod scanner;
pub mod chumsky_parser;
use std::ops::{self, Range};

use miette::{Diagnostic, SourceOffset};
use thiserror::Error;

use crate::syntax::{
    AnnotatedExpression, AnnotatedStatement, BOperator, Declaration, Expression, Program,
    Statement, UOperator, Variable, VariableDecl,
};
use scanner::{Reserved, Symbol, Token};

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
impl From<Range<usize>> for ByteSpan {
    fn from(value: Range<usize>) -> Self {
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

pub struct Parser<'list, 'src> {
    remaining: &'list [(Token<'src>, Range<usize>)],
    src: &'src str,
}

impl<'list, 'src> Parser<'list, 'src>
where
    'src: 'list,
{
    pub fn new(src: &'src str, tokens: &'list [(Token<'src>, Range<usize>)]) -> Self {
        Self {
            src,
            remaining: tokens,
        }
    }
    pub fn done(&self) -> bool {
        self.remaining.is_empty()
    }
    fn error(&self, msg: &str) -> ParseError {
        match self.remaining.first() {
            Some((_, r)) => ParseError::UnexpectedToken {
                src: self.src.to_owned(),
                span: r.clone().into(),
                help: msg.to_owned(),
            },
            None => ParseError::UnexpectedEnd {
                src: self.src.to_owned(),
                span: self.src.len().into(),
                help: msg.to_owned(),
            },
        }
    }
    fn parse_statement(&mut self) -> Result<ParsedStatement<'src>, ParseError> {
        if let Some(span1) = self.match_and_konsume(&Token::Reserved(Reserved::PRINT)) {
            let e = self.parse_expr()?;
            let span2 = self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            Ok(Statement::Print(e).annotate(span1 | span2))
        } else if let Some(span1) = self.match_and_konsume(&Token::Reserved(Reserved::RETURN)) {
            let e = self.parse_expr()?;
            let span2 = self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            Ok(Statement::Return(e).annotate(span1 | span2))
        } else if self.peek() == Some(&Token::Symbol(Symbol::LeftBrace)) {
            self.parse_block()
        } else if let Some(span1) = self.match_and_konsume(&Token::Reserved(Reserved::IF)) {
            self.consume(&Token::Symbol(Symbol::LeftParen))?;
            let e = self.parse_expr()?;
            self.consume(&Token::Symbol(Symbol::RightParen))?;
            let then_stmt = self.parse_statement()?;
            if self.match_and_consume(Token::Reserved(Reserved::ELSE)) {
                let else_stmt = self.parse_statement()?;
                let span = span1 | else_stmt.annotation;
                Ok(Statement::If(e, Box::new(then_stmt), Some(Box::new(else_stmt))).annotate(span))
            } else {
                let span = span1 | then_stmt.annotation;
                Ok(Statement::If(e, Box::new(then_stmt), None).annotate(span))
            }
        } else if let Some(span1) = self.match_and_konsume(&Token::Reserved(Reserved::WHILE)) {
            self.consume(&Token::Symbol(Symbol::LeftParen))?;
            let cond = self.parse_expr()?;
            self.consume(&Token::Symbol(Symbol::RightParen))?;
            let body = self.parse_statement()?;
            let span = span1 | body.annotation;
            Ok(Statement::While(cond, Box::new(body)).annotate(span))
        } else if self.match_and_consume(Token::Reserved(Reserved::FOR)) {
            let loopdef = self.parse_for_loop_line()?;
            let body = self.parse_statement()?;
            Ok(for_loop_into_while_loop(loopdef, body))
        } else {
            let e = self.parse_expr()?;
            let span2 = self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            let span = e.annotation | span2;
            Ok(Statement::Expression(e).annotate(span))
        }
    }
    /// This will match the `(var a = 1; a < 10; a = a + 1)` part of a for loop
    fn parse_for_loop_line(
        &mut self,
    ) -> Result<ForLoopDef<'src, &'src str, &'src str, ByteSpan>, ParseError> {
        self.consume(&Token::Symbol(Symbol::LeftParen))?;
        let (var_name, start) = if self.match_and_consume(Token::Symbol(Symbol::SEMICOLON)) {
            (None, None)
        } else if self.match_and_consume(Token::Reserved(Reserved::VAR)) {
            let var_name = self.parse_identifier()?;
            let start = if self.match_and_consume(Token::Symbol(Symbol::EQUAL)) {
                let e = self.parse_expr()?;
                Some(e)
            } else {
                None
            };
            self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            (Some(VariableDecl(var_name)), start)
        } else {
            let e = self.parse_expr()?;
            self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            (None, Some(e))
        };
        let cond = if self.match_and_consume(Token::Symbol(Symbol::SEMICOLON)) {
            None
        } else {
            let e = self.parse_expr()?;
            self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            Some(e)
        };
        let increment = if self.match_and_consume(Token::Symbol(Symbol::RightParen)) {
            None
        } else {
            let e = self.parse_expr()?;
            self.consume(&Token::Symbol(Symbol::RightParen))?;
            Some(e)
        };
        Ok(ForLoopDef {
            var_name,
            start,
            cond,
            increment,
        })
    }
    pub fn parse_program(mut self) -> Result<ParsedProgram<'src>, ParseError> {
        let mut decls = Vec::new();
        while !self.done() {
            decls.push(self.parse_declaration()?);
        }
        Ok(Program { decls })
    }
    pub fn parse_declaration(&mut self) -> Result<ParsedDeclaration<'src>, ParseError> {
        if self.match_and_consume(Token::Reserved(Reserved::VAR)) {
            let name = self.parse_identifier()?;
            let value = if self.match_and_consume(Token::Symbol(Symbol::EQUAL)) {
                let e = self.parse_expr()?;
                Some(e)
            } else {
                None
            };
            self.consume(&Token::Symbol(Symbol::SEMICOLON))?;
            Ok(Declaration::Var(VariableDecl(name), value))
        } else if self.match_and_consume(Token::Reserved(Reserved::FUN)) {
            let name = self.parse_identifier()?;
            let mut args = Vec::new();
            self.consume(&Token::Symbol(Symbol::LeftParen))?;
            if self.match_and_consume(Token::Symbol(Symbol::RightParen)) {
                // no params
            } else {
                loop {
                    let arg_name = self.parse_identifier()?;
                    args.push(VariableDecl(arg_name));
                    if !self.match_and_consume(Token::Symbol(Symbol::COMMA)) {
                        break;
                    }
                }
                self.consume(&Token::Symbol(Symbol::RightParen))?;
            }

            let body = self.parse_block()?;
            Ok(Declaration::Function {
                name: VariableDecl(name),
                args,
                body,
            })
        } else {
            self.parse_statement().map(Declaration::Statement)
        }
    }
    fn parse_block(&mut self) -> Result<ParsedStatement<'src>, ParseError> {
        let span1: ByteSpan = self.consume(&Token::Symbol(Symbol::LeftBrace))?;
        let mut decls = Vec::new();
        loop {
            if let Some(span2) = self.match_and_konsume(&Token::Symbol(Symbol::RightBrace)) {
                return Ok(Statement::Block(decls).annotate(span1 | span2));
            }
            decls.push(self.parse_declaration()?);
        }
    }
    fn peek(&self) -> Option<&Token<'src>> {
        self.remaining.first().map(|x| &x.0)
    }
    pub fn parse_expr(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        self.parse_assignment()
    }
    fn parse_assignment(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let left = self.parse_logic_or()?;

        if self.peek() == Some(&Token::Symbol(Symbol::EQUAL)) {
            self.consume(&Token::Symbol(Symbol::EQUAL))?;
            let right = self.parse_assignment()?;
            let ann = left.annotation | right.annotation;
            match left.value {
                Expression::Identifier(v) => {
                    let e = Expression::Assignment(v, Box::new(right));
                    Ok(e.annotate(ann))
                }
                _ => Err(self.error("LHS of an assignment must be a variable")),
            }
        } else {
            Ok(left)
        }
    }
    fn parse_logic_or(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut left = self.parse_logic_and()?;
        loop {
            if self.match_and_consume(Token::Reserved(Reserved::OR)) {
                let right = self.parse_logic_and()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::OR,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_logic_and(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut left = self.parse_equality()?;
        loop {
            if self.match_and_consume(Token::Reserved(Reserved::AND)) {
                let right = self.parse_equality()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::AND,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_grouping(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        self.consume(&Token::Symbol(Symbol::LeftParen))?;
        let e = self.parse_expr()?;
        self.consume(&Token::Symbol(Symbol::RightParen))?;
        Ok(e)
    }

    fn parse_primary(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let (token, range) = self
            .remaining
            .first()
            .ok_or_else(|| ParseError::UnexpectedEnd {
                src: self.src.to_owned(),
                span: self.src.len().into(),
                help: "Expected parentheses, identifier or literal".to_owned(),
            })?;
        let span = range.clone().into();
        match token {
            Token::Symbol(Symbol::LeftParen) => self.parse_grouping(),
            Token::Identifier(id) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::Identifier(Variable(*id)).annotate(span))
            }
            Token::NumberLiteral(n) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::NumberLiteral(*n).annotate(span))
            }
            Token::StringLiteral(s) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::StringLiteral(s).annotate(span))
            }
            Token::Reserved(Reserved::FALSE) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::BooleanLiteral(false).annotate(span))
            }
            Token::Reserved(Reserved::TRUE) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::BooleanLiteral(true).annotate(span))
            }
            Token::Reserved(Reserved::NIL) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::Nil.annotate(span))
            }
            _ => Err(self.error("Expected parenthesized expression, identifier, or literal")),
        }
    }

    fn parse_unary(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let x = if let Some(span1) = self.match_and_konsume(&Token::Symbol(Symbol::MINUS)) {
            Some((UOperator::MINUS, span1))
        } else if let Some(span1) = self.match_and_konsume(&Token::Symbol(Symbol::BANG)) {
            Some((UOperator::BANG, span1))
        } else {
            None
        };
        match x {
            None => self.parse_call(),
            Some((operator, span1)) => {
                let right = self.parse_unary()?;
                let ann = right.annotation;
                Ok(Expression::Unary {
                    operator,
                    right: Box::new(right),
                }
                .annotate(span1 | ann))
            }
        }
    }
    fn parse_call(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut expr = self.parse_primary()?;
        // Parse any number of calls
        let mut ann = expr.annotation;
        while self.match_and_consume(Token::Symbol(Symbol::LeftParen)) {
            let mut args = vec![];
            if let Some(span2) = self.match_and_konsume(&Token::Symbol(Symbol::RightParen)) {
                ann = ann | span2;
            } else {
                loop {
                    let e = self.parse_expr()?;
                    args.push(e);
                    if !self.match_and_consume(Token::Symbol(Symbol::COMMA)) {
                        break;
                    }
                    if args.len() >= 254 {
                        return Err(self.error("too many arguments"));
                    }
                }
                let span2 = self.consume(&Token::Symbol(Symbol::RightParen))?;
                ann = ann | span2;
            }
            expr = Expression::FunctionCall(Box::new(expr), args).annotate(ann);
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut left = self.parse_unary()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::STAR)) {
                let right = self.parse_unary()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::STAR,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else if self.match_and_consume(Token::Symbol(Symbol::SLASH)) {
                let right = self.parse_unary()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::SLASH,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_term(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut left = self.parse_factor()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::PLUS)) {
                let right = self.parse_factor()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::PLUS,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else if self.match_and_consume(Token::Symbol(Symbol::MINUS)) {
                let right = self.parse_factor()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::MINUS,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_comparison(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut left = self.parse_term()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::GREATER)) {
                let right = self.parse_term()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::GREATER,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else if self.match_and_consume(Token::Symbol(Symbol::GreaterEqual)) {
                let right = self.parse_term()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::GreaterEqual,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else if self.match_and_consume(Token::Symbol(Symbol::LESS)) {
                let right = self.parse_term()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::LESS,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else if self.match_and_consume(Token::Symbol(Symbol::LessEqual)) {
                let right = self.parse_term()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::LessEqual,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_equality(&mut self) -> Result<ParsedExpression<'src>, ParseError> {
        let mut left = self.parse_comparison()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::BangEqual)) {
                let right = self.parse_comparison()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::BangEqual,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else if self.match_and_consume(Token::Symbol(Symbol::EqualEqual)) {
                let right = self.parse_comparison()?;
                let ann = left.annotation | right.annotation;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::EqualEqual,
                    right: Box::new(right),
                }
                .annotate(ann);
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_identifier(&mut self) -> Result<&'src str, ParseError> {
        match self.remaining.split_first() {
            Some(((tk, _), rest)) => {
                self.remaining = rest;
                match tk {
                    Token::Identifier(s) => Ok(s),
                    _ => Err(self.error("expected identifier")),
                }
            }
            None => Err(self.error("end of input")),
        }
    }
    fn consume(&mut self, token: &Token<'src>) -> Result<ByteSpan, ParseError> {
        self.match_and_konsume(token)
            .ok_or_else(|| self.error(&format!("expected another token ...  {:?}", token)))
    }
    fn match_and_consume(&mut self, token: Token<'src>) -> bool {
        let is_match = self.peek() == Some(&token);
        if is_match {
            self.remaining = &self.remaining[1..];
        }
        is_match
    }
    fn match_and_konsume(&mut self, token: &Token<'src>) -> Option<ByteSpan> {
        self.remaining
            .first()
            .filter(|(t, _)| t == token)
            .map(|(_, r)| {
                self.remaining = &self.remaining[1..];
                r.clone().into()
            })
    }
}
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

// tests
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_expr() {
        let mut parser = Parser {
            remaining: &[
                (Token::Reserved(Reserved::FALSE), (0..0)),
                (Token::Symbol(Symbol::LessEqual), (0..0)),
                (Token::NumberLiteral(5.5), (0..0)),
            ],
            src: "",
        };
        let e = parser.parse_expr().unwrap();
        assert_eq!(e.value.pretty_print(), "(LessEqual false 5.5)");
    }
}
