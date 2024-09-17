use crate::{
    scanner::{Reserved, Symbol, Token},
    syntax::{
        BOperator, Declaration, Expression, ForLoopDef, Program, Statement, UOperator, VarName,
        Variable,
    },
};

pub struct Parser<'a, 'b> {
    remaining: &'a [Token<'b>],
}

#[derive(Debug)]
pub enum ParseError {
    Bad,
}

impl<'list, 'src> Parser<'list, 'src>
where
    'src: 'list,
{
    pub fn new(tokens: &'list [Token<'src>]) -> Self {
        Self { remaining: tokens }
    }
    pub fn done(&self) -> bool {
        self.remaining.is_empty()
    }
    pub fn parse_statement(&mut self) -> Result<Statement<'src>, ParseError> {
        if self.match_and_consume(Token::Reserved(Reserved::PRINT)) {
            let e = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
            Ok(Statement::Print(e))
        } else if self.peek() == Some(&Token::Symbol(Symbol::LeftBrace)) {
            self.parse_block()
        } else if self.match_and_consume(Token::Reserved(Reserved::IF)) {
            self.consume(&[Token::Symbol(Symbol::LeftParen)])?;
            let e = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::RightParen)])?;
            let then_stmt = self.parse_statement()?;
            if self.match_and_consume(Token::Reserved(Reserved::ELSE)) {
                let else_stmt = self.parse_statement()?;
                Ok(Statement::If(
                    e,
                    Box::new(then_stmt),
                    Some(Box::new(else_stmt)),
                ))
            } else {
                Ok(Statement::If(e, Box::new(then_stmt), None))
            }
        } else if self.match_and_consume(Token::Reserved(Reserved::WHILE)) {
            self.consume(&[Token::Symbol(Symbol::LeftParen)])?;
            let cond = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::RightParen)])?;
            let body = self.parse_statement()?;
            Ok(Statement::While(cond, Box::new(body)))
        } else if self.match_and_consume(Token::Reserved(Reserved::FOR)) {
            let loopdef = self.parse_for_loop_line()?;
            let body = self.parse_statement()?;
            Ok(Statement::For(loopdef, Box::new(body)))
        } else {
            let e = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
            Ok(Statement::Expression(e))
        }
    }
    /// This will match the `(var a = 1; a < 10; a = a + 1)` part of a for loop
    fn parse_for_loop_line(&mut self) -> Result<ForLoopDef<'src>, ParseError> {
        self.consume(&[Token::Symbol(Symbol::LeftParen)])?;
        let (var_name, start) = if self.match_and_consume(Token::Symbol(Symbol::SEMICOLON)) {
            (None, None)
        } else if self.match_and_consume(Token::Reserved(Reserved::VAR)) {
            let var_name = Some(self.parse_identifier()?);
            let start = if self.match_and_consume(Token::Symbol(Symbol::EQUAL)) {
                let e = self.parse_expr()?;
                Some(e)
            } else {
                None
            };
            self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
            (var_name, start)
        } else {
            let e = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
            (None, Some(e))
        };
        let cond = if self.match_and_consume(Token::Symbol(Symbol::SEMICOLON)) {
            None
        } else {
            let e = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
            Some(e)
        };
        let increment = if self.match_and_consume(Token::Symbol(Symbol::RightParen)) {
            None
        } else {
            let e = self.parse_expr()?;
            self.consume(&[Token::Symbol(Symbol::RightParen)])?;
            Some(e)
        };
        Ok(ForLoopDef {
            var_name,
            start,
            cond,
            increment,
        })
    }
    pub fn parse_program(&mut self) -> Result<Program<'src>, ParseError> {
        let mut decls = Vec::new();
        while !self.done() {
            decls.push(self.parse_declaration()?);
        }
        Ok(Program { decls })
    }
    fn parse_declaration(&mut self) -> Result<Declaration<'src>, ParseError> {
        if self.match_and_consume(Token::Reserved(Reserved::VAR)) {
            let name = self.parse_identifier()?;
            let value = if self.match_and_consume(Token::Symbol(Symbol::EQUAL)) {
                let e = self.parse_expr()?;
                Some(e)
            } else {
                None
            };
            self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
            Ok(Declaration::Var(name, value))
        } else if self.match_and_consume(Token::Reserved(Reserved::FUN)) {
            let name = self.parse_identifier()?;
            let mut args = Vec::new();
            self.consume(&[Token::Symbol(Symbol::LeftParen)])?;
            if self.match_and_consume(Token::Symbol(Symbol::RightParen)) {
                // no params
            } else {
                loop {
                    let arg_name = self.parse_identifier()?;
                    args.push(arg_name);
                    if !self.match_and_consume(Token::Symbol(Symbol::COMMA)) {
                        break;
                    }
                }
                self.consume(&[Token::Symbol(Symbol::RightParen)])?;
            }

            let body = self.parse_block()?;
            Ok(Declaration::Function { name, args, body })
        } else {
            self.parse_statement().map(Declaration::Statement)
        }
    }
    fn parse_block(&mut self) -> Result<Statement<'src>, ParseError> {
        self.consume(&[Token::Symbol(Symbol::LeftBrace)])?;
        let mut decls = Vec::new();
        while !self.match_and_consume(Token::Symbol(Symbol::RightBrace)) {
            decls.push(self.parse_declaration()?);
        }
        Ok(Statement::Block(decls))
    }
    fn peek(&self) -> Option<&Token<'src>> {
        self.remaining.first()
    }
    pub fn parse_expr(&mut self) -> Result<Expression<'src>, ParseError> {
        self.parse_assignment()
    }
    fn parse_assignment(&mut self) -> Result<Expression<'src>, ParseError> {
        let left = self.parse_logic_or()?;

        if self.remaining.first() == Some(&Token::Symbol(Symbol::EQUAL)) {
            self.consume(&[Token::Symbol(Symbol::EQUAL)])?;
            let right = self.parse_assignment()?;
            match left {
                Expression::Identifier(v) => {
                    let e = Expression::Assignment(v, Box::new(right));
                    Ok(e)
                }
                _ => Err(ParseError::Bad), // LHS of an assignment must be a variable
            }
        } else {
            Ok(left)
        }
    }
    fn parse_logic_or(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut left = self.parse_logic_and()?;
        loop {
            if self.match_and_consume(Token::Reserved(Reserved::OR)) {
                let right = self.parse_logic_and()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::OR,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_logic_and(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut left = self.parse_equality()?;
        loop {
            if self.match_and_consume(Token::Reserved(Reserved::AND)) {
                let right = self.parse_equality()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::AND,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_grouping(&mut self) -> Result<Expression<'src>, ParseError> {
        self.consume(&[Token::Symbol(Symbol::LeftParen)])?;
        let e = self.parse_expr()?;
        self.consume(&[Token::Symbol(Symbol::RightParen)])?;
        Ok(e)
    }

    fn parse_primary(&mut self) -> Result<Expression<'src>, ParseError> {
        match self.remaining.first() {
            Some(Token::Symbol(Symbol::LeftParen)) => self.parse_grouping(),
            Some(Token::Identifier(id)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::Identifier(Variable(id)))
            }
            Some(Token::NumberLiteral(n)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::NumberLiteral(*n))
            }
            Some(Token::StringLiteral(s)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::StringLiteral(s))
            }
            Some(Token::Reserved(Reserved::FALSE)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::BooleanLiteral(false))
            }
            Some(Token::Reserved(Reserved::TRUE)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::BooleanLiteral(true))
            }
            Some(Token::Reserved(Reserved::NIL)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::Nil)
            }
            _ => Err(ParseError::Bad), // Unexpected token
        }
    }

    fn parse_unary(&mut self) -> Result<Expression<'src>, ParseError> {
        let operator = if self.match_and_consume(Token::Symbol(Symbol::MINUS)) {
            Some(UOperator::MINUS)
        } else if self.match_and_consume(Token::Symbol(Symbol::BANG)) {
            Some(UOperator::BANG)
        } else {
            None
        };
        match operator {
            None => self.parse_call(),
            Some(operator) => {
                let right = self.parse_unary()?;
                Ok(Expression::Unary {
                    operator,
                    right: Box::new(right),
                })
            }
        }
    }
    fn parse_call(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut expr = self.parse_primary()?;
        // Parse any number of calls
        while self.match_and_consume(Token::Symbol(Symbol::LeftParen)) {
            let mut args = vec![];
            if !self.match_and_consume(Token::Symbol(Symbol::RightParen)) {
                loop {
                    args.push(self.parse_expr()?);
                    if !self.match_and_consume(Token::Symbol(Symbol::COMMA)) {
                        break;
                    }
                }
                self.consume(&[Token::Symbol(Symbol::RightParen)])?;
            }
            if args.len() > 254 {
                return Err(ParseError::Bad);
            }
            expr = Expression::FunctionCall(Box::new(expr), args);
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut left = self.parse_unary()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::STAR)) {
                let right = self.parse_unary()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::STAR,
                    right: Box::new(right),
                };
            } else if self.match_and_consume(Token::Symbol(Symbol::SLASH)) {
                let right = self.parse_unary()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::SLASH,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_term(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut left = self.parse_factor()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::PLUS)) {
                let right = self.parse_factor()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::PLUS,
                    right: Box::new(right),
                };
            } else if self.match_and_consume(Token::Symbol(Symbol::MINUS)) {
                let right = self.parse_factor()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::MINUS,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_comparison(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut left = self.parse_term()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::GREATER)) {
                let right = self.parse_term()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::GREATER,
                    right: Box::new(right),
                };
            } else if self.match_and_consume(Token::Symbol(Symbol::GreaterEqual)) {
                let right = self.parse_term()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::GreaterEqual,
                    right: Box::new(right),
                };
            } else if self.match_and_consume(Token::Symbol(Symbol::LESS)) {
                let right = self.parse_term()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::LESS,
                    right: Box::new(right),
                };
            } else if self.match_and_consume(Token::Symbol(Symbol::LessEqual)) {
                let right = self.parse_term()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::LessEqual,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn parse_equality(&mut self) -> Result<Expression<'src>, ParseError> {
        let mut left = self.parse_comparison()?;
        loop {
            if self.match_and_consume(Token::Symbol(Symbol::BangEqual)) {
                let right = self.parse_comparison()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::BangEqual,
                    right: Box::new(right),
                };
            } else if self.match_and_consume(Token::Symbol(Symbol::EqualEqual)) {
                let right = self.parse_comparison()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: BOperator::EqualEqual,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_identifier(&mut self) -> Result<VarName<'src>, ParseError> {
        match self.remaining.first() {
            Some(Token::Identifier(s)) => {
                self.remaining = &self.remaining[1..];
                Ok(Variable(s))
            }
            _ => Err(ParseError::Bad),
        }
    }
    fn consume(&mut self, string: &[Token<'src>]) -> Result<(), ParseError> {
        let x = self.remaining.starts_with(string);
        if !x {
            Err(ParseError::Bad)
        } else {
            self.remaining = &self.remaining[string.len()..];
            Ok(())
        }
    }
    fn match_and_consume(&mut self, token: Token<'src>) -> bool {
        let is_match = self.remaining.first() == Some(&token);
        if is_match {
            self.remaining = &self.remaining[1..];
        }
        is_match
    }
}

// tests
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_expr() {
        let mut parser = Parser {
            remaining: &[
                Token::Reserved(Reserved::FALSE),
                Token::Symbol(Symbol::LessEqual),
                Token::NumberLiteral(5.5),
            ],
        };
        let e = parser.parse_expr().unwrap();
        assert_eq!(e.pretty_print(), "(LessEqual false 5.5)");
    }
}
