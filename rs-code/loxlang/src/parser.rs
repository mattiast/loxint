use crate::{
    scanner::{Reserved, Symbol, Token},
    syntax::{BOperator, Expression, Statement, UOperator},
};

pub struct Parser<'a, 'b> {
    remaining: &'a [Token<'b>],
}

#[derive(Debug)]
pub enum ParseError {
    Bad,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'a [Token<'b>]) -> Self {
        Self { remaining: tokens }
    }
    pub fn done(&self) -> bool {
        self.remaining.is_empty()
    }
    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.remaining.first() {
            Some(Token::Reserved(Reserved::PRINT)) => {
                self.remaining = &self.remaining[1..];
                let e = self.parse_expr()?;
                self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
                Ok(Statement::Print(e))
            }
            _ => {
                let e = self.parse_expr()?;
                self.consume(&[Token::Symbol(Symbol::SEMICOLON)])?;
                Ok(Statement::Expression(e))
            }
        }
    }
    pub fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        self.parse_equality()
    }

    fn parse_grouping(&mut self) -> Result<Expression, ParseError> {
        self.consume(&[Token::Symbol(Symbol::LeftParen)])?;
        let e = self.parse_expr()?;
        self.consume(&[Token::Symbol(Symbol::RightParen)])?;
        Ok(e)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.remaining.first() {
            Some(Token::Symbol(Symbol::LeftParen)) => self.parse_grouping(),
            Some(Token::NumberLiteral(n)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::NumberLiteral(*n))
            }
            Some(Token::StringLiteral(s)) => {
                self.remaining = &self.remaining[1..];
                Ok(Expression::StringLiteral(s.to_string()))
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

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        let operator = match self.remaining.first() {
            Some(Token::Symbol(Symbol::MINUS)) => {
                self.remaining = &self.remaining[1..];
                Some(UOperator::MINUS)
            }
            Some(Token::Symbol(Symbol::BANG)) => {
                self.remaining = &self.remaining[1..];
                Some(UOperator::BANG)
            }
            _ => None,
        };
        match operator {
            None => self.parse_primary(),
            Some(operator) => {
                let right = self.parse_unary()?;
                Ok(Expression::Unary {
                    operator,
                    right: Box::new(right),
                })
            }
        }
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_unary()?;
        loop {
            match self.remaining.first() {
                Some(Token::Symbol(Symbol::STAR)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_unary()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::STAR,
                        right: Box::new(right),
                    };
                }
                Some(Token::Symbol(Symbol::SLASH)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_unary()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::SLASH,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(left)
    }
    fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_factor()?;
        loop {
            match self.remaining.first() {
                Some(Token::Symbol(Symbol::PLUS)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_factor()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::PLUS,
                        right: Box::new(right),
                    };
                }
                Some(Token::Symbol(Symbol::MINUS)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_factor()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::MINUS,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(left)
    }
    fn parse_comparison(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_term()?;
        loop {
            match self.remaining.first() {
                Some(Token::Symbol(Symbol::GREATER)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_term()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::GREATER,
                        right: Box::new(right),
                    };
                }
                Some(Token::Symbol(Symbol::GreaterEqual)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_term()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::GreaterEqual,
                        right: Box::new(right),
                    };
                }
                Some(Token::Symbol(Symbol::LESS)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_term()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::LESS,
                        right: Box::new(right),
                    };
                }
                Some(Token::Symbol(Symbol::LessEqual)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_term()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::LessEqual,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(left)
    }
    fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_comparison()?;
        loop {
            match self.remaining.first() {
                Some(Token::Symbol(Symbol::BangEqual)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_comparison()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::BangEqual,
                        right: Box::new(right),
                    };
                }
                Some(Token::Symbol(Symbol::EqualEqual)) => {
                    self.remaining = &self.remaining[1..];
                    let right = self.parse_comparison()?;
                    left = Expression::Binary {
                        left: Box::new(left),
                        operator: BOperator::EqualEqual,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(left)
    }
    fn consume(&mut self, string: &[Token<'b>]) -> Result<(), ParseError> {
        let x = self.remaining.starts_with(string);
        if !x {
            Err(ParseError::Bad)
        } else {
            self.remaining = &self.remaining[string.len()..];
            Ok(())
        }
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
