#[derive(Debug)]
pub enum Expression {
    Nil,
    StringLiteral(String),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    Unary {
        operator: UOperator,
        right: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BOperator,
        right: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum UOperator {
    MINUS,
    BANG,
}

#[derive(Debug)]
pub enum BOperator {
    PLUS,
    MINUS,
    STAR,
    SLASH,

    BangEqual,
    EqualEqual,
    LESS,
    GREATER,
    LessEqual,
    GreaterEqual,
}

impl Expression {
    pub fn pretty_print(&self) -> String {
        match self {
            Expression::Nil => "nil".to_string(),
            Expression::StringLiteral(s) => format!("\"{}\"", s),
            Expression::NumberLiteral(n) => format!("{}", n),
            Expression::BooleanLiteral(b) => format!("{}", b),
            Expression::Unary { operator, right } => {
                format!("({:?} {})", operator, right.pretty_print())
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                format!(
                    "({:?} {} {})",
                    operator,
                    left.pretty_print(),
                    right.pretty_print(),
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_pretty_print_expression() {
        // an expression with a binary operator and two number literals
        let expression = Expression::Binary {
            left: Box::new(Expression::NumberLiteral(1.0)),
            operator: BOperator::PLUS,
            right: Box::new(Expression::NumberLiteral(2.5)),
        };
        let expected = "(PLUS 1 2.5)";
        let actual = expression.pretty_print();
        assert_eq!(actual, expected);

        // an expression with a unary operator and a boolean literal
        let expression = Expression::Unary {
            operator: UOperator::BANG,
            right: Box::new(Expression::BooleanLiteral(true)),
        };
        let expected = "(BANG true)";
        let actual = expression.pretty_print();
        assert_eq!(actual, expected);
    }
}
