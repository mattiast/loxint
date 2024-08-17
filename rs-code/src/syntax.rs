pub enum Expression {
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
        pretty_print_expression(self)
    }
}
fn pretty_print_expression(expression: &Expression) -> String {
    match expression {
        Expression::StringLiteral(s) => format!("\"{}\"", s),
        Expression::NumberLiteral(n) => format!("{}", n),
        Expression::BooleanLiteral(b) => format!("{}", b),
        Expression::Unary { operator, right } => {
            format!("({:?} {})", operator, pretty_print_expression(right))
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            format!(
                "({:?} {} {})",
                operator,
                pretty_print_expression(left),
                pretty_print_expression(right)
            )
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
        let actual = pretty_print_expression(&expression);
        assert_eq!(actual, expected);

        // an expression with a unary operator and a boolean literal
        let expression = Expression::Unary {
            operator: UOperator::BANG,
            right: Box::new(Expression::BooleanLiteral(true)),
        };
        let expected = "(BANG true)";
        let actual = pretty_print_expression(&expression);
        assert_eq!(actual, expected);
    }
}
