#[derive(Debug)]
pub enum Statement<'a> {
    Expression(Expression<'a>),
    Print(Expression<'a>),
    Block(Vec<Declaration<'a>>),
    If(
        Expression<'a>,
        Box<Statement<'a>>,
        Option<Box<Statement<'a>>>,
    ),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VarName<'a>(pub &'a str);

#[derive(Debug)]
pub enum Declaration<'a> {
    Var(VarName<'a>, Option<Expression<'a>>),
    Statement(Statement<'a>),
}

pub struct Program<'a> {
    pub decls: Vec<Declaration<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Nil,
    StringLiteral(String),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    Identifier(VarName<'a>),
    Unary {
        operator: UOperator,
        right: Box<Expression<'a>>,
    },
    Binary {
        left: Box<Expression<'a>>,
        operator: BOperator,
        right: Box<Expression<'a>>,
    },
    Assignment(VarName<'a>, Box<Expression<'a>>),
    // TODO Supposedly "grouping" node will be needed for LHS of assignment operation
    // TODO Should there be some link to where this was defined in the source?
    // Generic annotation for each node?
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

impl<'a> Expression<'a> {
    pub fn pretty_print(&self) -> String {
        match self {
            Expression::Nil => "nil".to_string(),
            Expression::StringLiteral(s) => format!("\"{}\"", s),
            Expression::NumberLiteral(n) => format!("{}", n),
            Expression::BooleanLiteral(b) => format!("{}", b),
            Expression::Identifier(VarName(s)) => format!("{}", s),
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
            Expression::Assignment(VarName(s), e) => format!("(SET {} {})", s, e.pretty_print()),
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
