enum Expression {
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

enum UOperator {
    MINUS,
    BANG,
}

enum BOperator {
    PLUS,
    MINUS,
    STAR,
    SLASH,

    BANG_EQUAL,
    EQUAL_EQUAL,
    LESS,
    GREATER,
    LESS_EQUAL,
    GREATER_EQUAL,
}
