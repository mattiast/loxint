#[derive(Debug, Clone)]
pub enum Expression<'a, Var, Ann> {
    Nil,
    StringLiteral(&'a str),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    Identifier(Variable<Var>),
    Unary {
        operator: UOperator,
        right: Box<AnnotatedExpression<'a, Var, Ann>>,
    },
    Binary {
        left: Box<AnnotatedExpression<'a, Var, Ann>>,
        operator: BOperator,
        right: Box<AnnotatedExpression<'a, Var, Ann>>,
    },
    Assignment(Variable<Var>, Box<AnnotatedExpression<'a, Var, Ann>>),
    FunctionCall(
        Box<AnnotatedExpression<'a, Var, Ann>>,
        Vec<AnnotatedExpression<'a, Var, Ann>>,
    ),
}
pub type AnnotatedExpression<'a, Var, Ann> = Annotated<Expression<'a, Var, Ann>, Ann>;
#[derive(Debug, Clone)]
pub struct Annotated<T, A> {
    pub value: T,
    pub annotation: A,
}
// TODO Annotation also for Declaration?
#[derive(Debug, Clone)]
pub enum Statement<'a, VR, VD, Ann> {
    Expression(AnnotatedExpression<'a, VR, Ann>),
    Print(AnnotatedExpression<'a, VR, Ann>),
    Block(Vec<Declaration<'a, VR, VD, Ann>>),
    If(
        AnnotatedExpression<'a, VR, Ann>,
        Box<AnnotatedStatement<'a, VR, VD, Ann>>,
        Option<Box<AnnotatedStatement<'a, VR, VD, Ann>>>,
    ),
    While(
        AnnotatedExpression<'a, VR, Ann>,
        Box<AnnotatedStatement<'a, VR, VD, Ann>>,
    ),
    Return(AnnotatedExpression<'a, VR, Ann>),
}
pub type AnnotatedStatement<'a, VR, VD, Ann> = Annotated<Statement<'a, VR, VD, Ann>, Ann>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Variable<Var>(pub Var);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariableDecl<Var>(pub Var);

#[derive(Debug, Clone)]
pub enum Declaration<'a, VR, VD, Ann> {
    Var(VariableDecl<VD>, Option<AnnotatedExpression<'a, VR, Ann>>),
    Function {
        name: VariableDecl<VD>,
        args: Vec<VariableDecl<VD>>,
        body: AnnotatedStatement<'a, VR, VD, Ann>,
    },
    Statement(AnnotatedStatement<'a, VR, VD, Ann>),
}

#[derive(Debug, Clone)]
pub struct Program<'a, VR, VD, Ann> {
    pub decls: Vec<Declaration<'a, VR, VD, Ann>>,
}

#[derive(Debug, Clone)]
pub enum UOperator {
    MINUS,
    BANG,
}

#[derive(Debug, Clone)]
pub enum BOperator {
    PLUS,
    MINUS,
    STAR,
    SLASH,

    AND,
    OR,

    BangEqual,
    EqualEqual,
    LESS,
    GREATER,
    LessEqual,
    GreaterEqual,
}

impl<'a, Var, Ann> Expression<'a, Var, Ann> {
    pub fn annotate(self, annotation: Ann) -> Annotated<Self, Ann> {
        Annotated {
            value: self,
            annotation,
        }
    }
}
impl<'a, VR, VD, Ann> Statement<'a, VR, VD, Ann> {
    pub fn annotate(self, annotation: Ann) -> Annotated<Self, Ann> {
        Annotated {
            value: self,
            annotation,
        }
    }
}
impl<'a, Ann> Expression<'a, &'a str, Ann> {
    pub fn pretty_print(&self) -> String {
        match self {
            Expression::Nil => "nil".to_string(),
            Expression::StringLiteral(s) => format!("\"{}\"", s),
            Expression::NumberLiteral(n) => format!("{}", n),
            Expression::BooleanLiteral(b) => format!("{}", b),
            Expression::Identifier(Variable(s)) => format!("{}", s),
            Expression::Unary { operator, right } => {
                format!("({:?} {})", operator, right.value.pretty_print())
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                format!(
                    "({:?} {} {})",
                    operator,
                    left.value.pretty_print(),
                    right.value.pretty_print(),
                )
            }
            Expression::Assignment(Variable(s), e) => {
                format!("(SET {} {})", s, e.value.pretty_print())
            }
            Expression::FunctionCall(e, args) => {
                format!(
                    "(CALL {} {})",
                    e.value.pretty_print(),
                    args.iter()
                        .map(|arg| arg.value.pretty_print())
                        .collect::<Vec<_>>()
                        .join(" ")
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
            left: Box::new(Expression::NumberLiteral(1.0).annotate(())),
            operator: BOperator::PLUS,
            right: Box::new(Expression::NumberLiteral(2.5).annotate(())),
        };
        let expected = "(PLUS 1 2.5)";
        let actual = expression.pretty_print();
        assert_eq!(actual, expected);

        // an expression with a unary operator and a boolean literal
        let expression = Expression::Unary {
            operator: UOperator::BANG,
            right: Box::new(Expression::BooleanLiteral(true).annotate(())),
        };
        let expected = "(BANG true)";
        let actual = expression.pretty_print();
        assert_eq!(actual, expected);
    }
}
