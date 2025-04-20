use crate::syntax::{Annotated, BOperator, Expression};
use chumsky::prelude::*;

// Precedence levels for operators
const PRECEDENCE: [(BOperator, i32); 4] = [
    (BOperator::PLUS, 1),
    (BOperator::MINUS, 1),
    (BOperator::STAR, 2),
    (BOperator::SLASH, 2),
];

fn parse_bop<'src>() -> impl Parser<'src, &'src str, BOperator> + Clone {
    choice((
        just('+').to(BOperator::PLUS),
        just('-').to(BOperator::MINUS),
        just('*').to(BOperator::STAR),
        just('/').to(BOperator::SLASH),
    ))
}

fn parse_number<'src>() -> impl Parser<'src, &'src str, Expression<'src, &'src str, ()>> + Clone {
    text::digits(10)
        .to_slice()
        .map(|whole: &'src str| Expression::NumberLiteral(whole.parse::<f64>().unwrap()))
}

#[test]
fn test_parse_bop() {
    let parser = parse_bop();
    assert_eq!(parser.parse("+").unwrap(), BOperator::PLUS);
    assert_eq!(parser.parse("-").unwrap(), BOperator::MINUS);
    assert_eq!(parser.parse("*").unwrap(), BOperator::STAR);
    assert_eq!(parser.parse("/").unwrap(), BOperator::SLASH);
}

#[test]
fn test_parse_number() {
    let parser = parse_number();
    let result = parser.parse("123").unwrap();
    match result {
        Expression::NumberLiteral(n) => assert_eq!(n, 123.0),
        _ => panic!("Expected NumberLiteral"),
    }
}
