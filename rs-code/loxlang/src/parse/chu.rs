use crate::syntax::BOperator;
use chumsky::prelude::*;

fn parse_bop<'src>() -> impl Parser<'src, &'src str, BOperator> {
    choice((
        // All of the basic instructions are just single characters
        just('+').to(BOperator::PLUS),
        just('-').to(BOperator::MINUS),
        just('*').to(BOperator::STAR),
        just('/').to(BOperator::SLASH),
    ))
}

#[test]
fn test_parse_bop() {
    let parser = parse_bop();
    let r = parser.parse("hello");
    assert_eq!(parser.parse("+").unwrap(), BOperator::PLUS);
    assert_eq!(parser.parse("-").unwrap(), BOperator::MINUS);
    assert_eq!(parser.parse("*").unwrap(), BOperator::STAR);
    assert_eq!(parser.parse("/").unwrap(), BOperator::SLASH);
}
