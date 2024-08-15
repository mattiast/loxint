use nom::character::complete::digit1;
use nom::combinator::map;
use nom::{branch::alt, bytes::complete::tag, IResult};
use nom::{
    character::complete::char,
    combinator::{map_res, opt, recognize},
    sequence::tuple,
};
fn main() {
    println!("Hello, world!");
}

enum Token<'a> {
    Identifier(&'a str),
    Reserved(Reserved),
    Symbol(Symbol),
    NumberLiteral(f64),
    StringLiteral(&'a str),
}
#[derive(Debug, PartialEq)]
enum Symbol {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG_EQUAL,
    BANG,
    EQUAL_EQUAL,
    EQUAL,
    GREATER_EQUAL,
    GREATER,
    LESS_EQUAL,
    LESS,
}

#[derive(Debug, PartialEq)]
enum Reserved {
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
}
/// Parses an identifier from the input string.
/// Identifiers can start with a letter or underscore, and can contain letters, digits, and underscores.
fn parse_identifier(input: &str) -> IResult<&str, &str> {
    nom::character::complete::alphanumeric1(input)
}

fn parse_identifier_or_reserved(input: &str) -> IResult<&str, Token> {
    let (input, identifier) = parse_identifier(input)?;
    let reserved = match identifier {
        "and" => Reserved::AND,
        "class" => Reserved::CLASS,
        "else" => Reserved::ELSE,
        "false" => Reserved::FALSE,
        "fun" => Reserved::FUN,
        "for" => Reserved::FOR,
        "if" => Reserved::IF,
        "nil" => Reserved::NIL,
        "or" => Reserved::OR,
        "print" => Reserved::PRINT,
        "return" => Reserved::RETURN,
        "super" => Reserved::SUPER,
        "this" => Reserved::THIS,
        "true" => Reserved::TRUE,
        "var" => Reserved::VAR,
        "while" => Reserved::WHILE,
        _ => return Ok((input, Token::Identifier(identifier))),
    };
    Ok((input, Token::Reserved(reserved)))
}

fn parse_symbol(input: &str) -> IResult<&str, Symbol> {
    alt((
        map(tag("("), |_| Symbol::LEFT_PAREN),
        map(tag(")"), |_| Symbol::RIGHT_PAREN),
        map(tag("{"), |_| Symbol::LEFT_BRACE),
        map(tag("}"), |_| Symbol::RIGHT_BRACE),
        map(tag(","), |_| Symbol::COMMA),
        map(tag("."), |_| Symbol::DOT),
        map(tag("-"), |_| Symbol::MINUS),
        map(tag("+"), |_| Symbol::PLUS),
        map(tag(";"), |_| Symbol::SEMICOLON),
        map(tag("/"), |_| Symbol::SLASH),
        map(tag("*"), |_| Symbol::STAR),
        map(tag("!="), |_| Symbol::BANG_EQUAL),
        map(tag("!"), |_| Symbol::BANG),
        map(tag("=="), |_| Symbol::EQUAL_EQUAL),
        map(tag("="), |_| Symbol::EQUAL),
        map(tag(">="), |_| Symbol::GREATER_EQUAL),
        map(tag(">"), |_| Symbol::GREATER),
        map(tag("<="), |_| Symbol::LESS_EQUAL),
        map(tag("<"), |_| Symbol::LESS),
    ))(input)
}

fn parse_number(input: &str) -> IResult<&str, f64> {
    let parse_number = recognize(tuple((digit1, opt(tuple((char('.'), digit1))))));
    map_res(parse_number, |s: &str| s.parse::<f64>())(input)
}
fn parse_token(input: &str) -> IResult<&str, Token> {
    let input = input.trim_start();
    alt((
        parse_identifier_or_reserved,
        map(parse_symbol, Token::Symbol),
        map(parse_number, Token::NumberLiteral),
        // map(<parse string literal>, Token::StringLiteral),
    ))(input)
}

// tests
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_identifier() {
        let input = "hello";
        let expected = Ok(("", "hello"));
        let actual = parse_identifier(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_symbol() {
        let input = "(";
        let expected = Ok(("", Symbol::LEFT_PAREN));
        let actual = parse_symbol(input);
        assert_eq!(actual, expected);

        let input = "!=";
        let expected = Ok(("", Symbol::BANG_EQUAL));
        let actual = parse_symbol(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_number() {
        let input = "123.456";
        let expected = Ok(("", 123.456));
        let actual = parse_number(input);
        assert_eq!(actual, expected);
    }
}
