use nom::combinator::map;
use nom::{branch::alt, bytes::complete::tag, IResult};
fn main() {
    println!("Hello, world!");
}

enum Token<'a> {
    Identifier(&'a str),
    Symbol(Symbol),
    NumberLiteral(f64),
    BooleanLiteral(bool),
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

/// Parses an identifier from the input string.
/// Identifiers can start with a letter or underscore, and can contain letters, digits, and underscores.
fn parse_identifier(input: &str) -> IResult<&str, &str> {
    nom::character::complete::alphanumeric1(input)
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

fn parse_token(input: &str) -> IResult<&str, Token> {
    let input = input.trim_start();
    alt((
        map(parse_identifier, Token::Identifier),
        map(parse_symbol, Token::Symbol),
        // map(nom::character::complete::digit1, Token::NumberLiteral),
        // map(nom::character::complete::bool, Token::BooleanLiteral),
        // map(nom::character::complete::is_a("\"\""), Token::StringLiteral),
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
}
