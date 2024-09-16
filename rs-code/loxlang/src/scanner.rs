use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1},
    combinator::{map, map_res, opt, recognize},
    sequence::tuple,
    IResult,
};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Reserved(Reserved),
    Symbol(Symbol),
    NumberLiteral(f64),
    StringLiteral(&'a str),
}
#[derive(Debug, PartialEq)]
pub enum Symbol {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BangEqual,
    BANG,
    EqualEqual,
    EQUAL,
    GreaterEqual,
    GREATER,
    LessEqual,
    LESS,
}

#[derive(Debug, PartialEq)]
pub enum Reserved {
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
fn is_letter_or_underscore(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_letter_digit_or_underscore(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Parses an identifier from the input string.
/// Identifiers can start with a letter or underscore, and can contain letters, digits, and underscores.
fn parse_identifier(input: &str) -> IResult<&str, &str> {
    // TODO this is kind of silly
    let first_char = take_while1(is_letter_or_underscore);

    recognize(tuple((
        first_char,
        take_while(is_letter_digit_or_underscore),
    )))(input)
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
        map(tag("("), |_| Symbol::LeftParen),
        map(tag(")"), |_| Symbol::RightParen),
        map(tag("{"), |_| Symbol::LeftBrace),
        map(tag("}"), |_| Symbol::RightBrace),
        map(tag(","), |_| Symbol::COMMA),
        map(tag("."), |_| Symbol::DOT),
        map(tag("-"), |_| Symbol::MINUS),
        map(tag("+"), |_| Symbol::PLUS),
        map(tag(";"), |_| Symbol::SEMICOLON),
        map(tag("/"), |_| Symbol::SLASH),
        map(tag("*"), |_| Symbol::STAR),
        map(tag("!="), |_| Symbol::BangEqual),
        map(tag("!"), |_| Symbol::BANG),
        map(tag("=="), |_| Symbol::EqualEqual),
        map(tag("="), |_| Symbol::EQUAL),
        map(tag(">="), |_| Symbol::GreaterEqual),
        map(tag(">"), |_| Symbol::GREATER),
        map(tag("<="), |_| Symbol::LessEqual),
        map(tag("<"), |_| Symbol::LESS),
    ))(input)
}

fn parse_string_literal(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('"')(input)?;
    let (input, string) = take_while(|c| c != '"')(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, string))
}
fn parse_number(input: &str) -> IResult<&str, f64> {
    let parse_number = recognize(tuple((digit1, opt(tuple((char('.'), digit1))))));
    map_res(parse_number, |s: &str| s.parse::<f64>())(input)
}
fn parse_token(input: &str) -> IResult<&str, Token> {
    alt((
        parse_identifier_or_reserved,
        map(parse_symbol, Token::Symbol),
        map(parse_number, Token::NumberLiteral),
        map(parse_string_literal, Token::StringLiteral),
    ))(input)
}

pub fn parse_tokens(input: &str) -> IResult<&str, Vec<Token>> {
    // TODO change the error type to something that makes sense
    let mut tokens = Vec::new();
    let mut input = input.trim_start();
    while !input.is_empty() {
        let (remaining, token) = parse_token(input)?;
        tokens.push(token);
        input = remaining.trim_start();
    }
    Ok((input, tokens))
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
        let expected = Ok(("", Symbol::LeftParen));
        let actual = parse_symbol(input);
        assert_eq!(actual, expected);

        let input = "!=";
        let expected = Ok(("", Symbol::BangEqual));
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

    #[test]
    fn test_parse_string_literal() {
        let input = "\"hello\"+3";
        let expected = Ok(("+3", "hello"));
        let actual = parse_string_literal(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_multiple_tokens() {
        let input = "123.456 \"hello\"+3";
        let expected = Ok((
            "",
            vec![
                Token::NumberLiteral(123.456),
                Token::StringLiteral("hello"),
                Token::Symbol(Symbol::PLUS),
                Token::NumberLiteral(3.),
            ],
        ));
        let actual = parse_tokens(input);
        assert_eq!(actual, expected);
    }
}
