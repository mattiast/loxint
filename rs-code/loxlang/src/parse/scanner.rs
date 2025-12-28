use std::ops::Range;

use miette::{Diagnostic, SourceOffset};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::char,
    combinator::recognize,
    number::complete::double,
    IResult, Parser,
};
use thiserror::Error;

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
    // this will match one or more non-digits, then digits are allowed
    let first_char = take_while1(is_letter_or_underscore);

    recognize((first_char, take_while(is_letter_digit_or_underscore))).parse(input)
}

fn parse_identifier_or_reserved(input: &str) -> IResult<&str, Token<'_>> {
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
        tag("!=").map(|_| Symbol::BangEqual),
        tag("==").map(|_| Symbol::EqualEqual),
        tag(">=").map(|_| Symbol::GreaterEqual),
        tag("<=").map(|_| Symbol::LessEqual),
        tag("(").map(|_| Symbol::LeftParen),
        tag(")").map(|_| Symbol::RightParen),
        tag("{").map(|_| Symbol::LeftBrace),
        tag("}").map(|_| Symbol::RightBrace),
        tag(",").map(|_| Symbol::COMMA),
        tag(".").map(|_| Symbol::DOT),
        tag("-").map(|_| Symbol::MINUS),
        tag("+").map(|_| Symbol::PLUS),
        tag(";").map(|_| Symbol::SEMICOLON),
        tag("/").map(|_| Symbol::SLASH),
        tag("*").map(|_| Symbol::STAR),
        tag("!").map(|_| Symbol::BANG),
        tag("=").map(|_| Symbol::EQUAL),
        tag(">").map(|_| Symbol::GREATER),
        tag("<").map(|_| Symbol::LESS),
    ))
    .parse(input)
}
fn parse_string_literal(input: &str) -> IResult<&str, &str> {
    (char('"'), take_while(|c| c != '"'), char('"'))
        .map(|(_, string, _)| string)
        .parse(input)
}
fn parse_token<'a>(input: &'a str) -> IResult<&'a str, Token<'a>> {
    alt((
        parse_identifier_or_reserved,
        parse_symbol.map(|sym| Token::Symbol(sym)),
        double.map(|num| Token::NumberLiteral(num)),
        parse_string_literal.map(|lit| Token::StringLiteral(lit)),
    ))
    .parse(input)
}

#[derive(Error, Debug, Diagnostic)]
#[error("lexical_error")]
pub struct LexicalError {
    #[source_code]
    pub src: String,
    #[label("No valid token here")]
    pub source_offset: SourceOffset,
}
pub fn parse_tokens(input: &str) -> Result<Vec<(Token<'_>, Range<usize>)>, LexicalError> {
    let mut tokens = Vec::new();
    let start = input;
    let mut input = input.trim_start();
    while !input.is_empty() {
        let result = parse_token(input);
        match result {
            Ok((remaining, token)) => {
                let range = Range {
                    start: (input.as_ptr() as usize) - (start.as_ptr() as usize),
                    end: (remaining.as_ptr() as usize) - (start.as_ptr() as usize),
                };
                tokens.push((token, range));
                input = remaining.trim_start();
            }
            Err(_) => {
                let location = (input.as_ptr() as usize) - (start.as_ptr() as usize);
                return Err(LexicalError {
                    src: start.to_owned(),
                    source_offset: location.into(),
                });
            }
        }
    }
    Ok(tokens)
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
    fn test_parse_string_literal() {
        let input = "\"hello\"+3";
        let expected = Ok(("+3", "hello"));
        let actual = parse_string_literal(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_multiple_tokens() {
        let input = "123.456 \"hello\"+3";
        let expected = vec![
            Token::NumberLiteral(123.456),
            Token::StringLiteral("hello"),
            Token::Symbol(Symbol::PLUS),
            Token::NumberLiteral(3.),
        ];
        let actual = parse_tokens(input)
            .unwrap()
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
    #[test]
    fn test_error() {
        let input = "123.456 \"hello+3";
        let actual = parse_tokens(input).unwrap_err();
        assert_eq!(actual.source_offset.offset(), 8);
    }
}
