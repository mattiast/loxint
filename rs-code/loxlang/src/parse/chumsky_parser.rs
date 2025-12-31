use chumsky::prelude::*;
use crate::syntax::*;
use crate::parse::ByteSpan;

/// Simple example parser using chumsky
/// This starts with basic expressions: literals, binary operations, and parentheses

type ParsedExpression<'src> = AnnotatedExpression<'src, &'src str, ByteSpan>;

/// Token type for our lexer
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Number(f64),
    String(&'a str),
    True,
    False,
    Nil,
    Ident(&'a str),

    // Keywords
    Print,
    Var,
    If,
    Else,
    While,
    For,
    Fun,
    Return,
    And,
    Or,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Comma,

    // Comparison
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

/// Lexer that converts source code into tokens
pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<(Token<'a>, SimpleSpan)>, extra::Err<Simple<'a, char>>> {
    let number = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .map(|s: &str| Token::Number(s.parse().unwrap()))
        .labelled("number");

    let string = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(Token::String)
        .labelled("string");

    let operator = choice((
        just("==").to(Token::EqualEqual),
        just("!=").to(Token::BangEqual),
        just("<=").to(Token::LessEqual),
        just(">=").to(Token::GreaterEqual),
        just('=').to(Token::Equal),
        just('!').to(Token::Bang),
        just('<').to(Token::Less),
        just('>').to(Token::Greater),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
    ));

    let delimiter = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
    ));

    let keyword = text::ascii::ident().map(|s: &str| match s {
        "true" => Token::True,
        "false" => Token::False,
        "nil" => Token::Nil,
        "print" => Token::Print,
        "var" => Token::Var,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "for" => Token::For,
        "fun" => Token::Fun,
        "return" => Token::Return,
        "and" => Token::And,
        "or" => Token::Or,
        _ => Token::Ident(s),
    });

    let token = choice((
        number,
        string,
        operator,
        delimiter,
        keyword,
    ))
    .padded_by(text::whitespace())
    .map_with(|tok, e| (tok, e.span()));

    token.repeated().collect().then_ignore(end())
}

/// Helper to convert SimpleSpan to ByteSpan
fn to_byte_span(span: SimpleSpan) -> ByteSpan {
    ByteSpan {
        start: span.start,
        end: span.end,
    }
}

/// Parser that converts tokens into AST expressions
pub fn expr_parser<'a, 'src: 'a>() -> impl Parser<
    'a,
    &'a [(Token<'src>, SimpleSpan)],
    ParsedExpression<'src>,
    extra::Err<Simple<'a, (Token<'src>, SimpleSpan)>>,
> + Clone {
    recursive(|expr| {
        // Primary expressions: literals and parenthesized expressions
        let literal = select! {
            (Token::Number(n), span) => Expression::NumberLiteral(n).annotate(to_byte_span(span)),
            (Token::String(s), span) => Expression::StringLiteral(s).annotate(to_byte_span(span)),
            (Token::True, span) => Expression::BooleanLiteral(true).annotate(to_byte_span(span)),
            (Token::False, span) => Expression::BooleanLiteral(false).annotate(to_byte_span(span)),
            (Token::Nil, span) => Expression::Nil.annotate(to_byte_span(span)),
        }
        .labelled("literal");

        let ident = select! {
            (Token::Ident(s), span) => (s, span),
        }
        .map(|(name, span)| {
            Expression::Identifier(Variable(name)).annotate(to_byte_span(span))
        })
        .labelled("identifier");

        let lparen = select! { (Token::LParen, _) => () };
        let rparen = select! { (Token::RParen, _) => () };
        let comma = select! { (Token::Comma, _) => () };

        let atom = choice((
            literal,
            ident,
            expr.clone()
                .delimited_by(lparen.clone(), rparen.clone())
                .labelled("parenthesized expression"),
        ));

        // Function calls: foo(arg1, arg2, ...)
        let arg_list = expr
            .clone()
            .separated_by(comma)
            .collect::<Vec<_>>()
            .delimited_by(lparen, rparen);

        let call = atom
            .clone()
            .foldl(arg_list.repeated(), |func, args| {
                let start = func.annotation.start;
                let end = func.annotation.end;
                Expression::FunctionCall(Box::new(func), args)
                    .annotate(ByteSpan { start, end })
            })
            .boxed();

        // Unary: -expr, !expr
        let unary_op = select! {
            (Token::Minus, _) => UOperator::MINUS,
            (Token::Bang, _) => UOperator::BANG,
        };

        let unary = unary_op
            .then(call.clone())
            .map(|(op, right)| {
                let start = right.annotation.start;
                let end = right.annotation.end;
                Expression::Unary {
                    operator: op,
                    right: Box::new(right),
                }
                .annotate(ByteSpan { start, end })
            })
            .or(call)
            .boxed();

        // Factor: * /
        let factor_op = select! {
            (Token::Star, _) => BOperator::STAR,
            (Token::Slash, _) => BOperator::SLASH,
        };

        let factor = unary
            .clone()
            .foldl(
                factor_op.then(unary).repeated(),
                |left, (op, right)| {
                    let start = left.annotation.start;
                    let end = right.annotation.end;
                    Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    }
                    .annotate(ByteSpan { start, end })
                },
            )
            .boxed();

        // Term: + -
        let term_op = select! {
            (Token::Plus, _) => BOperator::PLUS,
            (Token::Minus, _) => BOperator::MINUS,
        };

        let term = factor
            .clone()
            .foldl(
                term_op.then(factor).repeated(),
                |left, (op, right)| {
                    let start = left.annotation.start;
                    let end = right.annotation.end;
                    Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    }
                    .annotate(ByteSpan { start, end })
                },
            )
            .boxed();

        // Comparison: < > <= >=
        let comparison_op = select! {
            (Token::Less, _) => BOperator::LESS,
            (Token::Greater, _) => BOperator::GREATER,
            (Token::LessEqual, _) => BOperator::LessEqual,
            (Token::GreaterEqual, _) => BOperator::GreaterEqual,
        };

        let comparison = term
            .clone()
            .foldl(
                comparison_op.then(term).repeated(),
                |left, (op, right)| {
                    let start = left.annotation.start;
                    let end = right.annotation.end;
                    Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    }
                    .annotate(ByteSpan { start, end })
                },
            )
            .boxed();

        // Equality: == !=
        let equality_op = select! {
            (Token::EqualEqual, _) => BOperator::EqualEqual,
            (Token::BangEqual, _) => BOperator::BangEqual,
        };

        let equality = comparison
            .clone()
            .foldl(
                equality_op.then(comparison).repeated(),
                |left, (op, right)| {
                    let start = left.annotation.start;
                    let end = right.annotation.end;
                    Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    }
                    .annotate(ByteSpan { start, end })
                },
            )
            .boxed();

        // Logical and
        let logic_and_op = select! {
            (Token::And, _) => BOperator::AND,
        };

        let logic_and = equality
            .clone()
            .foldl(
                logic_and_op.then(equality).repeated(),
                |left, (op, right)| {
                    let start = left.annotation.start;
                    let end = right.annotation.end;
                    Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    }
                    .annotate(ByteSpan { start, end })
                },
            )
            .boxed();

        // Logical or
        let logic_or_op = select! {
            (Token::Or, _) => BOperator::OR,
        };

        let logic_or = logic_and
            .clone()
            .foldl(
                logic_or_op.then(logic_and).repeated(),
                |left, (op, right)| {
                    let start = left.annotation.start;
                    let end = right.annotation.end;
                    Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    }
                    .annotate(ByteSpan { start, end })
                },
            )
            .boxed();

        // Assignment: x = expr (right-associative)
        let equal = select! { (Token::Equal, _) => () };

        let assignment = logic_or
            .clone()
            .then(equal.ignore_then(expr.clone()).or_not())
            .map(|(target, value)| {
                if let Some(val) = value {
                    // This is an assignment
                    if let Expression::Identifier(var) = target.value {
                        let start = target.annotation.start;
                        let end = val.annotation.end;
                        Expression::Assignment(var, Box::new(val))
                            .annotate(ByteSpan { start, end })
                    } else {
                        // Invalid assignment target, but we'll let this parse
                        // and catch it in a later pass
                        target
                    }
                } else {
                    target
                }
            });

        assignment
    })
}

type ParsedDeclaration<'src> = Declaration<'src, &'src str, &'src str, ByteSpan>;

/// Parser for statements and declarations (combined to avoid mutual recursion issues)
pub fn decl_parser<'a, 'src: 'a>() -> impl Parser<
    'a,
    &'a [(Token<'src>, SimpleSpan)],
    ParsedDeclaration<'src>,
    extra::Err<Simple<'a, (Token<'src>, SimpleSpan)>>,
> + Clone {
    recursive(|decl| {
        let expr = expr_parser();
        let semicolon = select! { (Token::Semicolon, _) => () };
        let equal = select! { (Token::Equal, _) => () };
        let comma = select! { (Token::Comma, _) => () };
        let lparen = select! { (Token::LParen, _) => () };
        let rparen = select! { (Token::RParen, _) => () };
        let lbrace = select! { (Token::LBrace, _) => () };
        let rbrace = select! { (Token::RBrace, _) => () };

        let ident = select! {
            (Token::Ident(s), _) => s,
        };

        // Statements (defined inline to avoid mutual recursion)
        let stmt = recursive(|stmt| {
            // Print statement
            let print_stmt = select! { (Token::Print, _) => () }
                .ignore_then(expr.clone())
                .then_ignore(semicolon.clone())
                .map_with(|e, ann| Statement::Print(e).annotate(to_byte_span(ann.span())));

            // Return statement
            let return_stmt = select! { (Token::Return, _) => () }
                .ignore_then(expr.clone())
                .then_ignore(semicolon.clone())
                .map_with(|e, ann| Statement::Return(e).annotate(to_byte_span(ann.span())));

            // Expression statement
            let expr_stmt = expr.clone()
                .then_ignore(semicolon.clone())
                .map_with(|e, ann| Statement::Expression(e).annotate(to_byte_span(ann.span())));

            // Block statement
            let block = decl
                .clone()
                .repeated()
                .collect()
                .delimited_by(lbrace.clone(), rbrace.clone())
                .map_with(|decls, ann| Statement::Block(decls).annotate(to_byte_span(ann.span())));

            // If statement
            let if_stmt = select! { (Token::If, _) => () }
                .ignore_then(expr.clone().delimited_by(lparen.clone(), rparen.clone()))
                .then(stmt.clone())
                .then(select! { (Token::Else, _) => () }.ignore_then(stmt.clone()).or_not())
                .map_with(|((cond, then_branch), else_branch), ann| {
                    Statement::If(
                        cond,
                        Box::new(then_branch),
                        else_branch.map(Box::new),
                    )
                    .annotate(to_byte_span(ann.span()))
                });

            // While statement
            let while_stmt = select! { (Token::While, _) => () }
                .ignore_then(expr.clone().delimited_by(lparen.clone(), rparen.clone()))
                .then(stmt.clone())
                .map_with(|(cond, body), ann| {
                    Statement::While(cond, Box::new(body)).annotate(to_byte_span(ann.span()))
                });

            choice((
                print_stmt,
                return_stmt,
                if_stmt,
                while_stmt,
                block,
                expr_stmt,
            ))
        });

        // Variable declaration
        let var_decl = select! { (Token::Var, _) => () }
            .ignore_then(ident.clone())
            .then(equal.ignore_then(expr.clone()).or_not())
            .then_ignore(semicolon.clone())
            .map(|(name, init)| {
                Declaration::Var(VariableDecl(name), init)
            });

        // Function declaration
        let param_list = ident
            .clone()
            .separated_by(comma)
            .collect::<Vec<_>>()
            .delimited_by(lparen.clone(), rparen.clone());

        let fun_decl = select! { (Token::Fun, _) => () }
            .ignore_then(ident.clone())
            .then(param_list)
            .then(stmt.clone())
            .map(|((name, args), body)| {
                Declaration::Function {
                    name: VariableDecl(name),
                    args: args.into_iter().map(VariableDecl).collect(),
                    body,
                }
            });

        // Statement as declaration
        let stmt_decl = stmt.map(Declaration::Statement);

        choice((
            var_decl,
            fun_decl,
            stmt_decl,
        ))
    })
}

/// Parser for a full program
pub fn program_parser<'a, 'src: 'a>() -> impl Parser<
    'a,
    &'a [(Token<'src>, SimpleSpan)],
    Program<'src, &'src str, &'src str, ByteSpan>,
    extra::Err<Simple<'a, (Token<'src>, SimpleSpan)>>,
> {
    decl_parser()
        .repeated()
        .collect()
        .map(|decls| Program { decls })
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_numbers() {
        let src = "42 3.14";
        let tokens = lexer().parse(src).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].0, Token::Number(42.0)));
        assert!(matches!(tokens[1].0, Token::Number(3.14)));
    }

    #[test]
    fn test_lexer_operators() {
        let src = "+ - * / == !=";
        let tokens = lexer().parse(src).unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::Plus);
        assert_eq!(tokens[1].0, Token::Minus);
        assert_eq!(tokens[2].0, Token::Star);
        assert_eq!(tokens[3].0, Token::Slash);
        assert_eq!(tokens[4].0, Token::EqualEqual);
        assert_eq!(tokens[5].0, Token::BangEqual);
    }

    #[test]
    fn test_parser_literal() {
        let src = "42";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();
        assert!(matches!(ast.value, Expression::NumberLiteral(42.0)));
    }

    #[test]
    fn test_parser_binary() {
        let src = "2 + 3";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        if let Expression::Binary { left, operator, right } = ast.value {
            assert!(matches!(left.value, Expression::NumberLiteral(2.0)));
            assert_eq!(operator, BOperator::PLUS);
            assert!(matches!(right.value, Expression::NumberLiteral(3.0)));
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parser_precedence() {
        let src = "2 + 3 * 4";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        // Should parse as: 2 + (3 * 4)
        if let Expression::Binary { left, operator: op1, right } = ast.value {
            assert!(matches!(left.value, Expression::NumberLiteral(2.0)));
            assert_eq!(op1, BOperator::PLUS);

            if let Expression::Binary { left: left2, operator: op2, right: right2 } = right.value {
                assert!(matches!(left2.value, Expression::NumberLiteral(3.0)));
                assert_eq!(op2, BOperator::STAR);
                assert!(matches!(right2.value, Expression::NumberLiteral(4.0)));
            } else {
                panic!("Expected binary expression on right side");
            }
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parser_parentheses() {
        let src = "(2 + 3) * 4";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        // Should parse as: (2 + 3) * 4
        if let Expression::Binary { left, operator, right } = ast.value {
            if let Expression::Binary { left: left2, operator: op2, right: right2 } = left.value {
                assert!(matches!(left2.value, Expression::NumberLiteral(2.0)));
                assert_eq!(op2, BOperator::PLUS);
                assert!(matches!(right2.value, Expression::NumberLiteral(3.0)));
            } else {
                panic!("Expected binary expression on left side");
            }

            assert_eq!(operator, BOperator::STAR);
            assert!(matches!(right.value, Expression::NumberLiteral(4.0)));
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parser_unary() {
        let src = "-42";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        if let Expression::Unary { operator, right } = ast.value {
            assert_eq!(operator, UOperator::MINUS);
            assert!(matches!(right.value, Expression::NumberLiteral(42.0)));
        } else {
            panic!("Expected unary expression");
        }
    }

    #[test]
    fn test_parser_string() {
        let src = r#""hello world""#;
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        assert!(matches!(ast.value, Expression::StringLiteral("hello world")));
    }

    #[test]
    fn test_parser_comparison() {
        let src = "3 < 5";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        if let Expression::Binary { left, operator, right } = ast.value {
            assert!(matches!(left.value, Expression::NumberLiteral(3.0)));
            assert_eq!(operator, BOperator::LESS);
            assert!(matches!(right.value, Expression::NumberLiteral(5.0)));
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parser_complex_expression() {
        // Test a more complex expression: (2 + 3) * 4 - 5 / 2
        let src = "(2 + 3) * 4 - 5 / 2";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        // Just verify it parses successfully and pretty-prints
        let pretty = ast.value.pretty_print();
        assert!(pretty.contains("PLUS"));
        assert!(pretty.contains("STAR"));
        assert!(pretty.contains("MINUS"));
        assert!(pretty.contains("SLASH"));
    }

    #[test]
    fn test_parser_demo() {
        // Demonstrate parsing various expressions
        let test_cases = vec![
            "42",
            "3.14",
            r#""hello""#,
            "true",
            "false",
            "nil",
            "x",
            "-5",
            "!false",
            "2 + 3",
            "5 - 2",
            "4 * 6",
            "10 / 2",
            "2 + 3 * 4",
            "(2 + 3) * 4",
            "x + y * z",
            "a == b",
            "x != y",
            "3 < 5",
            "10 > 2",
            "x <= y",
            "a >= b",
        ];

        for src in test_cases {
            let tokens = lexer().parse(src).into_result();
            assert!(tokens.is_ok(), "Failed to lex: {}", src);
            let tokens = tokens.unwrap();

            let ast = expr_parser().parse(&tokens).into_result();
            assert!(ast.is_ok(), "Failed to parse: {}", src);

            println!("{:20} => {}", src, ast.unwrap().value.pretty_print());
        }
    }

    #[test]
    fn test_parser_function_call() {
        let src = "foo(1, 2, 3)";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        if let Expression::FunctionCall(func, args) = ast.value {
            assert!(matches!(func.value, Expression::Identifier(_)));
            assert_eq!(args.len(), 3);
        } else {
            panic!("Expected function call");
        }
    }

    #[test]
    fn test_parser_assignment() {
        let src = "x = 5";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        if let Expression::Assignment(var, val) = ast.value {
            assert_eq!(var.0, "x");
            assert!(matches!(val.value, Expression::NumberLiteral(5.0)));
        } else {
            panic!("Expected assignment");
        }
    }

    #[test]
    fn test_parser_logical_operators() {
        let src = "true and false or true";
        let tokens = lexer().parse(src).unwrap();
        let ast = expr_parser().parse(&tokens).unwrap();

        // Should parse as: (true and false) or true
        if let Expression::Binary { left, operator, right } = ast.value {
            assert_eq!(operator, BOperator::OR);
            assert!(matches!(right.value, Expression::BooleanLiteral(true)));

            if let Expression::Binary { left: left2, operator: op2, right: right2 } = left.value {
                assert_eq!(op2, BOperator::AND);
                assert!(matches!(left2.value, Expression::BooleanLiteral(true)));
                assert!(matches!(right2.value, Expression::BooleanLiteral(false)));
            } else {
                panic!("Expected binary expression on left side");
            }
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_stmt_var_decl() {
        let src = "var x = 10;";
        let tokens = lexer().parse(src).unwrap();
        let ast = decl_parser().parse(&tokens).unwrap();

        if let Declaration::Var(var, Some(expr)) = ast {
            assert_eq!(var.0, "x");
            assert!(matches!(expr.value, Expression::NumberLiteral(10.0)));
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_program_simple() {
        let src = r#"
            var x = 10;
            print x;
        "#;
        let tokens = lexer().parse(src).unwrap();
        let program = program_parser().parse(&tokens).unwrap();

        assert_eq!(program.decls.len(), 2);
        assert!(matches!(program.decls[0], Declaration::Var(_, _)));
        if let Declaration::Statement(stmt) = &program.decls[1] {
            assert!(matches!(stmt.value, Statement::Print(_)));
        } else {
            panic!("Expected statement");
        }
    }

    #[test]
    fn test_program_function() {
        let src = r#"
            fun add(a, b) {
                return a + b;
            }
            print add(2, 3);
        "#;
        let tokens = lexer().parse(src).unwrap();
        let program = program_parser().parse(&tokens).unwrap();

        assert_eq!(program.decls.len(), 2);

        if let Declaration::Function { name, args, body } = &program.decls[0] {
            assert_eq!(name.0, "add");
            assert_eq!(args.len(), 2);
            assert!(matches!(body.value, Statement::Block(_)));
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_program_full_example() {
        let src = r#"
            fun fibonacci(n) {
                if (n < 2) {
                    return n;
                } else {
                    return fibonacci(n - 1) + fibonacci(n - 2);
                }
            }

            var i = 0;
            while (i < 10) {
                print fibonacci(i);
                i = i + 1;
            }
        "#;
        let tokens = lexer().parse(src).unwrap();
        let result = program_parser().parse(&tokens).into_result();

        assert!(result.is_ok(), "Failed to parse fibonacci program");
        let program = result.unwrap();
        assert_eq!(program.decls.len(), 3); // function, var, while
    }
}
