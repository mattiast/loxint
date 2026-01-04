use crate::parse::{for_loop_into_while_loop, ByteSpan, ForLoopDef};
use crate::syntax::*;
use chumsky::pratt::{infix, left, postfix, prefix, right};
use chumsky::prelude::*;

type ParsedExpression<'src> = AnnotatedExpression<'src, &'src str, ByteSpan>;

/// Helper to convert SimpleSpan to ByteSpan
fn to_byte_span(span: SimpleSpan) -> ByteSpan {
    ByteSpan {
        start: span.start,
        end: span.end,
    }
}
fn jop<'src, T: Clone>(
    s: &'static str,
    v: T,
) -> impl Parser<'src, &'src str, T, extra::Err<Simple<'src, char>>> + Clone {
    just(s).padded().to(v)
}
fn kw<'src>(
    s: &'static str,
) -> impl Parser<'src, &'src str, (), extra::Err<Simple<'src, char>>> + Clone {
    text::keyword(s).padded().to(())
}

/// Parser for expressions
pub fn expr_parser<'src>(
) -> impl Parser<'src, &'src str, ParsedExpression<'src>, extra::Err<Simple<'src, char>>> + Clone {
    recursive(|expr| {
        // Parse numbers
        let number = text::int(10)
            .then(just('.').then(text::digits(10)).or_not())
            .to_slice()
            .from_str::<f64>()
            .unwrapped()
            .map_with(|n, e| Expression::NumberLiteral(n).annotate(to_byte_span(e.span())))
            .labelled("number");

        // Parse strings
        let string = just('"')
            .ignore_then(none_of('"').repeated().to_slice())
            .then_ignore(just('"'))
            .map_with(|s: &'src str, e| {
                Expression::StringLiteral(s).annotate(to_byte_span(e.span()))
            })
            .labelled("string");

        // Parse identifiers and keywords
        let ident = text::ascii::ident()
            .map(|s: &'src str| match s {
                "true" => Expression::BooleanLiteral(true),
                "false" => Expression::BooleanLiteral(false),
                "nil" => Expression::Nil,
                _ => Expression::Identifier(Variable(s)),
            })
            .map_with(|expr, ex| {
                // Ensure the annotation spans the entire identifier
                expr.annotate(to_byte_span(ex.span()))
            })
            .labelled("identifier");

        // Delimiters
        let lparen = jop("(", ());
        let rparen = jop(")", ());
        let comma = jop(",", ());

        // Primary expressions (atoms)
        let literal = choice((number, string));

        let atom = choice((
            literal,
            ident,
            expr.clone()
                .padded()
                .delimited_by(lparen.clone(), rparen.clone())
                .labelled("parenthesized expression"),
        ))
        .padded();

        // Define operator parsers for each precedence level
        let equal = jop("=", ());

        let or_op = kw("or").to(BOperator::OR);
        let and_op = kw("and").to(BOperator::AND);

        let equality_op = choice((
            jop("==", BOperator::EqualEqual),
            jop("!=", BOperator::BangEqual),
        ));

        let comparison_op = choice((
            jop("<=", BOperator::LessEqual),
            jop(">=", BOperator::GreaterEqual),
            jop("<", BOperator::LESS),
            jop(">", BOperator::GREATER),
        ));

        let term_op = choice((jop("+", BOperator::PLUS), jop("-", BOperator::MINUS)));
        let factor_op = choice((jop("*", BOperator::STAR), jop("/", BOperator::SLASH)));
        let unary_op = choice((jop("-", UOperator::MINUS), jop("!", UOperator::BANG)));

        // Function calls: foo(arg1, arg2, ...)
        let arg_list = expr
            .clone()
            .padded()
            .separated_by(comma)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(lparen, rparen);

        fn bin_expr<'a, 'src: 'a>(
            left: ParsedExpression<'a>,
            op: BOperator,
            right: ParsedExpression<'a>,
        ) -> ParsedExpression<'a> {
            let l_ann = left.annotation;
            let r_ann = right.annotation;
            Expression::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            }
            .annotate(ByteSpan {
                start: l_ann.start,
                end: r_ann.end,
            })
        }
        // Build Pratt parser with all operators
        atom.pratt((
            // Assignment (right-associative, lowest precedence)
            infix(
                right(10),
                equal,
                |lhs: ParsedExpression<'src>, _, rhs, _| {
                    let l_ann = lhs.annotation;
                    let r_ann = rhs.annotation;
                    if let Expression::Identifier(var) = lhs.value {
                        Expression::Assignment(var, Box::new(rhs)).annotate(ByteSpan {
                            start: l_ann.start,
                            end: r_ann.end,
                        })
                    } else {
                        // TODO this should fail parsing
                        lhs
                    }
                },
            ),
            // Logical OR
            infix(left(20), or_op, |lhs, op, rhs, _| bin_expr(lhs, op, rhs)),
            // Logical AND
            infix(left(30), and_op, |lhs, op, rhs, _| bin_expr(lhs, op, rhs)),
            // Equality
            infix(left(40), equality_op, |lhs, op, rhs, _| {
                bin_expr(lhs, op, rhs)
            }),
            // Comparison
            infix(left(50), comparison_op, |lhs, op, rhs, _| {
                bin_expr(lhs, op, rhs)
            }),
            // Term: addition and subtraction
            infix(left(60), term_op, |lhs, op, rhs, _| bin_expr(lhs, op, rhs)),
            // Factor: multiplication and division
            infix(left(70), factor_op, |lhs, op, rhs, _| {
                bin_expr(lhs, op, rhs)
            }),
            // Unary operators: - and !
            prefix(80, unary_op, |op, rhs: ParsedExpression<'src>, _| {
                let ann = rhs.annotation;
                Expression::Unary {
                    operator: op,
                    right: Box::new(rhs),
                }
                .annotate(ann)
            }),
            // Function calls (postfix, highest precedence)
            postfix(90, arg_list, |func: ParsedExpression<'src>, args, _| {
                // TODO span should include args and closing paren
                let ann = func.annotation;
                Expression::FunctionCall(Box::new(func), args).annotate(ann)
            }),
        ))
    })
}

type ParsedDeclaration<'src> = Declaration<'src, &'src str, &'src str, ByteSpan>;

/// Parser for statements and declarations (combined to avoid mutual recursion issues)
pub fn decl_parser<'src>(
) -> impl Parser<'src, &'src str, ParsedDeclaration<'src>, extra::Err<Simple<'src, char>>> + Clone {
    recursive(|decl| {
        let expr = expr_parser();
        let semicolon = jop(";", ());
        let equal = jop("=", ());
        let comma = jop(",", ());
        let lparen = jop("(", ());
        let rparen = jop(")", ());
        let lbrace = jop("{", ());
        let rbrace = jop("}", ());

        let ident = text::ascii::ident().padded();

        // Statements (defined inline to avoid mutual recursion)
        let stmt = recursive(|stmt| {
            // Print statement
            let print_stmt = kw("print")
                .ignore_then(expr.clone())
                .then_ignore(semicolon.clone())
                .map_with(|e, ann| Statement::Print(e).annotate(to_byte_span(ann.span())));

            // Return statement
            let return_stmt = kw("return")
                .ignore_then(expr.clone().or_not())
                .then_ignore(semicolon.clone())
                .map_with(|e, ann| {
                    Statement::Return(
                        e.unwrap_or_else(|| Expression::Nil.annotate(to_byte_span(ann.span()))),
                    )
                    .annotate(to_byte_span(ann.span()))
                });

            // Expression statement
            let expr_stmt = expr
                .clone()
                .then_ignore(semicolon.clone())
                .map_with(|e, ann| Statement::Expression(e).annotate(to_byte_span(ann.span())));

            // Block statement
            let block = decl
                .clone()
                .padded()
                .repeated()
                .collect()
                .delimited_by(lbrace.clone(), rbrace.clone())
                .map_with(|decls, ann| Statement::Block(decls).annotate(to_byte_span(ann.span())));

            // If statement
            let if_stmt = kw("if")
                .ignore_then(expr.clone().delimited_by(lparen.clone(), rparen.clone()))
                .then(stmt.clone())
                .then(kw("else").ignore_then(stmt.clone()).or_not())
                .map_with(|((cond, then_branch), else_branch), ann| {
                    Statement::If(cond, Box::new(then_branch), else_branch.map(Box::new))
                        .annotate(to_byte_span(ann.span()))
                });

            // While statement
            let while_stmt = kw("while")
                .ignore_then(expr.clone().delimited_by(lparen.clone(), rparen.clone()))
                .then(stmt.clone())
                .map_with(|(cond, body), ann| {
                    Statement::While(cond, Box::new(body)).annotate(to_byte_span(ann.span()))
                });

            // For loop - desugared into while loop
            // for (init; cond; incr) body
            // =>
            // {
            //   init;
            //   while (cond) {
            //     { body }
            //     incr;
            //   }
            // }
            let for_stmt = kw("for")
                .ignore_then(lparen.clone())
                .ignore_then(
                    // Parse initializer - can be var declaration, expression, or empty
                    choice((
                        // var x = expr;
                        kw("var")
                            .ignore_then(ident.clone())
                            .then(equal.clone().ignore_then(expr.clone()).or_not())
                            .then_ignore(semicolon.clone())
                            .map(|(name, init)| (Some(VariableDecl(name)), init)),
                        // expr;
                        expr.clone()
                            .then_ignore(semicolon.clone())
                            .map(|e| (None, Some(e))),
                        // ; (empty)
                        semicolon.clone().to((None, None)),
                    )),
                )
                .then(
                    // Parse condition - expression or empty
                    expr.clone().or_not().then_ignore(semicolon.clone()),
                )
                .then(
                    // Parse increment - expression or empty
                    expr.clone().or_not(),
                )
                .then_ignore(rparen.clone())
                .then(stmt.clone())
                .map(|((((var_name, start), cond), increment), body)| {
                    let for_loop_def = ForLoopDef {
                        var_name,
                        start,
                        cond,
                        increment,
                    };
                    for_loop_into_while_loop(for_loop_def, body)
                });

            choice((
                print_stmt,
                return_stmt,
                if_stmt,
                while_stmt,
                for_stmt,
                block,
                expr_stmt,
            ))
        });

        // Variable declaration
        let var_decl = kw("var")
            .ignore_then(ident.clone())
            .then(equal.ignore_then(expr.clone()).or_not())
            .then_ignore(semicolon.clone())
            .map(|(name, init)| Declaration::Var(VariableDecl(name), init));

        // Function declaration
        let param_list = ident
            .clone()
            .separated_by(comma)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(lparen.clone(), rparen.clone());

        let fun_decl = kw("fun")
            .ignore_then(ident.clone())
            .then(param_list)
            .then(stmt.clone())
            .map(|((name, args), body)| Declaration::Function {
                name: VariableDecl(name),
                args: args.into_iter().map(VariableDecl).collect(),
                body,
            });

        // Statement as declaration
        let stmt_decl = stmt.map(Declaration::Statement);

        choice((var_decl, fun_decl, stmt_decl))
    })
}

/// Parser for a full program
pub fn program_parser<'src>() -> impl Parser<
    'src,
    &'src str,
    Program<'src, &'src str, &'src str, ByteSpan>,
    extra::Err<Simple<'src, char>>,
> {
    decl_parser()
        .padded()
        .repeated()
        .collect()
        .map(|decls| Program { decls })
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_literal() {
        let src = "42";
        let ast = expr_parser().parse(src).unwrap();
        assert!(matches!(ast.value, Expression::NumberLiteral(42.0)));
    }

    #[test]
    fn test_parser_binary() {
        let src = "2 + 3";
        let ast = expr_parser().parse(src).unwrap();

        if let Expression::Binary {
            left,
            operator,
            right,
        } = ast.value
        {
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
        let ast = expr_parser().parse(src).unwrap();

        // Should parse as: 2 + (3 * 4)
        if let Expression::Binary {
            left,
            operator: op1,
            right,
        } = ast.value
        {
            assert!(matches!(left.value, Expression::NumberLiteral(2.0)));
            assert_eq!(op1, BOperator::PLUS);

            if let Expression::Binary {
                left: left2,
                operator: op2,
                right: right2,
            } = right.value
            {
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
        let ast = expr_parser().parse(src).unwrap();

        // Should parse as: (2 + 3) * 4
        if let Expression::Binary {
            left,
            operator,
            right,
        } = ast.value
        {
            if let Expression::Binary {
                left: left2,
                operator: op2,
                right: right2,
            } = left.value
            {
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
        let ast = expr_parser().parse(src).unwrap();

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
        let ast = expr_parser().parse(src).unwrap();

        assert!(matches!(
            ast.value,
            Expression::StringLiteral("hello world")
        ));
    }

    #[test]
    fn test_parser_comparison() {
        let src = "3 < 5";
        let ast = expr_parser().parse(src).unwrap();

        if let Expression::Binary {
            left,
            operator,
            right,
        } = ast.value
        {
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
        let ast = expr_parser().parse(src).unwrap();

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
            let ast = expr_parser().parse(src).into_result();
            assert!(ast.is_ok(), "Failed to parse: {}", src);

            println!("{:20} => {}", src, ast.unwrap().value.pretty_print());
        }
    }

    #[test]
    fn test_parser_function_call() {
        let src = "foo(1, 2, 3)";
        let ast = expr_parser().parse(src).unwrap();

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
        let ast = expr_parser().parse(src).unwrap();

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
        let ast = expr_parser().parse(src).unwrap();

        // Should parse as: (true and false) or true
        if let Expression::Binary {
            left,
            operator,
            right,
        } = ast.value
        {
            assert_eq!(operator, BOperator::OR);
            assert!(matches!(right.value, Expression::BooleanLiteral(true)));

            if let Expression::Binary {
                left: left2,
                operator: op2,
                right: right2,
            } = left.value
            {
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
        let ast = decl_parser().parse(src).unwrap();

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
        let program = program_parser().parse(src).unwrap();

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
        let program = program_parser().parse(src).unwrap();

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
        let result = program_parser().parse(src).into_result();

        assert!(result.is_ok(), "Failed to parse fibonacci program");
        let program = result.unwrap();
        assert_eq!(program.decls.len(), 3); // function, var, while
    }
}
