type Token =
    | "left_paren" | "right_paren"
    | "left_brace" | "right_brace"
    | "comma" | "dot" | "minus" | "plus"
    | "semicolon" | "slash" | "star"
    | "bang" | "bang_equal"
    | "equal" | "equal_equal"
    | "greater" | "greater_equal"
    | "less" | "less_equal"
    | { __type: "number_literal", value: number }
    | { __type: "identifier", value: string }
    | { __type: "string_literal", value: string }
    | "and" | "class" | "else" | "false" | "fun" | "for" | "if" | "nil" | "or"
    | "print" | "return" | "super" | "this" | "true" | "var" | "while"
    | "eof"
    ;

type ParseError = "parse_error";
// Error type should have a message and a location


function scanTokens(input: string): Token[] | ParseError {
    if (input.length === 0) {
        return ["eof"];
    }
    const x = input[0];
    return ["left_paren"];
}


