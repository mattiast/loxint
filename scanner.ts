type Token = "left_paren" | "right_paren" | "plus" | { __type: "number_literal", value: number };

type ParseError = "parse_error";
// Error type should have a message and a location


function scanTokens(input: string): Token[] | ParseError {
    return ["left_paren"];
}


