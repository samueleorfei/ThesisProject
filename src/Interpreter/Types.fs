namespace Interpreter

module Types =
    type TokenType =
        | Identifier
        | Whitespace
        | OpenParenthesis
        | ClosedParenthesis
        | Operator

    type Token =
        | Identifier of string
        | Whitespace
        | OpenParenthesis
        | ClosedParenthesis
        | Not
        | And
        | Or
        | Imp
        | Iff
