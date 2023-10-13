namespace Interpreter

module Types =
    type TokenType =
        | Identifier
        | Whitespace
        | OpenParenthesis
        | ClosedParenthesis
        | Operator

    type Token<'T> =
        | Identifier of 'T
        | Whitespace
        | OpenParenthesis
        | ClosedParenthesis
        | Not
        | And
        | Or
        | Imp
        | Iff
