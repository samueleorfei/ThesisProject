namespace Models

module Enums =
    type TokenType =
        | Identifier
        | Whitespace
        | OpenParenthesis
        | ClosedParenthesis
        | Operator

module Types =
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
        | Eq

    type AST =
        | True
        | False
        | Atom of string
        | Not of AST
        | And of AST * AST
        | Or of AST * AST
        | Imp of AST * AST
        | Iff of AST * AST
        | Eq of AST * AST

    type Tree<'T> =
        | Null
        | Node of 'T * Tree<'T> * Tree<'T>
