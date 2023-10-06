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

    type AST<'T> =
        | True
        | False
        | Atom of 'T
        | Not of AST<'T>
        | And of AST<'T> * AST<'T>
        | Or of AST<'T> * AST<'T>
        | Imp of AST<'T> * AST<'T>
        | Iff of AST<'T> * AST<'T>
        | Eq of AST<'T> * AST<'T>

    type Tree<'T> =
        | Null
        | Node of 'T * Tree<'T> * Tree<'T>
