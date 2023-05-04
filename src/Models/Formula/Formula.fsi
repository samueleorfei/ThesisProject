namespace Models

open Models.Types

module Formula =
    val toString: AST<'T> -> string;

    val parse: 'T -> AST<'T>;

    val evaluate: AST<'T> -> bool list;