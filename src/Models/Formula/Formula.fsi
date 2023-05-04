namespace Models

module Formula =
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

    val toString: AST<'T> -> string;

    val parse: 'T -> AST<'T>;

    val evaluate: AST<'T> -> bool list;