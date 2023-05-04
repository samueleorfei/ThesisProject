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
    let rec toString (ast: AST<'T>) : string =
        match ast with
        | True -> "T"
        | False -> "F"
        | Atom(x: 'T) -> $"{x}"
        | Not(x: AST<'T>) -> $"-{toString (x)}"
        | And(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} && {toString (y)})"
        | Or(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} || {toString (y)})"
        | Imp(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} -> {toString (y)})"
        | Iff(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} <-> {toString (y)})"
        | Eq(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} = {toString (y)})"

    let rec parse input = Atom(input)

    let rec evaluate (ast: AST<'T>) = [true]
