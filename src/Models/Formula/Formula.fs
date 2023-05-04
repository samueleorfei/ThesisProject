namespace Models

module Formula =
    let rec toString (ast: AST<'T>) : string =
        match ast with
        | True -> "T"
        | False -> "F"
        | Atom(x: 'T) -> $"{x}"
        | Not(x: AST<'T>) -> $"-{evaluate (x)}"
        | And(x: AST<'T>, y: AST<'T>) -> $"({evaluate (x)} && {evaluate (y)})"
        | Or(x: AST<'T>, y: AST<'T>) -> $"({evaluate (x)} || {evaluate (y)})"
        | Imp(x: AST<'T>, y: AST<'T>) -> $"({evaluate (x)} -> {evaluate (y)})"
        | Iff(x: AST<'T>, y: AST<'T>) -> $"({evaluate (x)} <-> {evaluate (y)})"
        | Eq(x: AST<'T>, y: AST<'T>) -> $"({evaluate (x)} = {evaluate (y)})"

    let rec parse input = Atom("X")

    let rec evaluate ast = [true]
