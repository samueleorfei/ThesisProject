namespace Models

type F<'T> =
    | True
    | False
    | Atom of 'T
    | Not of F<'T>
    | And of F<'T> * F<'T>
    | Or of F<'T> * F<'T>
    | Imp of F<'T> * F<'T>
    | Iff of F<'T> * F<'T>
    | Eq of F<'T> * F<'T>

module Formula =
    let rec evaluate (F: F<'T>) : string =
        match F with
        | True -> "T"
        | False -> "F"
        | Atom(x: 'T) -> $"{x}"
        | Not(x: F<'T>) -> $"-{evaluate (x)}"
        | And(x: F<'T>, y: F<'T>) -> $"({evaluate (x)} && {evaluate (y)})"
        | Or(x: F<'T>, y: F<'T>) -> $"({evaluate (x)} || {evaluate (y)})"
        | Imp(x: F<'T>, y: F<'T>) -> $"({evaluate (x)} -> {evaluate (y)})"
        | Iff(x: F<'T>, y: F<'T>) -> $"({evaluate (x)} <-> {evaluate (y)})"
        | Eq(x: F<'T>, y: F<'T>) -> $"({evaluate (x)} = {evaluate (y)})"
