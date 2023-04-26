namespace Models

type Operator =
    | Not
    | And
    | Or
    | Imp
    | Iff
    | ForAll
    | Exists
    | Equal
    | Atom of string

module Operation =
    let evaluate (op: Operator): string =
        match op with
            | Not -> "-"
            | And -> "&&"
            | Or -> "||"
            | Imp -> "->"
            | Iff -> "<->"
            | Equal -> "="
            | Atom(x: string) -> x
            | _ -> ""

    let parse (op: string): Operator =
        match op with
            | "-" | "not" -> Not
            | "&&" | "&" | "^" | "and" -> And
            | "||" | "|" | "v" | "or" -> Or
            | "->" | "-->" | "imp" -> Imp
            | "<->" | "iff" -> Iff
            | "=" | "==" | "eq" -> Equal
            | _ -> Atom(op)