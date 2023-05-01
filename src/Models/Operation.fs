namespace Models

module Operation =
    let evaluate (op: Token) : string =
        match op with
        | Token.Not -> "-"
        | Token.And -> "&&"
        | Token.Or -> "||"
        | Token.Imp -> "->"
        | Token.Iff -> "<->"
        | Token.Eq -> "="
        | Identifier(x) -> x
        | OpenParenthesis -> "("
        | ClosedParenthesis -> ")"
        | Whitespace -> " "

    let parse (op: string) : Token =
        match op with
        | "-"
        | "not" -> Token.Not
        | "&&"
        | "&"
        | "^"
        | "and" -> Token.And
        | "||"
        | "|"
        | "v"
        | "or" -> Token.Or
        | "->"
        | "-->"
        | "imp" -> Token.Imp
        | "<->"
        | "iff" -> Token.Iff
        | "="
        | "=="
        | "eq" -> Token.Eq
        | "("
        | "["
        | "{" -> OpenParenthesis
        | ")"
        | "]"
        | "}" -> ClosedParenthesis
        | _ -> Identifier(op)