namespace Models

open Models.Enums
open Models.Types

module Token =
    let types =
        fun () ->
            [ TokenType.ClosedParenthesis
              TokenType.OpenParenthesis
              TokenType.Identifier
              Operator
              TokenType.Whitespace ]

    let rule (tokenType: TokenType) =
        match tokenType with
        | TokenType.Identifier -> "\w+"
        | TokenType.Whitespace -> "\s+"
        | TokenType.OpenParenthesis -> "(\{|\[|\()"
        | TokenType.ClosedParenthesis -> "(\}|\]|\))"
        | Operator -> "(--|&|\||->|<->|=)"

    let grammar =
        fun () ->
            types ()
            |> List.collect (fun x -> [ x, rule (x) ])
            |> Map.ofList<TokenType, string>

    let parse (input: string) =
        match input with
        | "-"
        | "--"
        | "not"
        | "!" -> Token.Not
        | "&"
        | "&&"
        | "^"
        | "and" -> Token.And
        | "|"
        | "||"
        | "v"
        | "V"
        | "or" -> Token.Or
        | "("
        | "["
        | "{" -> OpenParenthesis
        | ")"
        | "]"
        | "}" -> ClosedParenthesis
        | "->"
        | "-->"
        | "imp" -> Token.Imp
        | "<->"
        | "<-->"
        | "<>"
        | "iff" -> Token.Iff
        | "="
        | "=="
        | "eq" -> Token.Eq
        | " "
        | "   "
        | "\s" -> Whitespace
        | _ -> Identifier input

    let toString (token: Token<'T>) =
        match token with
        | Token.Not -> "--"
        | Token.And -> "&"
        | Token.Or -> "|"
        | Token.Imp -> "->"
        | Token.Iff -> "<->"
        | Token.Eq -> "="
        | Whitespace -> " "
        | OpenParenthesis -> "("
        | ClosedParenthesis -> ")"
        | Identifier x -> string (x)
