namespace Interpreter

open Interpreter.Types

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
        | "!" -> Not
        | "&"
        | "&&"
        | "^"
        | "and" -> And
        | "|"
        | "||"
        | "v"
        | "V"
        | "or" -> Or
        | "("
        | "["
        | "{" -> OpenParenthesis
        | ")"
        | "]"
        | "}" -> ClosedParenthesis
        | "->"
        | "-->"
        | "imp" -> Imp
        | "<->"
        | "<-->"
        | "<>"
        | "iff" -> Iff
        | " "
        | "   "
        | "\s" -> Whitespace
        | _ -> Identifier input

    let toString (token: Token) =
        match token with
        | Not -> "--"
        | And -> "&"
        | Or -> "|"
        | Imp -> "->"
        | Iff -> "<->"
        | Whitespace -> " "
        | OpenParenthesis -> "("
        | ClosedParenthesis -> ")"
        | Identifier(x: string) -> string (x)
