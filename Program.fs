// For more information see https://aka.ms/fsharp-console-apps

open System.Text.RegularExpressions

type TokenType =
    | Identifier
    | Whitespace
    | Separator
    | OpenParenthesis
    | ClosedParenthesis
    | Operator

type Statement =
    | NotOp
    | AndOp
    | OrOp
    | ImpOp
    | IffOp
    | ForAllOp
    | ExistsOp
    | EqualOp
    | Constant of string

type Token =
    { Type: TokenType
      Value: Statement
      RawValue: string
      Start: int
      End: int
      Index: int }

type F<'T> =
    | True
    | False
    | Atom of 'T
    | Not of F<'T>
    | And of F<'T> * F<'T>
    | Or of F<'T> * F<'T>
    | Imp of F<'T> * F<'T>
    | Iff of F<'T> * F<'T>

module Operation =
    let evaluate (op: Statement) : string =
        match op with
        | NotOp -> "-"
        | AndOp -> "&&"
        | OrOp -> "||"
        | ImpOp -> "->"
        | IffOp -> "<->"
        | EqualOp -> "="
        | Constant(x) -> x
        | _ -> ""

    let parse (op: string) : Statement =
        match op with
        | "-"
        | "not" -> NotOp
        | "&&"
        | "&"
        | "^"
        | "and" -> AndOp
        | "||"
        | "|"
        | "v"
        | "or" -> OrOp
        | "->"
        | "-->"
        | "imp" -> ImpOp
        | "<->"
        | "iff" -> IffOp
        | "="
        | "=="
        | "eq" -> EqualOp
        | _ -> Constant(op)

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

module Word =
    let split (word: string, rules: char array) : string array = word.Split(rules)

    let tokenize (word: string) : char list = Seq.toList word

module Lexer =
    let Grammar =
        Map.ofList
            [ Identifier, "\w+"
              Whitespace, "\s+"
              OpenParenthesis, "(\{|\[|\()"
              ClosedParenthesis, "(\}|\]|\))"
              Operator, "(--|&|\||->|<->|=)" ]

    let tokenize (input: string) : Token list =
        let scanner (input: string) : string list =
            let rec split (slice: string, rule: string) : string list =
                match Regex.IsMatch(slice, rule) with
                | false -> [ slice ]
                | _ -> Regex.Split(slice, rule) |> Array.toList

            split (input, (Map.find Whitespace Grammar))

        let rec evaluator (acc: Token list, index: int, keys: TokenType list, input: string list) : Token list =
            match input with
            | [] -> acc
            | x :: xs ->
                let rec rules (keys: TokenType list) : Token =
                    match keys with
                    | [] -> failwith "Input non valido"
                    | k :: ks ->
                        match Regex.IsMatch(x, (Map.find k Grammar)) with
                        | true ->
                            { Type = k
                              RawValue = x
                              Value = Operation.parse (x)
                              Start = 0
                              End = 0
                              Index = index }
                        | _ -> rules ks

                evaluator (acc @ [ rules (keys) ], index + 1, keys, xs)

        evaluator ([], 0, Grammar.Keys |> Seq.cast |> List.ofSeq, scanner input)

module Parser =
    type AST =
        | Left of AST
        | Right of AST
        | Expression of Token

    let parse (tokens: Token list) : F<string> = Atom("A")


let test = "A <-> B"

printf "%A" (Lexer.tokenize test)
