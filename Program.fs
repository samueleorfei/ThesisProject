// For more information see https://aka.ms/fsharp-console-apps

open System.Text.RegularExpressions

type TokenType =
    | Identifier
    | Whitespace
    | OpenParenthesis
    | ClosedParenthesis
    | Operator

type Token =
    | Identifier of string
    | Whitespace
    | OpenParenthesis
    | ClosedParenthesis
    | Not
    | And
    | Or
    | Imp
    | Iff
    | Eq

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

module Word =
    let split (word: string, rules: char array) : string array = word.Split(rules)

    let tokenize (word: string) : char list = Seq.toList word

module Lexer =
    let Grammar =
        Map.ofList
            [ TokenType.Identifier, "\w+"
              TokenType.Whitespace, "\s+"
              TokenType.OpenParenthesis, "(\{|\[|\()"
              TokenType.ClosedParenthesis, "(\}|\]|\))"
              TokenType.Operator, "(--|&|\||->|<->|=)" ]

    let tokenize (input: string) : Token list =
        let scanner (input: string) : string list =
            let rec split (slice: string, rule: string) : string list =
                match Regex.IsMatch(slice, rule) with
                | false -> [ slice ]
                | _ -> Regex.Split(slice, rule) |> Array.toList

            split (input, (Map.find TokenType.Whitespace Grammar))

        let evaluator (input: string list, keys: TokenType list) : Token list =
            let transform (input: string, keys: TokenType list) : Token =
                match List.exists (fun (k: TokenType) -> Regex.IsMatch(input, (Map.find k Grammar))) keys with
                | false -> failwith "Input non valido"
                | _ ->
                    match List.find (fun (k: TokenType) -> Regex.IsMatch(input, (Map.find k Grammar))) keys with
                    | TokenType.Operator -> Operation.parse (input)
                    | TokenType.Identifier -> Identifier input
                    | TokenType.OpenParenthesis -> OpenParenthesis
                    | TokenType.ClosedParenthesis -> ClosedParenthesis
                    | TokenType.Whitespace -> Whitespace

            List.map (fun (x: string) -> transform (x, keys)) input

        evaluator (scanner input, Grammar.Keys |> Seq.cast |> List.ofSeq)

type ParseError = string

module Parser =
    let parse (tokens: Token list) =
        let constParser c tokens = Ok(c, tokens)

        let parseIdentifier (tokens: Token list) =
            match tokens with
            | (Identifier x) :: tail -> Ok(x, tail)
            | first :: _ -> Error $"expected an identifier, got {first}"
            | [] -> Error $"expected an identifier, got EOF"

        let parseToken (t: Token, tokens: Token list) =
            match tokens with
            | first :: tail when first = t -> Ok(t, tail)
            | first :: _ -> Error $"expected {t}, got {first}"
            | [] -> Error $"expected {t}, got EOF"

        let (>>=) firstParser nextParser tokens =
            match firstParser tokens with
            | Error e -> Error e
            | Ok(r, rest) -> nextParser r rest

        let (<|>) p1 p2 tokens =
            match p1 tokens with
            | Ok(result, rest) -> Ok(result, rest)
            | Error _ ->
                match p2 tokens with
                | Ok(result, rest) -> Ok(result, rest)
                | Error e -> Error e

        let rec parseOperator (tokens: Token list) =
            match parseTerm tokens with
            | Error _ ->
                match tokens with
                | x :: _ ->
                    match parseToken (x, tokens) with
                    | Error e -> Error e
                    | Ok(op, ys) ->
                        match parseExpression ys with
                        | Error e -> Error e
                        | Ok(k, ks) ->
                            match op with
                            | Token.Not -> Ok(Not(k), ks)
                            | _ -> Error $"expected an unary operator, got {op}"
                | [] -> Error $"expected an unary operator, got EOF"
            | Ok(x, xs) ->
                match xs with
                | op :: _ ->
                    match parseToken (op, xs) with
                    | Error e -> Error e
                    | Ok(operator, ls) ->
                        match parseExpression ls with
                        | Error e -> Error e
                        | Ok(k, ks) ->
                            match operator with
                            | Token.And -> Ok(And(x, k), ks)
                            | Token.Or -> Ok(Or(x, k), ks)
                            | Token.Imp -> Ok(Imp(x, k), ks)
                            | Token.Iff -> Ok(Iff(x, k), ks)
                            | Token.Eq -> Ok(Eq(x, k), ks)
                            | _ -> Error $"expected a binary operator, got {operator}"
                | [] -> Error $"expected a binary operator, got EOF"

        and parseParenthesis (tokens: Token list) =
            match parseToken (OpenParenthesis, tokens) with
            | Error e -> Error e
            | Ok(x, xs) ->
                match parseExpression (xs) with
                | Error e -> Error e
                | Ok(f, ys) ->
                    match parseToken (ClosedParenthesis, ys) with
                    | Error e -> Error e
                    | Ok(_, ks) -> Ok(f, ks)

        and parseTerm =
            parseParenthesis <|> (parseIdentifier >>= fun x -> constParser (Atom x))

        and parseExpression = parseOperator <|> parseTerm

        parseExpression tokens



let test = "A <-> B & B | C"

let lexedTest = Lexer.tokenize test

printfn "%A" lexedTest

printfn "%A" (Parser.parse lexedTest)
