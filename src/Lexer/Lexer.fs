namespace Lexing

open Models
open Models.Enums
open Models.Types

open System.Text.RegularExpressions

module Lexer =
    let tokenize (input: string) =
        let scanner (input: string) : string list =
            let split (slice: string, rule: string) : string =
                match Regex.IsMatch(slice, rule) with
                | false -> slice
                | _ ->
                    let matches =
                        Regex.Matches(slice, rule)
                        |> Seq.cast
                        |> List.ofSeq
                        |> List.map (fun x -> string (x))

                    let rec change (matches, input: string) =
                        match matches with
                        | [] -> input
                        | m :: ms -> change (ms, input.Replace(m, m + "?"))

                    change (matches, slice)

            let rec splitByRules (result: string, rules: TokenType list) =
                match rules with
                | [] -> result
                | r :: rs ->
                    let accumulator: string = split (result, Token.rule r)

                    splitByRules (accumulator, rs)

            (splitByRules (input, Token.types ())).Split([| '?' |])
            |> Array.toList
            |> List.filter (fun x -> x <> "" && x <> " ")

        let evaluator (input: string list, keys: TokenType list) : Token<'T> list =
            let transform (input: string, keys: TokenType list) : Token<'T> =
                match List.exists (fun (k: TokenType) -> Regex.IsMatch(input, Token.rule k)) keys with
                | false -> failwith "Input non valido"
                | _ ->
                    match List.find (fun (k: TokenType) -> Regex.IsMatch(input, Token.rule k)) keys with
                    | Operator -> Token.parse (input)
                    | TokenType.Identifier -> Identifier input
                    | TokenType.OpenParenthesis -> OpenParenthesis
                    | TokenType.ClosedParenthesis -> ClosedParenthesis
                    | TokenType.Whitespace -> Whitespace

            List.map (fun (x: string) -> transform (x, keys)) input

        evaluator (scanner input, Token.types ())
