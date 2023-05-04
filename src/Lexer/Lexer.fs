namespace Lexing

open Models
open Models.Enums
open Models.Types

open System.Text.RegularExpressions

module Lexer =
    let tokenize input =
        let scanner (input: string) : string list =
            let rec split (slice: string, rule: string) : string list =
                match Regex.IsMatch(slice, rule) with
                | false -> [ slice ]
                | _ -> Regex.Split(slice, rule) |> Array.toList

            split (input, Token.rule (TokenType.Whitespace))

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
