namespace Lexing

open Models

open System.Text.RegularExpressions

module Lexer =
    let Grammar =
        Map.ofList
            [ TokenType.Identifier, "\w+"
              TokenType.Whitespace, "\s+"
              TokenType.OpenParenthesis, "(\{|\[|\()"
              TokenType.ClosedParenthesis, "(\}|\]|\))"
              Operator, "(--|&|\||->|<->|=)" ]

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
                    | Operator -> Operation.parse (input)
                    | TokenType.Identifier -> Identifier input
                    | TokenType.OpenParenthesis -> OpenParenthesis
                    | TokenType.ClosedParenthesis -> ClosedParenthesis
                    | TokenType.Whitespace -> Whitespace

            List.map (fun (x: string) -> transform (x, keys)) input

        evaluator (scanner input, Grammar.Keys |> Seq.cast |> List.ofSeq)
