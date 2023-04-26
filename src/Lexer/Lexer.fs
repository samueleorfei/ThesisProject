namespace Lexing

open System.Text.RegularExpressions

module Lexer =
    let Grammar =
        Map.ofList
            [ Identifier, "\w+"
              Whitespace, "\s+"
              OpenParenthesis, "(\{|\[|\()"
              ClosedParenthesis, "(\}|\]|\))"
              Operator, "(--|&|\||->|<->|=)" ]

    let tokenize (input: string) : Token list =
        let checkRule (input: string, rule: string) : bool = Regex.IsMatch(input, rule)

        let scanner (input: string) : string list =
            let rec split (slice: string, rule: string) : string list =
                match checkRule (slice, rule) with
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
                        match checkRule (x, (Map.find k Grammar)) with
                        | true ->
                            { Type = k
                              Value = x
                              Start = 0
                              End = 0
                              Index = index }
                        | _ -> rules ks

                evaluator (acc @ [ rules (keys) ], index + 1, keys, xs)

        evaluator ([], 0, Grammar.Keys |> Seq.cast |> List.ofSeq, scanner input)
