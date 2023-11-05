namespace Interpreter

open Interpreter.Types
open Models.Types

module Parser =
    type ParseError = string

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
                    | Error(e: string) -> Error e
                    | Ok(op: Token, ys: Token list) ->
                        match parseExpression ys with
                        | Error(e: string) -> Error e
                        | Ok(k: Formula, ks: Token list) ->
                            match op with
                            | Token.Not -> Ok(Not(k), ks)
                            | _ -> Error $"expected an unary operator, got {op}"
                | [] -> Error $"expected an unary operator, got EOF"
            | Ok(x: Formula, xs: Token list) ->
                match xs with
                | op :: _ ->
                    match parseToken (op, xs) with
                    | Error(e: string) -> Error e
                    | Ok(operator: Token, ls: Token list) ->
                        match parseExpression ls with
                        | Error(e: string) -> Error e
                        | Ok(k: Formula, ks: Token list) ->
                            match operator with
                            | Token.And -> Ok(And(x, k), ks)
                            | Token.Or -> Ok(Or(x, k), ks)
                            | Token.Imp -> Ok(Imp(x, k), ks)
                            | Token.Iff -> Ok(Iff(x, k), ks)
                            | _ -> Error $"expected a binary operator, got {operator}"
                | [] -> Error $"expected a binary operator, got EOF"

        and parseParenthesis (tokens: Token list) =
            match parseToken (OpenParenthesis, tokens) with
            | Error(e: string) -> Error e
            | Ok(_, xs: Token list) ->
                match parseExpression (xs) with
                | Error(e: string) -> Error e
                | Ok(f: Formula, ys: Token list) ->
                    match parseToken (ClosedParenthesis, ys) with
                    | Error(e: string) -> Error e
                    | Ok(_, ks: Token list) -> Ok(f, ks)

        and parseTerm =
            parseParenthesis <|> (parseIdentifier >>= fun x -> constParser (Atom x))

        and parseExpression = parseOperator <|> parseTerm

        parseExpression tokens
