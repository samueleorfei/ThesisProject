namespace Models

open Models.Types
open Lexing
open Parsing

module Formula =
    let rec toString (ast: AST<'T>) : string =
        match ast with
        | True -> "T"
        | False -> "F"
        | Atom(x: 'T) -> $"{x}"
        | Not(x: AST<'T>) -> $"-{toString (x)}"
        | And(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} && {toString (y)})"
        | Or(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} || {toString (y)})"
        | Imp(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} -> {toString (y)})"
        | Iff(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} <-> {toString (y)})"
        | Eq(x: AST<'T>, y: AST<'T>) -> $"({toString (x)} = {toString (y)})"

    let parse (input: string) =
        match Lexer.tokenize input |> Parser.parse with
        | Ok(ast, _) -> ast
        | Error(e) -> failwith e

    let subFormulas (ast: AST<string>) : AST<string> list =
        let rec sub (f: AST<string>) =
            match f with
            | True -> [ True ]
            | False -> [ False ]
            | Atom(x) -> [ Atom(x) ]
            | Not(x) -> [ Not(x) ] @ sub (x)
            | And(x, y) -> [ And(x, y) ] @ sub (x) @ sub (y)
            | Or(x, y) -> [ Or(x, y) ] @ sub (x) @ sub (y)
            | Imp(x, y) -> [ Imp(x, y) ] @ sub (x) @ sub (y)
            | Iff(x, y) -> [ Iff(x, y) ] @ sub (x) @ sub (y)
            | Eq(x, y) -> [ Eq(x, y) ] @ sub (x) @ sub (y)

        sub ast |> List.removeAt 0

    let positiveClosures (ast: AST<string>, sub: AST<string> list) = sub

    let negativeClosures (ast: AST<string>, sub: AST<string> list) = sub

    let evaluate (ast: AST<'T>) = [ true ]
