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
        let rec sub (acc: AST<string> list, f: AST<string>) =
            match f with
            | True -> acc @ [ True ]
            | False -> acc @ [ False ]
            | Atom(x) -> acc @ [ Atom(x) ]
            | Not(x) -> sub (acc, f)
            | And(x, y) -> sub (acc, x) @ sub (acc, y)
            | Or(x, y) -> sub (acc, x) @ sub (acc, y)
            | Imp(x, y) -> sub (acc, x) @ sub (acc, y)
            | Iff(x, y) -> sub (acc, x) @ sub (acc, y)
            | Eq(x, y) -> sub (acc, x) @ sub (acc, y)

        sub ([], ast)

    let evaluate (ast: AST<'T>) = [ true ]
