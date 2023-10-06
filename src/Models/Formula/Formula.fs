namespace Models

open Models.Types
open System.IO
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

    let rec toBinaryTree (ast: AST<'T>) : Tree<AST<'T>> =
        match ast with
        | True -> Node(True, Null, Null)
        | False -> Node(False, Null, Null)
        | Atom(x: 'T) -> Node(Atom x, Null, Null)
        | Not(x: AST<'T>) -> Node(Not x, toBinaryTree x, Null)
        | And(x: AST<'T>, y: AST<'T>) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Or(x: AST<'T>, y: AST<'T>) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Imp(x: AST<'T>, y: AST<'T>) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Iff(x: AST<'T>, y: AST<'T>) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Eq(x: AST<'T>, y: AST<'T>) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)

    let parse (input: string) =
        match Lexer.tokenize input |> Parser.parse with
        | Ok(ast, _) -> ast
        | Error(e) -> failwith e

    let fromFile (path: string) : AST<string> list =
        File.ReadLines(path) |> Seq.map parse |> Seq.toList

    let rec subFormulas (ast: AST<string>) : AST<string> list =
        match ast with
        | True -> [ True ]
        | False -> [ False ]
        | Atom(x) -> [ Atom(x) ]
        | Not(x) -> [ Not(x) ] @ subFormulas (x)
        | And(x, y) -> [ And(x, y) ] @ subFormulas (x) @ subFormulas (y)
        | Or(x, y) -> [ Or(x, y) ] @ subFormulas (x) @ subFormulas (y)
        | Imp(x, y) -> [ Imp(x, y) ] @ subFormulas (x) @ subFormulas (y)
        | Iff(x, y) -> [ Iff(x, y) ] @ subFormulas (x) @ subFormulas (y)
        | Eq(x, y) -> [ Eq(x, y) ] @ subFormulas (x) @ subFormulas (y)

    let rightSubformulas (ast: AST<string>) =
        let rec sub (acc: AST<string> list, sf: AST<string> list, last: AST<string>) =
            match sf with
            | [] -> acc
            | x :: xs ->
                match x with
                | _ when x = ast -> sub (acc @ [ x ], xs, x)
                | _ ->
                    match last with
                    | Imp(y, z) -> sub (acc @ [ y ], xs, y)
                    | And(y, z) -> sub (acc @ [ z ], xs, z)
                    | Or(y, z) -> sub (acc @ [ z ], xs, z)
                    | Iff(y, z) -> sub (acc @ [ z ], xs, z)
                    | Eq(y, z) -> sub (acc @ [ z ], xs, z)
                    | _ -> sub (acc, xs, x)

        sub ([], (subFormulas ast), ast)

    let leftSubformulas (ast: AST<string>) =
        let rec sub (acc: AST<string> list, sf: AST<string> list, last: AST<string>) =
            match sf with
            | [] -> acc
            | x :: xs ->
                match x with
                | _ when x = ast -> sub (acc, xs, x)
                | _ ->
                    match last with
                    | Imp(y, z) -> sub (acc @ [ z ], xs, z)
                    | And(y, z) -> sub (acc @ [ y ], xs, y)
                    | Or(y, z) -> sub (acc @ [ y ], xs, y)
                    | Iff(y, z) -> sub (acc @ [ y ], xs, y)
                    | Eq(y, z) -> sub (acc @ [ y ], xs, y)
                    | _ -> sub (acc, xs, x)

        sub ([], (subFormulas ast), ast)

    let positiveClosures (ast: AST<string>) : AST<string> list =
        let rec closures (acc: AST<string> list, sub: AST<string> list) : AST<string> list =
            match sub with
            | [] -> acc
            | x :: xs ->
                match x with
                | Atom(y) -> closures (acc @ [ Atom(y) ], xs)
                | And(y, z) when y = z -> closures (acc @ [ And(y, z) ], xs)
                | Or(y, z) -> closures (acc @ [ Or(y, z) ], xs)
                | Imp(y, z) -> closures (acc @ [ Imp(y, z) ], xs)
                | _ -> closures (acc, xs)

        closures ([], (subFormulas ast))

    let negativeClosures (ast: AST<string>) =
        let rec closures (acc: AST<string> list, subFormulas: AST<string> list) : AST<string> list =
            match subFormulas with
            | [] -> acc
            | x :: xs ->
                match x with
                | Atom(y) -> closures (acc @ [ Atom(y) ], xs)
                | And(y, z) -> closures (acc @ [ And(y, z) ], xs)
                | Or(y, z) when y = z -> closures (acc @ [ Or(y, z) ], xs)
                | _ -> closures (acc, xs)

        closures ([], (subFormulas ast))

    let evaluate (ast: AST<'T>) = [ true ]
