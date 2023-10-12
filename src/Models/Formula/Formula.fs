namespace Models

open Models.Types
open System.IO
open Lexing
open Parsing

module Formula =
    let rec toString (ast: AST) : string =
        match ast with
        | True -> "T"
        | False -> "F"
        | Atom(x: string) -> $"{x}"
        | Not(x: AST) -> $"-{toString (x)}"
        | And(x: AST, y: AST) -> $"({toString (x)} && {toString (y)})"
        | Or(x: AST, y: AST) -> $"({toString (x)} || {toString (y)})"
        | Imp(x: AST, y: AST) -> $"({toString (x)} -> {toString (y)})"
        | Iff(x: AST, y: AST) -> $"({toString (x)} <-> {toString (y)})"
        | Eq(x: AST, y: AST) -> $"({toString (x)} = {toString (y)})"

    let rec toBinaryTree (ast: AST) : Tree<AST> =
        match ast with
        | True -> Node(True, Null, Null)
        | False -> Node(False, Null, Null)
        | Atom(x: string) -> Node(Atom x, Null, Null)
        | Not(x: AST) -> Node(Not x, toBinaryTree x, Null)
        | And(x: AST, y: AST) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Or(x: AST, y: AST) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Imp(x: AST, y: AST) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Iff(x: AST, y: AST) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Eq(x: AST, y: AST) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)

    let parse (input: string) =
        match Lexer.tokenize input |> Parser.parse with
        | Ok(ast, _) -> ast
        | Error(e) -> failwith e

    let fromFile (path: string) : AST list =
        File.ReadLines(path) |> Seq.map parse |> Seq.toList

    let subFormulas (ast: AST) =
        let rec sub (left: Set<AST>, right: Set<AST>, f: AST, isRight: bool) =
            match f with
            | True
            | False
            | Atom(_) ->
                match isRight with
                | true -> (left, (Set.add f right))
                | false -> ((Set.add f left), right)
            | Not(x) ->
                match isRight with
                | true ->
                    let (l, r) = sub (left, (Set.add x right), x, isRight)

                    (l, (Set.add f r))
                | false ->
                    let (l, r) = sub ((Set.add x left), right, x, isRight)

                    ((Set.add f l), r)
            | And(x, y)
            | Or(x, y) ->
                match isRight with
                | true ->
                    let (fl, fr) = sub (left, (Set.add x right), x, isRight)
                    let (sl, sr) = sub (left, (Set.add y right), y, isRight)

                    ((Set.union fl sl), (Set.add f (Set.union fr sr)))
                | false ->
                    let (fl, fr) = sub ((Set.add x left), right, x, isRight)
                    let (sl, sr) = sub ((Set.add y left), right, y, isRight)

                    ((Set.add f (Set.union fl sl)), (Set.union fr sr))
            | Imp(x, y)
            | Iff(x, y)
            | Eq(x, y) ->
                match isRight with
                | true ->
                    let (fl, fr) = sub (left, (Set.add y right), y, isRight)
                    let (sl, sr) = sub ((Set.add x left), right, x, false)

                    ((Set.union fl sl), (Set.add f (Set.union fr sr)))
                | false ->
                    let (fl, fr) = sub ((Set.add x left), right, x, isRight)
                    let (sl, sr) = sub (left, (Set.add y right), y, true)

                    ((Set.add f (Set.union fl sl)), (Set.union fr sr))

        let (left, right) = sub (Set.empty, Set.empty, ast, true)

        ((Set.toList left), Set.toList right)

    let evaluate (ast: AST) = [ true ]
