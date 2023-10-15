namespace Models

open System.IO

open Models.Types

open Interpreter

module Expression =
    let rec toString (f: Formula) : string =
        match f with
        | True -> "T"
        | False -> "F"
        | Atom(x: string) -> $"{x}"
        | Not(x: Formula) -> $"-{toString (x)}"
        | And(x: Formula, y: Formula) -> $"({toString (x)} && {toString (y)})"
        | Or(x: Formula, y: Formula) -> $"({toString (x)} || {toString (y)})"
        | Imp(x: Formula, y: Formula) -> $"({toString (x)} -> {toString (y)})"
        | Iff(x: Formula, y: Formula) -> $"({toString (x)} <-> {toString (y)})"

    let rec toBinaryTree (f: Formula) : Tree<Formula> =
        match f with
        | True -> Node(True, Null, Null)
        | False -> Node(False, Null, Null)
        | Atom(x: string) -> Node(Atom x, Null, Null)
        | Not(x: Formula) -> Node(Not x, toBinaryTree x, Null)
        | And(x: Formula, y: Formula) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Or(x: Formula, y: Formula) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Imp(x: Formula, y: Formula) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)
        | Iff(x: Formula, y: Formula) -> Node(And(x, y), toBinaryTree x, toBinaryTree y)

    let parse (input: string) =
        match Lexer.tokenize input |> Parser.parse with
        | Ok(f, _) -> f
        | Error(e) -> failwith e

    let fromFile (path: string) : Formula list =
        File.ReadLines(path) |> Seq.map parse |> Seq.toList

    let subFormulas (formula: Formula) =
        let rec sub (left: Set<Formula>, right: Set<Formula>, f: Formula, isRight: bool) =
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
            | Iff(x, y) ->
                match isRight with
                | true ->
                    let (fl, fr) = sub (left, (Set.add y right), y, isRight)
                    let (sl, sr) = sub ((Set.add x left), right, x, false)

                    ((Set.union fl sl), (Set.add f (Set.union fr sr)))
                | false ->
                    let (fl, fr) = sub ((Set.add x left), right, x, isRight)
                    let (sl, sr) = sub (left, (Set.add y right), y, true)

                    ((Set.add f (Set.union fl sl)), (Set.union fr sr))

        let (left, right) = sub (Set.empty, Set.empty, formula, true)

        ((Set.toList left), Set.toList right)

    let rec isPositiveClosure (f: Formula, set: Formula list) : bool =
        match List.contains f set with
        | true -> true
        | false ->
            match f with
            | And(x, y) -> isPositiveClosure (x, set) && isPositiveClosure (y, set)
            | Or(x, y) -> isPositiveClosure (x, set) || isPositiveClosure (y, set)
            | Imp(x, y) -> isPositiveClosure (y, set)
            | _ -> false

    let rec isNegativeClosure (f: Formula, set: Formula list) : bool =
        match List.contains f set with
        | true -> true
        | false ->
            match f with
            | And(x, y) -> isNegativeClosure (x, set) || isNegativeClosure (y, set)
            | Or(x, y) -> isNegativeClosure (x, set) && isNegativeClosure (y, set)
            | Imp(x, y) -> isNegativeClosure (y, set)
            | _ -> false

    let evaluate (f: Formula) = [ true ]
