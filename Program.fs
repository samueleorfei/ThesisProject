// For more information see https://aka.ms/fsharp-console-apps

open Models
open Algorithm
open Types





let atoms (set: Formula list) : Set<Formula> =
    let rec single (acc: Formula list, f: Formula) : Formula list =
        match f with
        | True
        | False -> acc
        | Atom(_) -> acc @ [ f ]
        | Not(x) -> single (acc, x)
        | Imp(x, y)
        | Or(x, y)
        | And(x, y)
        | Iff(x, y) -> single (acc, x) @ single (acc, y)

    List.map (fun x -> single ([], x)) set |> List.fold (@) [] |> Set.ofList

let imps (set: Formula list) : Set<Formula> =
    let rec single (acc: Formula list, f: Formula) : Formula list =
        match f with
        | True
        | False
        | Atom(_) -> acc
        | Or(x, y)
        | And(x, y)
        | Iff(x, y) -> single (acc, x) @ single (acc, y)
        | Not(x) -> single (acc @ [ x ], x)
        | Imp(x, y) -> single (acc @ [ x ], x) @ single (acc @ [ y ], y)

    List.map (fun x -> single ([], x)) set
    |> List.fold (@) []
    |> List.filter (fun x ->
        match x with
        | Not(_)
        | Imp(_, _) -> true
        | _ -> false)
    |> Set.ofList

printfn "%s \n" "Lettura della formula da file..."

let goalPath = "./src/Input/Goal.txt"
//let premisesPath = "./src/Input/Premises.txt"

//let goal = List.item 0 (Expression.fromFile goalPath)
//let premises = Expression.fromFile premisesPath

let A = Not(And(Atom("q"), Atom("r")))
let B = Imp(And(Not(Not(Atom("p"))), Imp(Atom("p"), Atom("q"))), Atom("q"))
let C = Imp(B, And(Not(Not(Atom("p"))), Atom("q")))

let goal =
    Or(Or(Or(A, Imp(Atom("p"), Atom("r"))), B), Imp(C, Or(Atom("p"), Not(Atom("p")))))

let (sl, sr) = Expression.subFormulas goal

let atomSL = atoms sl
let atomSR = atoms sr
let impSL = imps sl
let impSR = imps sr
let atomImpSL = Set.union atomSL impSL
let atomImpSR = Set.union atomSR impSR

printfn "%A" impSL
printfn "%A" impSR

printfn "Formula letta: %s \n" (goal |> Expression.toString)

printfn "%s \n" "Inizio del calcolo..."

let (gamma, lambda, delta) = Calculus.execute (goal)

printfn "%s \n" "Fine algoritmo. Sequenti finali:"

printfn "%A =/=> %A; %A \n" gamma lambda delta
