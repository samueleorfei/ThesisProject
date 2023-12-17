// For more information see https://aka.ms/fsharp-console-apps

open Models
open Algorithm
open Types

printfn "%s \n" "Lettura della formula da file..."

let goalPath = "./src/Input/Goal.txt"

//let goal = List.item 0 (Expression.fromFile goalPath)

let A = Not(And(Atom("q"), Atom("r")))
let B = Imp(And(Not(Not(Atom("p"))), Imp(Atom("p"), Atom("q"))), Atom("q"))
let C = Imp(B, And(Not(Not(Atom("p"))), Atom("q")))

let goal =
    Or(Or(Or(A, Imp(Atom("p"), Atom("r"))), B), Imp(C, Or(Atom("p"), Not(Atom("p")))))

printfn "Formula letta: %s \n" (goal |> Expression.toString)

printfn "%s \n" "Inizio del calcolo..."

let results = Calculus.execute (goal)

Calculus.printResult results
