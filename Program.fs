// For more information see https://aka.ms/fsharp-console-apps

open Models
open Algorithm

printfn "%s \n" "Lettura della formula da file..."

let goalPath = "./src/Input/Goal.txt"
//let premisesPath = "./src/Input/Premises.txt"

let goal = List.item 0 (Expression.fromFile goalPath)
//let premises = Expression.fromFile premisesPath

printfn "Formula letta: %s \n" (goal |> Expression.toString)

printfn "%s \n" "Inizio del calcolo..."

let (gamma, lambda, delta) = Calculus.execute (goal)

printfn "%s \n" "Fine algoritmo. Sequenti finali:"

printfn "Gamma: %A" gamma
printfn "Lambda: %A" lambda
printfn "Delta: %A" delta
