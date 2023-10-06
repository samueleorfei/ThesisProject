// For more information see https://aka.ms/fsharp-console-apps

open Models

let goalPath = "./src/Input/Goal.txt"
let premisesPath = "./src/Input/Premises.txt"

let goal = List.item 0 (Formula.fromFile goalPath)
let premises = Formula.fromFile premisesPath

printfn "%A" (goal)
