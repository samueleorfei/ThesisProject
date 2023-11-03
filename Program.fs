// For more information see https://aka.ms/fsharp-console-apps

open Models
open Algorithm

let goalPath = "./src/Input/Goal.txt"
let premisesPath = "./src/Input/Premises.txt"

let goal = List.item 0 (Expression.fromFile goalPath)
let premises = Expression.fromFile premisesPath

printfn "%A" (Calculus.execute goal)
