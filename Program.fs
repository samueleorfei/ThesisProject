// For more information see https://aka.ms/fsharp-console-apps

open Models

let goalPath = "./src/Input/Goal.txt"
let premisesPath = "./src/Input/Premises.txt"

let goal = List.item 0 (Expression.fromFile goalPath)
let premises = Expression.fromFile premisesPath

let (sl, sr) = Expression.subFormulas goal

printfn "%A" (sl)
printfn "%A" (sr)
printfn "%A" (Expression.isPositiveClosure (goal, premises))
