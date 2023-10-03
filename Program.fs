// For more information see https://aka.ms/fsharp-console-apps

open Models

let test = "A & B -> C"

printfn "%A" (Formula.parse test |> Formula.subFormulas)
