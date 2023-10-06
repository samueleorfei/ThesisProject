// For more information see https://aka.ms/fsharp-console-apps

open Models

let test = "(B -> C) & A"

printfn "%A" (Formula.parse test |> Formula.toString)
