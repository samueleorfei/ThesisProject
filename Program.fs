// For more information see https://aka.ms/fsharp-console-apps

open Lexing
open Parsing

let test = "(A & B) -> C"

let lexedTest = Lexer.tokenize test

printfn "%A" lexedTest

printfn "%A" (Parser.parse lexedTest)
