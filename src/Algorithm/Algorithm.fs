namespace Algorithm

open Models
open Types

module Calculus =


    let generateAxioms (f: Formula) : (Formula list * Formula list) list =
        (*let extraction (set: Formula list) : Set<Formula> =
            let rec filter (acc: Set<Formula>, fl: Formula list) =
                match fl with
                | [] -> acc
                | x :: xs ->
                    match x with
                    | True
                    | False
                    | Atom(_)
                    | Imp(_, _) -> filter (Set.add x acc, xs)
                    | _ -> filter (acc, xs)

            filter (Set.empty, set)*)

        let isValid (l: Set<Formula>, r: Set<Formula>, total: Set<Formula>) =
            let intersectionRule = (Set.intersect l r = Set.empty)
            let unionRule = (Set.union l r = total)

            intersectionRule && unionRule

        let (sl, sr) = Expression.subFormulas f

        let atomsSL = Expression.atoms sl
        let atomsSR = Expression.atoms sr

        let union = Set.union atomsSL atomsSR

        match isValid (atomsSL, atomsSR, union) with
        | true ->
            let rec combinations (acc: (Formula list * Formula list) list, current: Formula list, set: Formula list) =
                match set with
                | [] -> acc @ [ (current, set) ]
                | x :: xs -> combinations (acc @ [ (current, set) ], current @ [ x ], xs)

            combinations ([], [], (Set.toList union))
        | false -> []

    let execute (goal: Formula, premises: Formula list) : Formula list = []
