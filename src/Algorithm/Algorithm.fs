namespace Algorithm

open Models
open Types

module Calculus =
    let atoms (set: Formula list) : Set<Formula> =
        let condition (f: Formula) : bool =
            match f with
            | True
            | False
            | Atom(_) -> true
            | _ -> false

        List.filter condition set |> Set.ofList

    let gamma (sl: Formula list) : Set<Formula> =
        let condition (f: Formula) : bool =
            match f with
            | True
            | False
            | Atom(_)
            | Imp(_, _)
            | Not(_) -> true
            | _ -> false

        List.filter condition sl |> Set.ofList

    let lambda (sl: Formula list, sr: Formula list) : Set<Formula> = Set.intersect (atoms sl) (atoms sr)

    let delta (sr: Formula list) : Set<Formula> =
        let condition (f: Formula) : bool =
            match f with
            | True
            | False
            | Atom(_)
            | Imp(_, _)
            | Not(_) -> true
            | _ -> false

        List.filter condition sr |> Set.ofList

    let generateAxioms (g: Set<Formula>, d: Set<Formula>, l: Set<Formula>) : (Formula list * Formula list) list =
        let isValid (l: Set<Formula>, r: Set<Formula>, total: Set<Formula>) =
            let intersectionRule = (Set.intersect l r = Set.empty)
            let unionRule = (Set.union l r = total)

            intersectionRule && unionRule

        let atomsG = atoms (Set.toList g)
        let atomsD = atoms (Set.toList d)
        let atomsL = atoms (Set.toList l)

        let union = Set.union atomsG atomsD

        match isValid (atomsG, atomsD, union) with
        | true ->
            let rec combinations (acc: (Formula list * Formula list) list, current: Formula list, set: Formula list) =
                match set with
                | [] -> acc @ [ (current, set) ]
                | x :: xs -> combinations (acc @ [ (current, set) ], current @ [ x ], xs)

            combinations ([], [], (Set.toList union))
        | false -> []

    let execute (goal: Formula) : (Formula list * Formula list) list =
        let (sl, sr) = Expression.subFormulas goal

        let g = gamma sl
        let d = delta sr
        let l = lambda (sl, sr)

        generateAxioms (g, d, l)
