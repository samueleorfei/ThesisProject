namespace Algorithm

open Models
open Types

module Calculus =
    let atoms (set: Formula list) : Set<Formula> =
        let rec single (acc: Formula list, f: Formula) : Formula list =
            match f with
            | True
            | False -> acc
            | Atom(_) -> acc @ [ f ]
            | Not(x) -> single (acc, x)
            | Imp(x, y)
            | Or(x, y)
            | And(x, y)
            | Iff(x, y) -> single (acc, x) @ single (acc, y)

        List.map (fun x -> single ([], x)) set |> List.fold (@) [] |> Set.ofList

    let imps (set: Formula list) : Set<Formula> =
        let rec single (acc: Formula list, f: Formula) : Formula list =
            match f with
            | True
            | False
            | Atom(_) -> acc
            | Or(x, y)
            | And(x, y)
            | Iff(x, y) -> single (acc, x) @ single (acc, y)
            | Not(x) -> single (acc @ [ x ], x)
            | Imp(x, y) -> single (acc @ [ x ], x) @ single (acc @ [ y ], y)

        List.map (fun x -> single ([], x)) set |> List.fold (@) [] |> Set.ofList

    let generateAxioms
        (
            sl: Formula list,
            sr: Formula list,
            sf: Formula list
        ) : Set<Formula> * Set<Formula> * Set<Formula> =
        let atomSL = atoms sl
        let atomSR = atoms sr
        let atomSF = atoms sf

        let impSL = imps sl
        let impSR = imps sr
        let impSF = imps sf

        let atomImpSL = Set.union atomSL impSL
        let atomImpSR = Set.union atomSR impSR

        //let axioms = generateAxioms (subSL, subSR)

        let g = atomSL
        let d = Set.add False (Set.difference atomSF g)
        let l = Set.empty

        assert (Set.union g atomSR = atomSF)
        assert (Set.intersect g (Set.difference atomSF g) = Set.empty)

        match
            (Set.union g atomSR = atomSF)
            && (Set.intersect g (Set.difference atomSF g) = Set.empty)
        with
        | true -> (g, d, l)
        | _ -> failwithf "I sequenti non rispettano le condizioni iniziali"

    let execute (goal: Formula) : Set<Formula> * Set<Formula> * Set<Formula> =
        // Creare gli insiemi
        // Generare gli assiomi
        // Applicare tutte le regole a tutte le combinazioni di assiomi per vedere la strada corretta

        let (sl, sr) = Expression.subFormulas goal

        let sf = sl @ sr

        let (gamma, delta, lambda) = generateAxioms (sl, sr, sf)

        let rec loop
            (
                goal: Formula,
                gamma: Set<Formula>,
                delta: Set<Formula>,
                lambda: Set<Formula>,
                sl: Formula list,
                sr: Formula list
            ) =
            let leftConditions (f: Formula, gamma: Set<Formula>, delta: Set<Formula>, lambda: Set<Formula>) : bool =
                let impCondition (f: Formula, gamma: Set<Formula>, delta: Set<Formula>) : bool =
                    match f with
                    | Imp(_, _)
                    | Not(_) -> Set.contains f (Set.union gamma delta) |> not
                    | _ -> false

                let negative (f: Formula, delta: Set<Formula>, lambda: Set<Formula>) : bool =
                    match f with
                    | Imp(x, _)
                    | Not(x) -> Expression.isNegativeClosure (x, (Set.union delta lambda) |> Set.toList)
                    | _ -> false

                let positive (f: Formula, gamma: Set<Formula>, lambda: Set<Formula>) : bool =
                    match f with
                    | Imp(_, y) -> Expression.isPositiveClosure (y, (Set.union gamma lambda) |> Set.toList)
                    | Not(_) -> Expression.isPositiveClosure (False, (Set.union gamma lambda) |> Set.toList)
                    | _ -> false

                impCondition (f, gamma, delta)
                && negative (f, delta, lambda)
                && positive (f, gamma, lambda)

            let rightConditions (f: Formula, gamma: Set<Formula>, delta: Set<Formula>, lambda: Set<Formula>) : bool =
                let impCondition (f: Formula, delta: Set<Formula>) : bool =
                    match f with
                    | Imp(_, _)
                    | Not(_) -> Set.contains f (Set.union delta delta) |> not
                    | _ -> false

                let negative (f: Formula, delta: Set<Formula>, lambda: Set<Formula>) : bool =
                    match f with
                    | Imp(_, y) -> Expression.isNegativeClosure (y, (Set.union delta lambda) |> Set.toList)
                    | Not(_) -> Expression.isNegativeClosure (False, (Set.union delta lambda) |> Set.toList)
                    | _ -> false

                let positive (f: Formula, gamma: Set<Formula>) : bool =
                    match f with
                    | Imp(x, _)
                    | Not(x) -> Expression.isPositiveClosure (x, gamma |> Set.toList)
                    | _ -> false

                impCondition (f, delta) && positive (f, gamma) && negative (f, delta, lambda)

            match Expression.isNegativeClosure (goal, (Set.union delta lambda) |> Set.toList) with
            | true -> (gamma, delta, lambda)
            | false ->
                let leftItems: Formula list =
                    List.filter (fun x -> leftConditions (x, gamma, delta, lambda)) sl

                match leftItems with
                | [] ->
                    let rightItems = List.filter (fun x -> rightConditions (x, gamma, delta, lambda)) sr

                    match rightItems with
                    | [] ->
                        printfn "%A" "Succ"
                        (gamma, delta, lambda) // Succ?
                    | r :: ri -> loop (goal, gamma, Set.add r delta, lambda, sl, ri)
                | l :: li -> loop (goal, Set.add l gamma, delta, lambda, li, sr)

        loop (goal, gamma, delta, lambda, sl, sr)
