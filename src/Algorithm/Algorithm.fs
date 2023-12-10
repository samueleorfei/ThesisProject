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

    (*let imps (set: Formula list) : Set<Formula> =
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

        List.map (fun x -> single ([], x)) set
        |> List.fold (@) []
        |> List.filter (fun x ->
            match x with
            | Not(_)
            | Imp(_, _) -> true
            | _ -> false)
        |> Set.ofList*)

    let imps (set: Formula list) : Set<Formula> =
        let condition (f: Formula) : bool =
            match f with
            | Imp(_, _)
            | Not(_) -> true
            | _ -> false

        List.filter condition set |> Set.ofList

    let generateAxioms
        (
            sl: Formula list,
            sr: Formula list,
            sf: Formula list
        ) : Set<Formula> * Set<Formula> * Set<Formula> =
        printfn "%s \n" "Estrapolazione degli atomi dalle liste di sotto-formule..."

        let atomSL = atoms sl
        let atomSR = atoms sr
        let atomSF = atoms sf

        printfn "Insieme di atomi sinistri: %A" (Set.map (fun x -> Expression.toString x) atomSL)
        printfn "Insieme di atomi destri: %A \n" (Set.map (fun x -> Expression.toString x) atomSR)

        let g = atomSL
        let d = Set.add False (Set.difference atomSF g)
        let l = Set.empty

        printfn "%s \n" "Creazione dei sequenti iniziali..."

        printfn
            "%A =/=> %A; %A \n"
            (Set.map (fun x -> Expression.toString x) g)
            (Set.map (fun x -> Expression.toString x) l)
            (Set.map (fun x -> Expression.toString x) d)

        assert (Set.union g atomSR = atomSF)
        assert (Set.intersect g (Set.difference atomSF g) = Set.empty)

        match
            (Set.union g atomSR = atomSF)
            && (Set.intersect g (Set.difference atomSF g) = Set.empty)
        with
        | true -> (g, l, d)
        | _ -> failwithf "I sequenti non rispettano le condizioni iniziali"

    let execute (goal: Formula) : Set<Formula> * Set<Formula> * Set<Formula> =
        printfn "%s \n" "Step 1: calcolo delle sotto-formule del goal"

        printfn "%s \n" "Estrapolazione delle sotto-formule dal goal..."

        let (sl, sr) = Expression.subFormulas goal

        let sf = sl @ sr

        let impSL = imps sl |> Set.toList
        let impSR = imps sr |> Set.toList

        printfn "Sotto-formule sinistre: %A \n" (List.map (fun x -> Expression.toString x) sl)
        printfn "Sotto-formule destre: %A \n" (List.map (fun x -> Expression.toString x) sr)

        printfn "%s \n" "Step 2: generazione degli assiomi di partenza"

        let (gamma, lambda, delta) = generateAxioms (sl, sr, sf)

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

                let negativeSingle (f: Formula, delta: Set<Formula>) : bool =
                    match f with
                    | Imp(x, _)
                    | Not(x) -> Expression.isNegativeClosure (x, delta |> Set.toList)
                    | _ -> false

                let positive (f: Formula, gamma: Set<Formula>, lambda: Set<Formula>) : bool =
                    match f with
                    | Imp(_, y) -> Expression.isPositiveClosure (y, (Set.union gamma lambda) |> Set.toList)
                    | Not(_) -> Expression.isPositiveClosure (False, (Set.union gamma lambda) |> Set.toList)
                    | _ -> false

                match Set.count lambda with
                | 0 -> impCondition (f, gamma, delta) && negativeSingle (f, delta)
                | _ ->
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

                match Set.count lambda with
                | 0 -> impCondition (f, delta) && positive (f, gamma)
                | _ -> impCondition (f, delta) && positive (f, gamma) && negative (f, delta, lambda)

            match (List.length sl = 0 && List.length sr = 0) with
            | true ->
                printfn "%s \n" "Insiemi di sotto-formule vuoti. Non è più possibile espandere i sequenti."

                (gamma, lambda, delta)
            | false ->
                match Expression.isNegativeClosure (goal, (Set.union delta lambda) |> Set.toList) with
                | true ->
                    printfn "%s \n" "Condizione finale rispettata."

                    (gamma, lambda, delta)
                | false ->
                    let leftItems: Formula list =
                        List.filter (fun x -> leftConditions (x, gamma, delta, lambda)) sl

                    match leftItems with
                    | [] ->
                        let rightItems = List.filter (fun x -> rightConditions (x, gamma, delta, lambda)) sr

                        match rightItems with
                        | [] ->
                            printfn "%s" "Saturazione..."
                            printfn "%s" "----------------------------------------------------------------------------"

                            let nextLambda = Set.intersect gamma (atoms (sl @ sr))
                            let nG = (Set.difference gamma nextLambda)
                            let nD = (Set.union delta nextLambda)

                            printfn "K = %d, applicazione regola Succ" (Set.count nextLambda)

                            let mappedG = Set.map (fun x -> Expression.toString x) nG
                            let mappedL = Set.map (fun x -> Expression.toString x) nextLambda
                            let mappedD = Set.map (fun x -> Expression.toString x) nD

                            printfn "%A =/=> %A; %A" mappedG mappedL mappedD

                            printfn
                                "%s \n"
                                "----------------------------------------------------------------------------"

                            loop (goal, nG, nD, nextLambda, sl, sr)
                        | r :: ri ->
                            printfn "K = %d, applicazione regola R->" (Set.count lambda)
                            let nD = Set.add r delta

                            let mappedG = Set.map (fun x -> Expression.toString x) gamma
                            let mappedL = Set.map (fun x -> Expression.toString x) lambda
                            let mappedD = Set.map (fun x -> Expression.toString x) nD

                            printfn "%A =/=> %A; %A \n" mappedG mappedL mappedD

                            loop (goal, gamma, nD, lambda, sl, (List.filter (fun x -> x <> r) sr))
                    | l :: li ->
                        printfn "K = %d, applicazione regola L->" (Set.count lambda)
                        let nG = Set.add l gamma

                        let mappedG = Set.map (fun x -> Expression.toString x) nG
                        let mappedL = Set.map (fun x -> Expression.toString x) lambda
                        let mappedD = Set.map (fun x -> Expression.toString x) delta

                        printfn "%A =/=> %A; %A \n" mappedG mappedL mappedD

                        loop (goal, nG, delta, lambda, (List.filter (fun x -> x <> l) sl), sr)

        printfn "%s \n" "Step 3: inizio del calcolo ricorsivo"

        loop (goal, gamma, delta, lambda, impSL, impSR)
