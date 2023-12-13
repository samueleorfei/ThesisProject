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
        let generateAxioms (sf: Formula list) =
            let atomsSF = atoms sf

            let fullLeft = [ (atomsSF, Set.empty, Set.add False Set.empty) ]
            let fullRight = [ (Set.empty, Set.empty, Set.add False atomsSF) ]

            let leftToRight =
                Set.toList atomsSF
                |> List.map (fun x ->
                    ((Set.ofList [ x ]), (Set.empty), (Set.add False (Set.difference atomsSF (Set.ofList [ x ])))))

            let rightToLeft =
                Set.toList atomsSF
                |> List.map (fun x ->
                    ((Set.difference atomsSF (Set.ofList [ x ])), (Set.empty), (Set.add False (Set.ofList [ x ]))))

            fullLeft @ rightToLeft @ leftToRight @ fullRight

        let rec proof
            (
                goal: Formula,
                gamma: Set<Formula>,
                delta: Set<Formula>,
                lambda: Set<Formula>,
                sl: Formula list,
                sr: Formula list,
                k: int
            ) =
            let leftConditions
                (
                    f: Formula,
                    gamma: Set<Formula>,
                    delta: Set<Formula>,
                    lambda: Set<Formula>,
                    k: int
                ) : bool =
                match k with
                | 0 ->
                    match f with
                    | Imp(x, _)
                    | Not(x) ->
                        (Set.contains f (Set.union gamma delta) |> not)
                        && (Expression.isNegativeClosure (x, delta |> Set.toList))
                    | _ -> false
                | _ ->
                    match f with
                    | Imp(x, y) ->
                        (Set.contains f (Set.union gamma delta) |> not)
                        && (Expression.isNegativeClosure (x, (Set.union delta lambda) |> Set.toList))
                        && (Expression.isPositiveClosure (y, (Set.union gamma lambda) |> Set.toList))
                    | Not(x) ->
                        (Set.contains f (Set.union gamma delta) |> not)
                        && (Expression.isNegativeClosure (x, (Set.union delta lambda) |> Set.toList))
                        && (Expression.isPositiveClosure (False, (Set.union gamma lambda) |> Set.toList))
                    | _ -> false

            let rightConditions (f: Formula, gamma: Set<Formula>, delta: Set<Formula>, lambda: Set<Formula>) : bool =
                match f with
                | Imp(x: Formula, y) ->
                    (Set.contains f (Set.union delta delta) |> not)
                    && (Expression.isPositiveClosure (x, gamma |> Set.toList))
                    && (Expression.isNegativeClosure (y, (Set.union delta lambda) |> Set.toList))
                | Not(x) ->
                    (Set.contains f (Set.union delta delta) |> not)
                    && (Expression.isNegativeClosure (False, (Set.union delta lambda) |> Set.toList))
                    && (Expression.isPositiveClosure (x, gamma |> Set.toList))
                | _ -> false

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
                        List.filter (fun x -> leftConditions (x, gamma, delta, lambda, k)) sl

                    match leftItems with
                    | [] ->
                        let rightItems = List.filter (fun x -> rightConditions (x, gamma, delta, lambda)) sr

                        match rightItems with
                        | [] ->
                            printfn "%s" "Saturazione..."
                            printfn "%s" "----------------------------------------------------------------------------"

                            // Testare tutti i possibili lambda primo: [p, q, r], [p, q], [p, r], [q, r], [p], [q], [r]

                            let nextLambda =
                                Set.intersect (atoms (gamma |> Set.toList)) (atoms (delta |> Set.toList)) //Set.intersect gamma (atoms (sl @ sr)) Sistemare calcolo di nextLambda

                            let nG = (Set.difference gamma nextLambda)
                            let nD = (Set.union delta lambda)
                            let nK = k + 1

                            printfn "K = %d, applicazione regola Succ" nK

                            let mappedG = Set.map (fun x -> Expression.toString x) nG
                            let mappedL = Set.map (fun x -> Expression.toString x) nextLambda
                            let mappedD = Set.map (fun x -> Expression.toString x) nD

                            printfn "%A =/=> %A; %A" mappedG mappedL mappedD

                            printfn
                                "%s \n"
                                "----------------------------------------------------------------------------"

                            proof (goal, nG, nD, nextLambda, sl, sr, nK)
                        | r :: ri ->
                            printfn "K = %d, applicazione regola R->" k
                            let nD = Set.add r delta

                            let mappedG = Set.map (fun x -> Expression.toString x) gamma
                            let mappedL = Set.map (fun x -> Expression.toString x) lambda
                            let mappedD = Set.map (fun x -> Expression.toString x) nD

                            printfn "%A =/=> %A; %A \n" mappedG mappedL mappedD

                            proof (goal, gamma, nD, lambda, sl, (List.filter (fun x -> x <> r) sr), k)
                    | l :: li ->
                        printfn "K = %d, applicazione regola L->" k
                        let nG = Set.add l gamma

                        let mappedG = Set.map (fun x -> Expression.toString x) nG
                        let mappedL = Set.map (fun x -> Expression.toString x) lambda
                        let mappedD = Set.map (fun x -> Expression.toString x) delta

                        printfn "%A =/=> %A; %A \n" mappedG mappedL mappedD

                        proof (goal, nG, delta, lambda, (List.filter (fun x -> x <> l) sl), sr, k)


        printfn "%s \n" "Step 1: calcolo delle sotto-formule del goal"

        printfn "%s \n" "Estrapolazione delle sotto-formule dal goal..."

        let (sl, sr) = Expression.subFormulas goal

        let sf = sl @ sr

        let impSL = imps sl |> Set.toList
        let impSR = imps sr |> Set.toList

        printfn "Sotto-formule sinistre: %A \n" (List.map (fun x -> Expression.toString x) sl)
        printfn "Sotto-formule destre: %A \n" (List.map (fun x -> Expression.toString x) sr)

        printfn "%s \n" "Step 2: generazione degli assiomi di partenza"

        let axioms = generateAxioms (sf)

        printfn "%s \n" "Step 3: inizio del calcolo ricorsivo"

        let rec loop
            (
                goal: Formula,
                sf: Formula list,
                sl: Formula list,
                sr: Formula list,
                axioms: (Set<Formula> * Set<Formula> * Set<Formula>) list
            ) =
            match axioms with
            | [] -> failwith "Nessun assioma disponibile"
            | (gamma, lambda, delta) :: ax ->
                printfn "Ax"

                printfn
                    "%A =/=> %A; %A \n"
                    (Set.map (fun x -> Expression.toString x) gamma)
                    (Set.map (fun x -> Expression.toString x) lambda)
                    (Set.map (fun x -> Expression.toString x) delta)

                let firstCondition = (Set.union gamma (atoms (delta |> Set.toList)) = (atoms sf))

                let secondCondition =
                    (Set.intersect gamma (atoms (delta |> Set.toList)) = Set.empty)

                assert firstCondition
                assert secondCondition

                match firstCondition && secondCondition with
                | true -> (gamma, lambda, delta)
                | _ -> failwithf "I sequenti non rispettano le condizioni iniziali"

        //loop (goal, gamma, delta, lambda, impSL, impSR, 0)
        loop (goal, sf, impSL, impSR, axioms |> List.take 1)
