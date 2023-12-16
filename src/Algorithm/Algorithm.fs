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
        let condition (f: Formula) : bool =
            match f with
            | Imp(_, _)
            | Not(_) -> true
            | _ -> false

        List.filter condition set |> Set.ofList

    let rec printResult (results: option<list<Rule * Set<Formula> * Set<Formula> * Set<Formula> * int>>) : unit =
        match results with
        | None -> printfn "%A" "Non è possibile costruire il contro-modello"
        | Some step ->
            match step with
            | [] -> printfn "%A \n" "Fine algoritmo"
            | (r, g, l, d, i) :: sx ->
                let mappedG = Set.map (fun x -> Expression.toString x) g
                let mappedL = Set.map (fun x -> Expression.toString x) l
                let mappedD = Set.map (fun x -> Expression.toString x) d

                match r with
                | Ax ->
                    printfn "K = %d, Ax" i
                    printfn "%A =/=> %A; %A" mappedG mappedL mappedD
                | Left ->
                    printfn "K = %d, applicazione regola L->" i
                    printfn "%A =/=> %A; %A" mappedG mappedL mappedD
                | Right ->
                    printfn "K = %d, applicazione regola R->" i
                    printfn "%A =/=> %A; %A" mappedG mappedL mappedD
                | Succ ->
                    printfn
                        "%s"
                        "-------------------------------------------------------------------------------------------"

                    printfn "K = %d, applicazione regola Succ" i
                    printfn "%A =/=> %A; %A" mappedG mappedL mappedD

                    printfn
                        "%s"
                        "-------------------------------------------------------------------------------------------"

                printfn "\n"

                printResult (Some sx)

    let execute (goal: Formula) : option<list<Rule * Set<Formula> * Set<Formula> * Set<Formula> * int>> =
        let combinations (lst: Formula list) =
            let rec comb accLst elemLst =
                match elemLst with
                | h :: t ->
                    let next = [ h ] :: List.map (fun el -> h :: el) accLst @ accLst
                    comb next t
                | _ -> accLst

            comb [] lst

        let generateAxioms (sf: Formula list) =
            let atomsSF = atoms sf

            let combs =
                combinations (atomsSF |> Set.toList)
                |> List.map (fun x ->
                    (x |> Set.ofList, Set.empty<Formula>, Set.add False (Set.difference atomsSF (x |> Set.ofList))))

            combs @ [ (Set.empty, Set.empty, Set.add False atomsSF) ]

        let rec proof
            (
                goal: Formula,
                gamma: Set<Formula>,
                delta: Set<Formula>,
                lambda: Set<Formula>,
                sl: Formula list,
                sr: Formula list,
                k: int,
                history: (Rule * Set<Formula> * Set<Formula> * Set<Formula> * int) list
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
            | true -> Some history
            | false ->
                match Expression.isNegativeClosure (goal, (Set.union delta lambda) |> Set.toList) with
                | true -> Some history
                | false ->
                    let leftItems: Formula list =
                        List.filter (fun x -> leftConditions (x, gamma, delta, lambda, k)) sl

                    match leftItems with
                    | [] ->
                        let rightItems = List.filter (fun x -> rightConditions (x, gamma, delta, lambda)) sr

                        match rightItems with
                        | [] ->
                            let nextLambda =
                                Set.intersect (atoms (gamma |> Set.toList)) (atoms (delta |> Set.toList))

                            let rec controlCombinations (combs, lambda) =
                                match combs with
                                | [] -> None
                                | c :: cs ->
                                    match c = lambda with
                                    | true -> None
                                    | _ ->
                                        let nG = (Set.difference gamma c)
                                        let nD: Set<Formula> = (Set.union delta lambda)
                                        let nK = k + 1

                                        let nextHistory = history @ [ (Succ, nG, c, nD, nK) ]

                                        let res = proof (goal, nG, nD, c, sl, sr, nK, nextHistory)

                                        match res with
                                        | None -> controlCombinations (cs, lambda)
                                        | _ -> res

                            match nextLambda = lambda with
                            | false ->
                                controlCombinations (
                                    List.map (fun x -> Set.ofList x) (combinations (nextLambda |> Set.toList)),
                                    lambda
                                )
                            | _ -> None

                        | r :: ri ->
                            let nD = Set.add r delta

                            let nextHistory = history @ [ (Right, gamma, lambda, nD, k) ]

                            proof (goal, gamma, nD, lambda, sl, (List.filter (fun x -> x <> r) sr), k, nextHistory)
                    | l :: li ->
                        let nG = Set.add l gamma

                        let nextHistory = history @ [ (Left, nG, lambda, delta, k) ]

                        proof (goal, nG, delta, lambda, (List.filter (fun x -> x <> l) sl), sr, k, nextHistory)


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

        printfn
            "%A \n"
            (axioms
             |> List.map (fun (g, l, d) ->
                 (Set.map (fun y -> Expression.toString y) g,
                  Set.map (fun y -> Expression.toString y) l,
                  Set.map (fun y -> Expression.toString y) d)))

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
            | [] ->
                printfn "%s \n" "Nessun assioma disponibile"

                None
            | (gamma, lambda, delta) :: ax ->
                let firstCondition = (Set.union gamma (atoms (delta |> Set.toList)) = (atoms sf))

                let secondCondition =
                    (Set.intersect gamma (atoms (delta |> Set.toList)) = Set.empty)

                assert firstCondition
                assert secondCondition

                match firstCondition && secondCondition with
                | true ->
                    match proof (goal, gamma, delta, lambda, sl, sr, 0, [ (Ax, gamma, lambda, delta, 0) ]) with
                    | Some(result) -> Some result
                    | None -> loop (goal, sf, sl, sr, ax)
                | _ -> failwithf "I sequenti non rispettano le condizioni iniziali"

        loop (goal, sf, impSL, impSR, axioms)
