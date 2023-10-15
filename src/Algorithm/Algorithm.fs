namespace Algorithm

open Models
open Types

module Calculus =
    let generateAxioms (f: Formula) : Formula list =
        let (sl, sr) = Expression.subFormulas f

        let atomsSL = Expression.atoms sl
        let atomsSR = Expression.atoms sr

        let isValid (l: Set<Formula>, r: Set<Formula>, total: Set<Formula>) =
            let intersectionRule = (Set.intersect l r = Set.empty)
            let unionRule = (Set.union l r = total)

            intersectionRule && unionRule

        // Generare gli assiomi (Funzione incompleta, solo preparata)

        match isValid (atomsSL, atomsSR, Set.union atomsSL atomsSR) with
        | true -> (Set.union atomsSL atomsSR) |> Set.toList
        | false -> []

    let execute (goal: Formula, premises: Formula list) : Formula list = []
