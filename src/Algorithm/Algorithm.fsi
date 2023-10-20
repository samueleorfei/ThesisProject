namespace Algorithm

open Models.Types

/// Il modulo **Algorithm** contiene la definizione delle funzioni per eseguire il calcolo di
/// refutazione nella logica di Godel-Dummet.
module Calculus =
    ///
    /// La funzione <code>generateAxioms</code> riceve in input una formula che rappresenta l'obiettivo del processo dimostrativo,
    /// e restituisce un insieme di assiomi che rappresentano tutte le combinazioni di inferenza
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> goal
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>(Formula list * Formula list) list</code>
    ///
    val generateAxioms: Formula -> (Formula list * Formula list) list

    ///
    /// La funzione <code>calculate</code> riceve in input una formula che rappresenta l'obiettivo del processo dimostrativo,
    /// una lista di formule che constituiscono le premesse da cui partire e restituisce una lista di formule che rappresentano
    /// gli assiomi validi
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> goal
    /// - <code>Formula list</code> premises
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Formula list</code>
    ///
    val execute: Formula * Formula list -> Formula list
