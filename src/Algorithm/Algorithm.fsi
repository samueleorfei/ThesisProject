namespace Algorithm

open Models.Types

/// Il modulo **Algorithm** contiene la definizione delle funzioni per eseguire il calcolo di
/// refutazione nella logica di Godel-Dummet.
module Calculus =
    ///
    /// La funzione <code>generateAxioms</code> riceve in input gli insiemi contenenti le sotto-formule sinitre, destre
    /// e restituisce un insieme di assiomi che rappresentano tutte le combinazioni di inferenza
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Set<Formula></code> gamma
    /// - <code>Set<Formula></code> delta
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>(Formula list * Formula list) list</code>
    ///
    //val generateAxioms: Set<Formula> * Set<Formula> -> (Formula list * Formula list) list
    val generateAxioms: Formula list * Formula list * Formula list -> Set<Formula> * Set<Formula> * Set<Formula>

    ///
    /// Questa funzione prende in input una lista di formule e restituisce soltanto gli atomi.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Set</code> of formulas
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>Set<Formula></code>
    ///
    val atoms: Formula list -> Set<Formula>

    ///
    /// La funzione <code>delta</code> riceve in input una lista contenente le sotto-formule destre del goal,
    /// e restituisce una lista di formule che rappresentano gli assiomi validi
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
    /// - <code>Set<Formula> * Set<Formula> * Set<Formula></code>
    ///
    val execute: Formula -> Set<Formula> * Set<Formula> * Set<Formula>
