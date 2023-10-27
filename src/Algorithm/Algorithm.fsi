namespace Algorithm

open Models.Types

/// Il modulo **Algorithm** contiene la definizione delle funzioni per eseguire il calcolo di
/// refutazione nella logica di Godel-Dummet.
module Calculus =
    ///
    /// La funzione <code>generateAxioms</code> riceve in input gli insiemi contenenti le sotto-formule sinitre, destre ed una intersezione di esse
    /// e restituisce un insieme di assiomi che rappresentano tutte le combinazioni di inferenza
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Set<Formula></code> gamma
    /// - <code>Set<Formula></code> delta
    /// - <code>Set<Formula></code> lambda
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>(Formula list * Formula list) list</code>
    ///
    val generateAxioms: Set<Formula> * Set<Formula> * Set<Formula> -> (Formula list * Formula list) list

    ///
    /// La funzione <code>gamma</code> riceve in input una formula che rappresenta l'obiettivo del processo dimostrativo,
    /// e restituisce un insieme contenente tutti gli atomi e le implicazioni dell'insieme delle sue sotto-formule sinistre
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
    /// - <code>Set<Formula></code>
    ///
    val gamma: Formula list -> Set<Formula>

    ///
    /// La funzione <code>delta</code> riceve in input una lista contenente le sotto-formule sinistre del goal,
    /// e restituisce un insieme contenente tutti gli atomi e le implicazioni dell'insieme
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula list</code> goal
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Set<Formula></code>
    ///
    val delta: Formula list -> Set<Formula>

    ///
    /// La funzione <code>lambda</code> riceve in input le sotto-formule sinistre e destre del goal,
    /// e restituisce un insieme contenente l'intersezione delle liste
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula list</code> sub-formulas sl
    /// - <code>Formula list</code> sub-formulas sr
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Set<Formula></code>
    ///
    val lambda: Formula list * Formula list -> Set<Formula>

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
    /// - <code>(Formula list * Formula list) list</code>
    ///
    val execute: Formula -> (Formula list * Formula list) list
