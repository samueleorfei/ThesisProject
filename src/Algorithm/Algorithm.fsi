namespace Algorithm

open Models.Types

/// Il modulo **Algorithm** contiene la definizione delle funzioni per eseguire il calcolo di
/// refutazione nella logica di Godel-Dummet.
module Calculus =
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
    /// La funzione <code>execute</code> riceve in input una lista contenente le sotto-formule destre del goal,
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
    /// - <code>option<list<Rule * Set<Formula> * Set<Formula> * Set<Formula> * int>></code>
    ///
    val execute: Formula -> option<list<Rule * Set<Formula> * Set<Formula> * Set<Formula> * int>>

    ///
    /// La funzione <code>printResult</code> riceve in input una lista contenente tutti gli step corretti per la costruzione del contromodello,
    /// e visualizza sul terminale ogni step correttamente indentato
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>option<list<Rule * Set<Formula> * Set<Formula> * Set<Formula * int>>></code> result
    /// ---
    ///
    /// **Return**
    ///
    /// -
    ///
    val printResult: option<list<Rule * Set<Formula> * Set<Formula> * Set<Formula> * int>> -> unit
