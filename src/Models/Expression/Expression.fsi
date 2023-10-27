namespace Models

open Models.Types

///
/// Il _modulo_ <code>Expression</code> contiene la definizione delle funzioni per l'interpretazione o la trasformazione di
/// un albero di sintassi per espressioni logiche (**Formula**).
///
module Expression =
    ///
    /// Questa funzione prende in input un albero di sintassi generico <code>Formula</code> e restituisce l'espressione
    /// logica che esso rappresenta sotto forma di stringa.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> Formula
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>string</code>
    ///
    val toString: Formula -> string

    ///
    /// Questa funzione prende in input un albero di sintassi generico <code>Formula</code> e restituisce l'espressione
    /// logica che esso rappresenta sotto forma di albero binario di ricerca.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> Formula
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Tree<Formula></code>
    ///
    val toBinaryTree: Formula -> Tree<Formula>

    ///
    /// Questa funzione prende in input una path di un file e restituisce una lista di formule sotto forma di
    /// **Formula** risultate dall'interpretazione del contenuto dei file di input
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>string</code> path
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Formula list</code>
    ///
    val fromFile: string -> Formula list

    ///
    /// Questa funzione prende in input un argomento generico rappresentante una qualsiasi espressione logica
    /// e resituisce il suo albero di sintassi.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>string</code> expression
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>Formula</code>
    ///
    val parse: string -> Formula

    ///
    /// Questa funzione prende in input una formula e ne restituisce la lista di tutte le sue sotto-formule.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> expression
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>Formula list</code>
    ///
    val subFormulas: Formula -> Formula list * Formula list

    ///
    /// Questa funzione prende in input una formula ed un insieme di formule e restituisce se la formula rientra nelle chiusure positive dell'insieme.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> formula
    /// - <code>Set of formulas</code> set
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>boolean</code>
    ///
    val isPositiveClosure: Formula * Formula list -> bool

    ///
    /// Questa funzione prende in input una formula ed un insieme di formule e restituisce se la formula rientra nelle chiusure negative dell'insieme.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> formula
    /// - <code>Set of formulas</code> set
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>boolean</code>
    ///
    val isNegativeClosure: Formula * Formula list -> bool

    ///
    /// Questa funzione prende in input un albero di sintassi di una espressione logica e ne restituisce una valutazione booleana delle costanti
    /// o quantificatori in esso presenti valutando, per ogni nodo, il risultato delle operazioni booleane.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Formula</code> Formula
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>bool list</code>
    ///
    val evaluate: Formula -> bool list
