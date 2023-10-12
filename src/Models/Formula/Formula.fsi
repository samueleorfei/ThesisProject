namespace Models

open Models.Types

///
/// Il _modulo_ <code>Formula</code> contiene la definizione delle funzioni per l'interpretazione o la trasformazione di
/// un albero di sintassi per espressioni logiche (**AST**).
///
module Formula =
    ///
    /// Questa funzione prende in input un albero di sintassi generico <code>AST</code> e restituisce l'espressione
    /// logica che esso rappresenta sotto forma di stringa.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST</code> ast
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>string</code>
    ///
    val toString: AST -> string

    ///
    /// Questa funzione prende in input un albero di sintassi generico <code>AST</code> e restituisce l'espressione
    /// logica che esso rappresenta sotto forma di albero binario di ricerca.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST</code> ast
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Tree<AST></code>
    ///
    val toBinaryTree: AST -> Tree<AST>

    ///
    /// Questa funzione prende in input una path di un file e restituisce una lista di formule sotto forma di
    /// **AST** risultate dall'interpretazione del contenuto dei file di input
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
    /// - <code>AST list</code>
    ///
    val fromFile: string -> AST list

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
    /// - <code>AST</code>
    ///
    val parse: string -> AST

    ///
    /// Questa funzione prende in input una formula e ne restituisce la lista di tutte le sue sotto-formule.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST</code> expression
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>AST list</code>
    ///
    val subFormulas: AST -> AST list * AST list

    ///
    /// Questa funzione prende in input un albero di sintassi di una espressione logica e ne restituisce una valutazione booleana delle costanti
    /// o quantificatori in esso presenti valutando, per ogni nodo, il risultato delle operazioni booleane.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST</code> ast
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>bool list</code>
    ///
    val evaluate: AST -> bool list
