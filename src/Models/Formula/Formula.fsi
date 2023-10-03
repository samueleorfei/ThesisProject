namespace Models

open Models.Types

///
/// Il _modulo_ <code>Formula</code> contiene la definizione delle funzioni per l'interpretazione o la trasformazione di
/// un albero di sintassi per espressioni logiche (**AST**).
///
module Formula =
    ///
    /// Questa funzione prende in input un albero di sintassi generico <code>AST<'T></code> e restituisce l'espressione
    /// logica che esso rappresenta sotto forma di stringa.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST<'T></code> ast
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>string</code>
    ///
    val toString: AST<'T> -> string

    ///
    /// Questa funzione prende in input un argomento generico rappresentante una qualsiasi espressione logica
    /// e resituisce il suo albero di sintassi.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>'T</code> expression
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>AST<'T></code>
    ///
    val parse: string -> AST<string>

    ///
    /// Questa funzione prende in input una formula e ne restituisce la lista di tutte le sue sotto-formule.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST<string></code> expression
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>AST<'T> list</code>
    ///
    val subFormulas: AST<string> -> AST<string> list

    ///
    /// Questa funzione prende in input un albero di sintassi di una espressione logica e ne restituisce una valutazione booleana delle costanti
    /// o quantificatori in esso presenti valutando, per ogni nodo, il risultato delle operazioni booleane.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST<'T></code> ast
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>bool list</code>
    ///
    val evaluate: AST<'T> -> bool list
