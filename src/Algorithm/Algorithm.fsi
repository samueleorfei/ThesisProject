namespace Algorithm

open Models.Types

/// Il modulo **Algorithm** contiene la definizione delle funzioni per eseguire il calcolo di
/// refutazione nella logica di Godel-Dummet.
module Algorithm =
    ///
    /// La funzione <code>calculate</code> riceve in input una formula che rappresenta l'obiettivo del processo dimostrativo,
    /// una lista di formule che constituiscono le premesse da cui partire e restituisce una lista di formule che rappresentano
    /// gli assiomi validi
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>AST<string></code> goal
    /// - <code>AST<string> list</code> premises
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>AST<string> list</code>
    ///
    val calculate: AST<string> * AST<string> list -> AST<string> list
