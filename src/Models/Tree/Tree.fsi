namespace Models

open Models.Types

module Tree =
    ///
    /// Questa funzione prende in input un albero binario di ricerca <code>Tree<'T></code> e restituisce una lista
    /// di elementi ottenuta tramite ricerca simmetrica.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Tree<'T></code> ast
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>'T list</code>
    ///
    val inorder: Tree<'T> -> 'T list

    ///
    /// Questa funzione prende in input un albero binario di ricerca <code>Tree<'T></code> e restituisce una lista
    /// di elementi ottenuta tramite ricerca in ordine anticipato.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Tree<'T></code> ast
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>'T list</code>
    ///
    val preorder: Tree<'T> -> 'T list

    ///
    /// Questa funzione prende in input un albero binario di ricerca <code>Tree<'T></code> e restituisce una lista
    /// di elementi ottenuta tramite ricerca in ordine posticipato.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Tree<'T></code> ast
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>'T list</code>
    ///
    val postorder: Tree<'T> -> 'T list
