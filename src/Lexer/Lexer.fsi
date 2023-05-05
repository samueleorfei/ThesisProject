namespace Lexing

open Models.Types

/// Il modulo Lexer contiene la definizione delle funzioni per eseguire la scansione di una
/// stringa e la sua suddivisione in Token gestibili da un Parser.
/// Rappresenta la prima parte di un interprete.
module Lexer =
    ///
    /// La funzione <code>tokenize</code> riceve in input una stringa e restituisce una lista di <code>Token</code>
    /// spezzando la stringa in inputa in base ad una data _grammatica_ e valutando ogni sotto-stringa ottenuta.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>string</code> input
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Token<<code>string</code>> list</code>
    ///
    val tokenize: string -> Token<string> list
