namespace Interpreter

open Interpreter.Types
open Models.Types

///
/// Il modulo <code>Parser</code> contiene la definizione delle funzioni che servono a costruire un
/// albero di sintassi (<code>Formula</code>) partendo da una lista di <code>Token</code> generata dal _Lexer_.
///
module Parser =
    type ParseError = string

    ///
    /// Questa funzione riceve in input una lista di <code>Token</code> generata dal <code>Lexer</code> e
    /// restituisce la rappresentazione interna dell'espressione booleana data da un albero di sintassi (<code>Formula</code>)
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Token list</code> input
    ///
    /// ---
    ///
    /// **Return**
    ///
    /// - <code>Result<(Formula * Token list), ParseError></code>
    ///
    val parse: Token list -> Result<(Formula * Token list), ParseError>
