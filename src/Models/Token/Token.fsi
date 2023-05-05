namespace Models

open Models.Enums
open Models.Types

module Token =
    ///
    /// Questa funzione restituisce la lista dei tipi di Token (<code>TokenType</code>)
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>TokenType list</code>
    ///
    val types: unit -> TokenType list;

    ///
    /// Questa funzione restituisce la regola associata ad uno specifico tipo di Token.
    /// La regola è rappresentata da una stringa che identifica un pattern _Regex_.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>TokenType</code> tokenType
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>string</code>
    ///
    val rule: TokenType -> string;

    ///
    /// Questa funzione restituisce l'intera grammatica sotto forma di mappa <<code>TokenType</code>, <code>string</code>>.
    /// Le chiavi della mappa sono rappresentate dai tipi di token, i valori dalle regole associate a ciascun tipo.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>Map<TokenType, string></code>
    ///
    val grammar: unit -> Map<TokenType, string>;

    ///
    /// Questa funzione restituisce il <code>Token</code> corretto in base al tipo di carattere o stringa in input.
    /// Es. <code>"&" -> Token.And</code>.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>string</code> input
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>Token<string></code>
    ///
    val parse: string -> Token<string>;

    ///
    /// Questa funzione restituisce la stringa corrispondente al <code>Token</code> in input.
    /// Es. <code>Token.And -> "&"</code>.
    ///
    /// ---
    ///
    /// **Parameters**
    ///
    /// - <code>Token<'T></code> token
    ///
    /// ---
    ///
    /// **Returns**
    ///
    /// - <code>string</code>
    ///
    val toString: Token<'T> -> string;