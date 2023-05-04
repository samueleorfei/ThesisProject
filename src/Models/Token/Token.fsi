namespace Models

open Models.Enums
open Models.Types

module Token =    
    val types: unit -> TokenType list;

    val rule: TokenType -> string;

    val grammar: unit -> Map<TokenType, string>;

    val parse: string -> Token<string>;

    val toString: Token<'T> -> string;