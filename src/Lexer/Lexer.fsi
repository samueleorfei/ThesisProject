namespace Lexing

open Models.Types

module Lexer =
    val tokenize: string -> Token<string> list;