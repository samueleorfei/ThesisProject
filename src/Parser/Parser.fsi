namespace Parsing

open Models.Types

module Parser =
    type ParseError = string

    val parse: Token<string> list -> Result<(AST<string> * Token<string> list), ParseError>;