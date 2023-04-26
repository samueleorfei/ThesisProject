namespace Models

type TokenType =
    | Identifier
    | Whitespace
    | Separator
    | OpenParenthesis
    | ClosedParenthesis
    | Operator

type Token =
    { Type: TokenType
      Value: string
      Start: int
      End: int
      Index: int }
