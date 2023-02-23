module Feint.Compiler.Token

type Token =
    // Whitespace ------------------------------------------------------
    | Newline
    | Indent of int
    | Dedent
    | EOF
    // Comments --------------------------------------------------------
    | Comment of string
    | DocComment of string
    // Scopes ----------------------------------------------------------
    | ScopeStart
    | ScopeEnd
    | InlineScopeStart
    | InlineScopeEnd
    | FuncStart
    | InlineFuncStart
    // Groupings -------------------------------------------------------
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBracket
    | RBracket
    // Keyword Types ---------------------------------------------------
    | Nil
    | True
    | False
    // Types -----------------------------------------------------------
    | Int of bigint
    | Float of float
    | Str of string
    // Keywords --------------------------------------------------------
    | Block
    | If
    | Else
    | Match
    | Print
    // Unary Operators -------------------------------------------------
    | Bang
    | BangBang
    // Binary Operators ------------------------------------------------
    | Caret
    | Star
    | Slash
    | DoubleSlash
    | Plus
    | Dash
    // Logic Operators -------------------------------------------------
    | And
    | Or
    | NilOr
    // Comparison Operators --------------------------------------------
    | DollarDollar
    | DollarNot
    | EqEqEq
    | NotEqEq
    | EqEq
    | NotEq
    | Lt
    | LtOrEq
    | GT
    | GtOrEq
    // In Place Operators --------------------------------------------------
    | MulEq
    | DivEq
    | AddEq
    | SubEq
    // Assignment Operators ------------------------------------------------
    | Eq
    | Feed

type PosToken =
    { startPos: (uint * uint)
      endPos: (uint * uint)
      token: Token }

let formatToken token = $"{token}"

let formatPosToken token =
    let { startPos = (sLine, sCol)
          endPos = (eLine, eCol)
          token = token } =
        token

    $"{sLine}:{sCol} -> {eLine}:{eCol} = {formatToken token}"
