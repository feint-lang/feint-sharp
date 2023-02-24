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
    | FuncStart
    // Misc ------------------------------------------------------------
    | Colon
    | Comma
    | Dot
    | DotDot
    | Ellipsis
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
    | Always
    | Int of bigint
    | Float of float
    | Str of string
    // Keywords --------------------------------------------------------
    | Import
    | As
    | Block
    | If
    | Else
    | Match
    | Loop
    | Break
    | Continue
    | Jump
    | Return
    | Halt
    | Print
    // Identifiers -----------------------------------------------------
    | Ident of string
    | SpecialIdent of string
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
    | TildeTilde
    | BangTilde
    | EqEq
    | NotEq
    | Lt
    | LtOrEq
    | GT
    | GtOrEq
    // Assignment Operators --------------------------------------------
    | Eq
    | Feed

type PosToken =
    { startPos: (uint * uint)
      endPos: (uint * uint)
      token: Token }

let keywordToken word =
    match word with
    | "nil" -> Some Nil
    | "true" -> Some True
    | "false" -> Some False
    //
    | "import" -> Some Import
    | "as" -> Some As
    | "block" -> Some Block
    | "if" -> Some If
    | "else" -> Some Else
    | "match" -> Some Match
    | "loop" -> Some Loop
    | "break" -> Some Break
    | "continue" -> Some Continue
    | "jump" -> Some Jump
    | "return" -> Some Return
    | "$halt" -> Some Halt
    | "$print" -> Some Print
    | _ -> None

let formatToken token = $"{token}"

let formatPosToken token =
    let { startPos = (sLine, sCol)
          endPos = (eLine, eCol)
          token = token } =
        token

    $"{sLine}:{sCol} -> {eLine}:{eCol} = {formatToken token}"
