module Feint.Compiler.Tokens

open System.Globalization

open LexerUtil

type Token =
    | EndOfStatement
    // Comments --------------------------------------------------------
    | Comment of string
    | DocComment of string
    // Scopes ----------------------------------------------------------
    | ScopeStart
    | FuncStart
    | ScopeEnd
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
    | FormatStr of string
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

let intFromChars chars =
    stringFromChars chars |> bigint.Parse |> Int

let intFromHexChars chars =
    // NOTE: The leading 0 is necessary to avoid the string being
    //       parsed as negative when the leading char is 8-F.
    let digits = stringFromChars ([ '0' ] @ chars)
    let value = bigint.Parse(digits, NumberStyles.AllowHexSpecifier)
    Int value

let floatFromChars chars = stringFromChars chars |> float |> Float

/// Token with its span in the source stream.
type SpanToken =
    { span: (uint * uint) * (uint * uint)
      token: Token }

let makePosToken startPos endPos token =
    { span = (startPos, endPos)
      token = token }

let keywordToken word =
    match word with
    | "nil" -> Some Nil
    | "true" -> Some True
    | "false" -> Some False
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
    let { span = span; token = token } = token
    let (sLine, sCol), (eLine, eCol) = span
    $"{sLine}:{sCol} -> {eLine}:{eCol} = {formatToken token}"
