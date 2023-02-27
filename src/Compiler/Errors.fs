module Feint.Compiler.Errors

open Feint.Compiler.Tokens

type SyntaxErrKind =
    | NotImplemented of string // placeholder
    // Characters
    | Tab
    | UnhandledChar of char
    // Indentation
    | ExpectedIndent of level: uint
    | UnexpectedIndent
    | InvalidIndent of count: uint
    // Numbers
    | InvalidNumber of msg: string
    | InvalidFloat of msg: string
    // Strings
    | UnterminatedLiteralStr of string
    | UnterminatedFormatStr of string

type SyntaxErr =
    { fileName: string
      startPos: uint * uint
      endPos: uint * uint
      kind: SyntaxErrKind }

type ParseErrKind =
    | ExpectedToken of Token
    | UnexpectedEOF

// TODO: Add file name and positions
type ParseErr = { kind: ParseErrKind }

let makeSyntaxErr fileName startPos endPos kind : SyntaxErr =
    { fileName = fileName
      startPos = startPos
      endPos = endPos
      kind = kind }

let makeParseErr kind : ParseErr = { kind = kind }

let formatSyntaxErrKind (kind: SyntaxErrKind) =
    match kind with
    | NotImplemented msg -> $"not implemented: {msg}"
    // Characters
    | Tab -> "TAB cannot be used for indentation or whitespace"
    | UnhandledChar c -> $"unhandled character: {c}"
    // Indentation
    | ExpectedIndent level -> $"expected indent level {level}"
    | UnexpectedIndent -> "unexpected indent"
    | InvalidIndent count ->
        let ess = if count = 1u then "" else "s"
        $"invalid indent of {count} space{ess} (should be a multiple of 4)"
    // Numbers
    | InvalidNumber msg -> $"invalid number: {msg}"
    | InvalidFloat msg -> $"invalid float: {msg}"
    // Strings
    | UnterminatedLiteralStr s -> $"unterminated string literal: {s}"
    | UnterminatedFormatStr s -> $"unterminated format string: {s}"

// TODO: Show line and highlight error range
let formatSyntaxErr (err: SyntaxErr) =
    let { SyntaxErr.kind = kind
          fileName = fileName
          startPos = (sLine, sCol)
          endPos = (eLine, eCol) } =
        err

    let kind = formatSyntaxErrKind kind
    let msg = $"Syntax error in '{fileName}' on line {sLine} at column {sCol}: {kind}"
    msg

let formatParseErrKind (kind: ParseErrKind) =
    match kind with
    | ExpectedToken token -> $"expected token: {token}"
    | UnexpectedEOF -> "unexpected EOF"

// TODO: Show line and highlight error range
let formatParseErr (err: ParseErr) =
    let { ParseErr.kind = kind } = err
    let kind = formatParseErrKind kind
    let msg = $"Parser error: {kind}"
    msg
