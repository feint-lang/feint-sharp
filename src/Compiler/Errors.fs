module Feint.Compiler.Errors

open System

open Tokens
open LexerUtil

type SyntaxErrKind =
    | NotImplemented of string // placeholder
    | UnexpectedWhitespace
    // Characters
    | Tab
    | UnhandledChar of char
    // Groupings
    | MismatchedBracket of
        openChar: char *
        openPos: Pos *
        expectedCloseChar: char *
        actualCloseChar: char *
        closePos: Pos
    | UnmatchedClosingBracket of closeChar: char
    // Indentation
    | ExpectedIndent of level: uint
    | UnexpectedIndent of level: uint
    | InvalidIndent of count: uint
    // Numbers
    | InvalidNumber of msg: string
    | InvalidFloat of msg: string
    | InvalidIntPrefix of prefix: char
    // Strings
    | UnterminatedLiteralStr of string
    | UnterminatedFormatStr of string

type SyntaxErr =
    { fileName: string
      text: string option
      span: Span
      kind: SyntaxErrKind }

type ParseErrKind =
    | ExpectedToken of Token
    | UnexpectedEOF

// TODO: Add file name and positions
type ParseErr = { kind: ParseErrKind }

let makeSyntaxErr fileName text startPos endPos kind : SyntaxErr =
    { fileName = fileName
      text = text
      span = (startPos, endPos)
      kind = kind }

let makeParseErr kind : ParseErr = { kind = kind }

let formatSyntaxErrKind (kind: SyntaxErrKind) =
    match kind with
    | NotImplemented msg -> $"Not implemented: {msg}"
    | UnexpectedWhitespace -> "Unexpected whitespace"
    // Characters
    | Tab -> "TAB cannot be used for indentation or whitespace"
    | UnhandledChar c -> $"Unhandled character: {c}"
    // Groupings
    | MismatchedBracket(openChar, openPos, expectedCloseChar, actualCloseChar, closePos) ->
        let (openLine, openCol) = openPos
        let (closeLine, closeCol) = closePos
        $"Mismatched bracket: expected closing {expectedCloseChar} at {closeLine}:{closeCol} to match opening {openChar} at {openLine}:{openCol}; got {actualCloseChar}"
    | UnmatchedClosingBracket c -> $"Unmatched closing bracket: {c}"
    // Indentation
    | ExpectedIndent level -> $"Expected indent to level {level}"
    | UnexpectedIndent level -> $"Unexpected indent to level {level}"
    | InvalidIndent count ->
        let ess = if count = 1u then "" else "s"
        $"Invalid indent of {count} space{ess} (should be a multiple of 4)"
    // Numbers
    | InvalidNumber msg -> $"Invalid number: {msg}"
    | InvalidFloat msg -> $"Invalid float: {msg}"
    | InvalidIntPrefix prefix -> $"Invalid integer prefix: 0{prefix}"
    // Strings
    | UnterminatedLiteralStr s -> $"Unterminated string: {s}"
    | UnterminatedFormatStr s -> $"Unterminated format string: {s}"

let formatSyntaxErr (err: SyntaxErr) =
    let { SyntaxErr.kind = kind
          fileName = fileName
          text = maybeText
          span = ((sLine, sCol), (_, eCol)) } =
        err

    let stream: IO.TextReader =
        match maybeText with
        | Some text -> new IO.StringReader(text)
        | None -> new IO.StreamReader(fileName)

    let mutable lineNo = 0u
    let mutable sourceLine = "<line not found>"

    while lineNo <> sLine do
        sourceLine <- stream.ReadLine()
        lineNo <- lineNo + 1u

    let width = int (sCol - 1u)
    let space = Array.create width ' ' |> stringFromChars

    let width = int (eCol - sCol + 1u)
    let indicator = Array.create width '^' |> stringFromChars

    let lines =
        [ $"Syntax error in '{fileName}' on line {sLine} at column {sCol}:"
          ""
          " |"
          $" | {sourceLine}"
          $" | {space}{indicator}"
          " |"
          ""
          formatSyntaxErrKind kind ]

    String.Join("\n", lines)

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
