module Feint.Compiler.Errors

type SyntaxErrKind =
    | Tab
    | UnhandledChar of char
    | UnterminatedStringLiteral of string

type SyntaxErr =
    { fileName: string
      startPos: (uint * uint)
      endPos: (uint * uint)
      kind: SyntaxErrKind }

let formatSyntaxErrKind (kind: SyntaxErrKind) =
    match kind with
    | Tab -> "TAB cannot be used for indentation or whitespace"
    | UnhandledChar c -> $"unhandled character: {c}"
    | UnterminatedStringLiteral s -> $"unterminated string literal: {s}"

// TODO: Show line and highlight error range
let formatSyntaxErr err =
    let { kind = kind
          fileName = fileName
          startPos = (sLine, sCol)
          endPos = (eLine, eCol) } =
        err

    let msg = $"Syntax error in '{fileName}' on line {sLine} at column {sCol}: {kind}"
    msg
