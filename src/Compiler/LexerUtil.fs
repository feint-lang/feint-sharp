module Feint.Compiler.LexerUtil

open FSharp.Text.Lexing

open Feint.Compiler.Parser

exception LexerErr of string

let raiseErr msg = raise (LexerErr msg)

let lexeme (lexbuf: LexBuffer<char>) = LexBuffer<char>.LexemeString lexbuf

// Increment line number
let newLine (lexbuf: LexBuffer<_>) =
    lexbuf.StartPos <- lexbuf.EndPos.NextLine
    lexbuf.EndPos <- lexbuf.StartPos

// Increment line number N times
let rec newLines lexbuf count =
    match count with
    | 0 -> ()
    | _ ->
        newLine lexbuf
        newLines lexbuf (count - 1)

// Count newlines in string
let countNewlines str = (String.filter ((=) '\n') str).Length

let newLinesFromLexeme lexbuf =
    countNewlines (lexeme lexbuf) |> newLines lexbuf

// Get current line number as string
// NOTE: Positions are 0-based
let lineNo (lexbuf: LexBuffer<char>) = $"{lexbuf.StartPos.Line + 1}"

// Get current column number as string
// NOTE: Positions are 0-based
let colNo (lexbuf: LexBuffer<char>) = $"{lexbuf.StartPos.Column + 1}"

// Format current line and column numbers
let formatPos lexbuf =
    $"line {lineNo lexbuf} at column {colNo lexbuf}"

// Format current line and column numbers as line:column
let formatPosShort lexbuf = $"{lineNo lexbuf}:{colNo lexbuf}"

// Keywords
let keywords =
    Map.ofList
        [ ("nil", NIL)
          ("true", TRUE)
          ("false", FALSE)
          ("block", BLOCK)
          ("if", IF)
          ("else", ELSE)
          ("match", MATCH)
          ("$print", PRINT) ]

let getKeyword word =
    match keywords.TryGetValue word with
    | true, token -> token
    | _ -> IDENT word

// Operators
let operators =
    Map.ofList
        [
          // Unary
          ("!", BANG)
          ("!!", BANG_BANG)
          // Binary
          ("^", CARET)
          ("*", STAR)
          ("/", SLASH)
          ("//", DOUBLE_SLASH)
          ("%", PERCENT)
          ("+", PLUS)
          ("-", DASH)
          // Short Circuit
          ("&&", AND)
          ("((", OR)
          ("??", NIL_OR)
          // Compare
          ("$$", DOLLAR_DOLLAR)
          ("$!", DOLLAR_NOT)
          ("===", EQ_EQ_EQ)
          ("!==", NOT_EQ_EQ)
          ("==", EQ_EQ)
          ("!=", NOT_EQ)
          ("<", LT)
          ("<=", LT_OR_EQ)
          (">", GT)
          (">=", GT_OR_EQ)
          // In Place
          ("*=", MUL_EQ)
          ("/=", DIV_EQ)
          ("+=", ADD_EQ)
          ("-=", SUB_EQ)
          // Assignment
          ("=", EQ)
          ("<-", FEED)
          // Other
          (".", DOT)
          (",", COMMA) ]

let processStr lexbuf =
    let lex = lexeme lexbuf
    newLines lexbuf (countNewlines lex)

    // NOTE: Remove trailing quote from lexeme
    let str = lex.Substring(0, lex.Length - 1)
    let peekStr = (str.Substring 1) + "\\"

    let buf = new System.Text.StringBuilder()
    let mutable skipNextC = false

    for (c, d) in (Seq.zip str peekStr) do
        if c = '\\' then
            // Handle escaped char d.
            skipNextC <- true

            match d with
            | '\n' -> ()
            | '\\' -> buf.Append('\\') |> ignore // backslash
            | '0' -> buf.Append('\x00') |> ignore // null
            | 'a' -> buf.Append('\a') |> ignore // bell
            | 'b' -> buf.Append('\b') |> ignore // backspace
            | 'f' -> buf.Append('\f') |> ignore // form feed
            | 'n' -> buf.Append('\n') |> ignore // newline
            | 'r' -> buf.Append('\r') |> ignore // carriage return
            | 't' -> buf.Append('\t') |> ignore // tab
            | 'v' -> buf.Append('\v') |> ignore // vertical tab
            // Unescape escaped quotes
            | '"' -> buf.Append('"') |> ignore
            | '\'' -> buf.Append('\'') |> ignore
            // Other escaped chars resolve to the original escape sequence
            | _ ->
                buf.Append('\\') |> ignore
                buf.Append(d) |> ignore

        else if skipNextC then
            // Skip c because it was escaped and handled above on the
            // previous iteration.
            skipNextC <- false

        else
            // Handle unescaped char c.
            buf.Append(c) |> ignore

    buf.ToString()
