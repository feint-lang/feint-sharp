module Feint.Compiler.LexerUtil

let charsToString chars = new string (chars |> Seq.toArray)

let isAscii c = System.Char.IsAsciiLetterOrDigit c
let isIdentChar c = isAscii c || c = '_'

let isDigit c = System.Char.IsAsciiDigit c
let isHexDigit c = System.Char.IsAsciiHexDigit c
let isFloatIndicator c = c = '.' || c = 'e' || c = 'E'

let isDot c = c = '.'
let isNotNewline c = c <> '\n'

let processEscapedChar (d: char) =
    match d with
    | '\\' -> [ '\\' ] // backslash
    | '0' -> [ '\000' ] // null
    | 'a' -> [ '\a' ] // bell
    | 'b' -> [ '\b' ] // backspace
    | 'f' -> [ '\f' ] // form feed
    | 'n' -> [ '\n' ] // newline
    | 'r' -> [ '\r' ] // carriage return
    | 't' -> [ '\t' ] // tab
    | 'v' -> [ '\v' ] // vertical tab
    | '"' -> [ '"' ] // unescape escaped double quote
    | '\'' -> [ '\'' ] // unescape escaped single quote
    | _ -> [ '\\'; d ] // original escape sequence
