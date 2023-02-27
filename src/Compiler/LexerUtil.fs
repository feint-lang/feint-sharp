module Feint.Compiler.LexerUtil

open System

let stringFromChars chars = new string (chars |> Seq.toArray)

let isAscii c = Char.IsAsciiLetterOrDigit c
let isAsciiLetter c = Char.IsAsciiLetter c
let isIdentChar c = isAscii c || c = '_'

let isDigitBase2 c = [| '0' .. '1' |] |> Array.contains c
let isDigitBase8 c = [| '0' .. '7' |] |> Array.contains c
let isDigitBase10 c = Char.IsAsciiDigit c
let isDigitBase16 c = Char.IsAsciiHexDigit c

let isFloatIndicator c = [| '.'; 'e' |] |> Array.contains c

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
