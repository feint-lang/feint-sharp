module Feint.Compiler.LexerUtil

let charsToString chars = new string (chars |> Seq.toArray)

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
