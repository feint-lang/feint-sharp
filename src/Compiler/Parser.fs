module Feint.Compiler.Parser

open Lexer

type Parser(lexer: Lexer) =

    member this.parse() = ()

let fromText (fileName: string) (text: string) =
    let lexer = Lexer.fromText fileName text
    Parser lexer

let fromFile (path: string) =
    let lexer = Lexer.fromFile path
    Parser lexer
