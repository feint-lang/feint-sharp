module Feint.Compiler.Parser

open Combinators
open Lexer

type Parser(lexer: Lexer) =

    member this.parse() = ()
