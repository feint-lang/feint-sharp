module Feint.Compiler.Driver

open System.IO
open FSharp.Text.Lexing

open Feint.Compiler
open Feint.Compiler.LexerUtil

// LexBuffer Creation --------------------------------------------------

let initLexbuf (lexbuf: LexBuffer<char>) fileName =
    lexbuf.EndPos <-
        { pos_bol = 0
          pos_fname = fileName
          pos_cnum = 0
          pos_lnum = 0
          pos_orig_lnum = 0 }

    lexbuf

let lexbufForText text fileName =
    let lexbuf = LexBuffer<char>.FromString text
    initLexbuf lexbuf fileName

let lexbufForFile fileName =
    let stream = File.OpenText(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader(stream)
    initLexbuf lexbuf fileName

// Tokenizing ----------------------------------------------------------

let printTokens lexbuf =
    let rec read () =
        match Lexer.read lexbuf with
        | Parser.EOF -> eprintfn "EOF @ %s" (formatPosShort lexbuf)
        | token ->
            eprintfn "%A @ %s" token (formatPosShort lexbuf)
            read ()

    read ()

let printTokensFromText text fileName =
    printTokens (lexbufForText text fileName)

let printTokensFromFile fileName = printTokens (lexbufForFile fileName)

// Parsing -------------------------------------------------------------

let tryParse lexbuf fileName =
    try
        Ok(Parser.Module Lexer.read lexbuf)
    with
    | LexerUtil.LexerErr msg ->
        let pos = formatPos lexbuf
        Error($"Syntax error in {fileName} on {pos}: {msg}")
    | exc ->
        let pos = formatPos lexbuf
        Error($"Parse error in {fileName} on {pos}:\n\n{exc}")

let parseText (text: string) =
    let fileName = "<text>"
    let lexbuf = lexbufForText text fileName
    tryParse lexbuf fileName

let parseFile fileName =
    let lexbuf = lexbufForFile fileName
    tryParse lexbuf fileName
