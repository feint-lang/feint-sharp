module Feint.Compiler.Driver

open System
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
        let token = Lexer.read lexbuf
        eprintfn "%A @ %s" token (formatPosShort lexbuf)

        if not lexbuf.IsPastEndOfStream then
            read ()

    read ()

let printTokensFromText text fileName =
    printTokens (lexbufForText text fileName)

let printTokensFromFile fileName = printTokens (lexbufForFile fileName)

// Parsing -------------------------------------------------------------

type ParseResult =
    | Statements of Ast.Statement list
    | Error of string

let tryParse lexbuf fileName =
    try
        let statements = Parser.Module Lexer.read lexbuf
        Statements(statements)
    with
    | LexerUtil.LexerErr msg ->
        let pos = formatPos lexbuf
        Error $"Syntax error in {fileName} on {pos}: {msg}"
    | exc ->
        let pos = formatPos lexbuf
        Error $"Parse error in {fileName} on {pos}:\n\n{exc}"

let parseText text fileName =
    let lexbuf = lexbufForText text fileName
    tryParse lexbuf fileName

let parseFile fileName =
    let lexbuf = lexbufForFile fileName
    tryParse lexbuf fileName

// AST -----------------------------------------------------------------

let handleParseResultForPrintAst =
    function
    | Statements statements -> Console.Write(Ast.formatStatements statements 0)
    | Error msg -> Console.Error.WriteLine msg

let printAstFromText text fileName =
    parseText text fileName |> handleParseResultForPrintAst

let printAstFromFile fileName =
    parseFile fileName |> handleParseResultForPrintAst
