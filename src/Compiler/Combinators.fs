module Feint.Compiler.Combinators

open System

open Errors
open Lexer

type Result<'a> =
    | Success of value: 'a
    | SyntaxErr of SyntaxErr
    | ParseErr of ParseErr

type Parser<'a> = Parser of parse: (Lexer -> Result<'a>)

// Entrypoint ----------------------------------------------------------

let run parser lexer =
    let (Parser parse) = parser
    parse lexer

// Primitives ----------------------------------------------------------

let matchToken token =
    Parser(fun lexer ->
        match lexer.nextToken () with
        | Token posToken when posToken.token = token -> Success posToken
        | Token _ -> ParseErr(makeParseErr (ExpectedToken token))
        | EOF -> ParseErr(makeParseErr UnexpectedEOF)
        | Lexer.SyntaxErr e -> SyntaxErr e)

let andThen p1 p2 =
    Parser(fun lexer ->
        match run p1 lexer with
        | Success v1 ->
            match run p2 lexer with
            | Success v2 -> Success(v1, v2)
            | SyntaxErr e -> SyntaxErr e
            | ParseErr e -> ParseErr e
        | SyntaxErr e -> SyntaxErr e
        | ParseErr e -> ParseErr e)

let (.>>.) = andThen

let orElse p1 p2 =
    Parser(fun lexer ->
        match run p1 lexer with
        | Success v -> Success v
        | SyntaxErr _
        | ParseErr _ ->
            match run p2 lexer with
            | Success s -> Success s
            | SyntaxErr e -> SyntaxErr e
            | ParseErr e -> ParseErr e)

let (<|>) = orElse

let map parser mapping =
    Parser(fun lexer ->
        match run parser lexer with
        | Success v -> Success(mapping v)
        | SyntaxErr e -> SyntaxErr e
        | ParseErr e -> ParseErr e)

let (|>>) = map

// ---------------------------------------------------------------------

let choice parsers = parsers |> Seq.reduce orElse

let zeroOrMore x = ()
let oneOrMore x = ()

let sequence parsers =
    Parser(fun lexer ->
        let rec loop results parsers =
            match Seq.isEmpty parsers with
            | true -> Success results
            | false ->
                match run (Seq.head parsers) lexer with
                | SyntaxErr e -> SyntaxErr e
                | ParseErr e -> ParseErr e
                | Success v ->
                    try
                        let tail = Seq.tail parsers
                        loop (results @ [ v ]) tail
                    with :? InvalidOperationException ->
                        Success results

        loop [] parsers)
