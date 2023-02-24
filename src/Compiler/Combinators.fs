module Feint.Compiler.Combinators

open System

type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> Result<'a * string>)

let run parser input =
    let (Parser p) = parser
    p input

let andThen p1 p2 =
    let parser input =
        match run p1 input with
        | Success(v1, input') ->
            match run p2 input' with
            | Success(v2, input') -> Success((v1, v2), input')
            | Failure f -> Failure f
        | Failure f -> Failure f

    Parser parser

let (.>>.) = andThen

let orElse p1 p2 =
    let parser input =
        match run p1 input with
        | Success(v, input') -> Success(v, input')
        | Failure _ ->
            match run p2 input with
            | Success(v, input') -> Success(v, input')
            | Failure f -> Failure f

    Parser parser

let (<|>) = orElse

let map parser mapper =
    let parser input =
        match run parser input with
        | Success(v, input') -> Success(mapper v, input')
        | Failure f -> Failure f

    Parser parser

let (|>>) = map

let choice parsers = parsers |> List.reduce orElse

let rec sequence parsers =
    let parser input =
        match parsers with
        | [] -> Success([], input)
        | (Parser p) :: [] ->
            match p input with
            | Failure f -> Failure f
            | Success(r, input') -> Success([ r ], input')
        | p1 :: p2 :: rest ->
            match run (p1 .>>. p2) input with
            | Failure f -> Failure f
            | Success((v1, v2), input') ->
                match run (sequence rest) input' with
                | Failure f -> Failure f
                | Success(tail, _) -> Success([ v1; v2 ] @ tail, input')

    Parser parser

let matchChar c =
    let parser input =
        match input with
        | null
        | "" -> Failure "Unexpected EOF"
        | _ ->
            match input.[0] with
            | c' when c' = c -> Success(c, input.[1..])
            | d -> Failure $"Expected char '{c}'; got '{d}'"

    Parser parser

let matchCharI c = matchChar (Char.ToLower c)

let anyCharOf chars = chars |> List.map matchChar |> choice

let anyCharOfI chars =
    chars |> List.map Char.ToLower |> anyCharOf

let zero = matchChar '0'
let digit = anyCharOf [ '0' .. '9' ]
let natural = anyCharOf [ '1' .. '9' ]
let binDigit = anyCharOf [ '0' .. '1' ]
let octDigit = anyCharOf [ '0' .. '7' ]
let hexLetter = anyCharOfI [ 'a' .. 'f' ]
let hexDigit = choice [ digit; hexLetter ]

let asciiLower = anyCharOf [ 'a' .. 'z' ]
let asciiUpper = anyCharOf [ 'A' .. 'Z' ]
let asciiLetter = choice [ asciiLower; asciiUpper ]
let asciiLetterOrDigit = [ asciiLetter; digit ]
