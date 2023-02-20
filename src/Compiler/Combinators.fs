module Feint.Compiler.Combinators

open System

type Result<'a> =
    | Success of value: 'a
    | Failure of message: string

type Parser<'a> = Parser of parse: (string -> Result<'a * string>)

// Entrypoint ----------------------------------------------------------

let run parser input =
    let (Parser parse) = parser
    parse input

// Primitives ----------------------------------------------------------

let matchChar c =
    Parser(fun input ->
        match input with
        | null
        | "" -> Failure "Unexpected EOF"
        | _ ->
            match input.[0] with
            | c' when c' = c -> Success(c, input.[1..])
            | d -> Failure $"Expected char '{c}'; got '{d}'")

let matchCharI c = matchChar (Char.ToLower c)

let andThen p1 p2 =
    Parser(fun input ->
        match run p1 input with
        | Success(v1, input') ->
            match run p2 input' with
            | Success(v2, input'') -> Success((v1, v2), input'')
            | Failure f -> Failure f
        | Failure f -> Failure f)

let (.>>.) = andThen

let orElse p1 p2 =
    Parser(fun input ->
        match run p1 input with
        | Success(v, input') -> Success(v, input')
        | Failure _ ->
            match run p2 input with
            | Success(v, input') -> Success(v, input')
            | Failure f -> Failure f)

let (<|>) = orElse

let map parser mapping =
    Parser(fun input ->
        match run parser input with
        | Success(v, input') -> Success(mapping v, input')
        | Failure f -> Failure f)

let (|>>) = map

// ---------------------------------------------------------------------

let anyOf parsers = parsers |> Seq.reduce orElse
let choice = anyOf

let zeroOrMore x = ()
let oneOrMore x = ()

let sequence parsers =
    Parser(fun input ->
        let rec loop results parsers parser input =
            match run parser input with
            | Failure f -> Failure f
            | Success(v, input') ->
                match parsers with
                | [] -> Success(results @ [ v ], input')
                | parser' :: parsers' -> loop (results @ [ v ]) parsers' parser' input'

        match parsers with
        | [] -> Success([], input)
        | parser :: parsers' -> loop [] parsers' parser input)

// Characters ----------------------------------------------------------

let anyCharOf chars = chars |> Seq.map matchChar |> anyOf
let anyCharOfI chars = chars |> Seq.map matchCharI |> anyOf

let asciiLower = anyCharOf [ 'a' .. 'z' ]
let asciiUpper = anyCharOf [ 'A' .. 'Z' ]
let asciiLetter = anyOf [ asciiLower; asciiUpper ]

// Numbers -------------------------------------------------------------

let zero = matchChar '0'
let digit = anyCharOf [ '0' .. '9' ]
let natural = anyCharOf [ '1' .. '9' ]
let binDigit = anyCharOf [ '0' .. '1' ]
let octDigit = anyCharOf [ '0' .. '7' ]
let hexLetter = anyCharOfI [ 'a' .. 'f' ]
let hexDigit = anyOf [ digit; hexLetter ]

// Identifiers ---------------------------------------------------------

let asciiLetterOrDigit = anyOf [ asciiLetter; digit ]

// Keywords ------------------------------------------------------------

let str (str: string) =
    str |> Seq.map matchChar |> sequence |>> Seq.toArray |>> String

// Strings -------------------------------------------------------------

let string = sequence [ matchChar '"'; matchChar '"' ]
