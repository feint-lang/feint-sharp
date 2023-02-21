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

let choice parsers = parsers |> Seq.reduce orElse

let zeroOrMore x = ()
let oneOrMore x = ()

let sequence parsers =
    Parser(fun input ->
        let rec loop results parsers input =
            match Seq.isEmpty parsers with
            | true -> Success(results, input)
            | false ->
                match run (Seq.head parsers) input with
                | Failure f -> Failure f
                | Success(v, input') ->
                    try
                        let tail = Seq.tail parsers
                        loop (results @ [ v ]) (tail) input'
                    with :? InvalidOperationException ->
                        Success(results, input')

        loop [] parsers input)

// Characters ----------------------------------------------------------

let anyOf chars = chars |> Seq.map matchChar |> choice
let anyOfI chars = chars |> Seq.map matchCharI |> choice

let asciiLower = anyOf [ 'a' .. 'z' ]
let asciiUpper = anyOf [ 'A' .. 'Z' ]
let asciiLetter = choice [ asciiLower; asciiUpper ]

// Numbers -------------------------------------------------------------

let zero = matchChar '0'
let digit = anyOf [ '0' .. '9' ]
let natural = anyOf [ '1' .. '9' ]
let binDigit = anyOf [ '0' .. '1' ]
let octDigit = anyOf [ '0' .. '7' ]
let hexLetter = anyOfI [ 'a' .. 'f' ]
let hexDigit = choice [ digit; hexLetter ]

// Identifiers ---------------------------------------------------------

let asciiLetterOrDigit = choice [ asciiLetter; digit ]

// Keywords ------------------------------------------------------------

let keyword (word: string) =
    word |> Seq.map matchChar |> sequence |>> Seq.toArray |>> String

// Strings -------------------------------------------------------------

let singleQuotedString = sequence [| matchChar '\''; matchChar '\'' |]
let doubleQuotedString = sequence [| matchChar '"'; matchChar '"' |]
