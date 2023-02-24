module Feint.Compiler.Lexer

open System
open System.Collections.Generic

open Errors
open LexerUtil
open Token

type Result =
    | Token of PosToken
    | EOF
    | SyntaxErr of SyntaxErr

type Lexer(fileName: string, stream: IO.TextReader) =
    let stream = stream
    let mutable queue = new Queue<char>()

    // Last character read from queue.
    let mutable lastChar = None

    // NOTE: Positions are 1-based.
    let mutable line = 1u
    let mutable col = 0u

    /// Start position of the current lexeme/token.
    let mutable startPos = (line, col)

    /// End position of the current lexeme/token.
    let mutable endPos = (line, col)

    /// Fill the queue IF it's empty by reading the next line from the
    /// stream. All newline styles are normalized to `\n` to simplify
    /// lexing.
    ///
    /// Returns a flag indicating whether the queue contains any items.
    ///
    /// TODO: This could be made more robust by reading N bytes into a
    ///       preallocated buffer.
    let fill () =
        if queue.Count = 0 then
            match stream.ReadLine() with
            | null -> ()
            | line ->
                Seq.iter queue.Enqueue line
                queue.Enqueue '\n'

        queue.Count > 0

    /// Peek at next character in queue.
    let peek () =
        let c = ref '\000'

        if queue.TryPeek(c) then Some(c.Value)
        else if fill () && queue.TryPeek(c) then Some(c.Value)
        else None

    /// Read next character from queue.
    let read () =
        let c = ref '\000'

        if queue.TryDequeue(c) then Some(c.Value)
        else if fill () && queue.TryDequeue(c) then Some c.Value
        else None

    /// Get next character, set last character read, and increment line
    /// and/or column number.
    let next () =
        lastChar <- read ()

        match lastChar with
        | None -> None
        | Some c ->
            match c with
            | '\n' ->
                line <- line + 1u
                col <- 0u
                Some '\n'
            | _ ->
                col <- col + 1u
                Some(char c)

    let nextIf predicate =
        match peek () with
        | Some c ->
            match predicate c with
            | true -> next ()
            | false -> None
        | None -> None

    let rec nextWhile predicate =
        match peek () with
        | None -> []
        | Some c ->
            match predicate c with
            | false -> []
            | true ->
                match next () with
                | None -> []
                | Some c -> [ c ] @ nextWhile (predicate)

    let rec skipWhile predicate =
        match peek () with
        | None -> ()
        | Some c ->
            match predicate c with
            | false -> ()
            | true ->
                next () |> ignore
                skipWhile (predicate)

    let skipWhitespace () = skipWhile (fun c -> c = ' ')

    // Error Handlers --------------------------------------------------

    let makeSyntaxErr kind =
        endPos <- (line, col)

        SyntaxErr
            { fileName = fileName
              startPos = startPos
              endPos = endPos
              kind = kind }

    // Handlers --------------------------------------------------------

    let makeToken token =
        endPos <- (line, col)

        Token(
            { startPos = startPos
              endPos = endPos
              token = token }
        )

    let handleInt firstDigit =
        let otherDigits = nextWhile (fun c -> c >= '0' && c <= '9')
        let str = firstDigit :: otherDigits |> List.toArray |> String
        makeToken (Int(bigint.Parse str))

    let handleStr quoteChar =
        let rec loop chars =
            match next (), peek () with
            | None, _ -> chars
            | Some c, _ when c = quoteChar -> chars
            | Some '\\', Some d ->
                next () |> ignore
                processEscapedChar d @ loop chars
            | Some c, _ -> [ c ] @ loop chars

        let chars = loop []
        let terminated = lastChar = Some quoteChar
        let str = chars |> Seq.toArray |> String

        if terminated then
            makeToken (Str str)
        else
            makeSyntaxErr (UnterminatedStringLiteral $"{quoteChar}{str}")

    // API -------------------------------------------------------------

    member _.pos = (startPos, endPos)

    member _.nextToken() =
        skipWhitespace ()

        match next () with
        | None -> EOF
        | Some c ->
            startPos <- (line, col)

            // Default end position for token. Will be updated if more
            // characters are consumed.
            endPos <- (line, col)

            match c with
            // Groupings -----------------------------------------------
            | '(' -> makeToken LParen
            | ')' -> makeToken RParen
            | '[' -> makeToken LBrace
            | ']' -> makeToken RBrace
            | '{' -> makeToken LBracket
            | '}' -> makeToken RBracket
            // Binary Operators ----------------------------------------
            | '^' -> makeToken Caret
            | '*' -> makeToken Star
            | '/' -> makeToken Slash
            // TODO:
            // | Some '//' -> makeToken this.pos Slash
            | '+' -> makeToken Plus
            | '-' -> makeToken Dash
            | '=' ->
                match nextIf (fun d -> d = '=') with
                | Some _ -> makeToken EqEq
                | _ -> makeToken Eq
            // Types ---------------------------------------------------
            | firstDigit when System.Char.IsDigit(c) -> handleInt firstDigit
            | quoteChar when c = '"' || c = '\'' -> handleStr quoteChar
            // Errors --------------------------------------------------
            | c ->
                match c with
                | '\t' -> makeSyntaxErr Tab
                | unhandled -> makeSyntaxErr (UnhandledChar unhandled)

let fromText (fileName: string) (text: string) =
    new Lexer(fileName, new IO.StringReader(text))

let fromFile (fileName: string) =
    new Lexer(fileName, new IO.StreamReader(fileName))

let tokensFromText (fileName: string) (text: string) =
    let lexer = fromText fileName text

    let rec loop tokens =
        let result = lexer.nextToken ()

        match result with
        | Token _ -> [ result ] @ (loop tokens)
        | EOF -> [ EOF ]
        | SyntaxErr _ -> [ result ]

    loop []

let formatResult fileName text result =
    match result with
    | Token token -> formatPosToken token
    | EOF -> "EOF"
    | SyntaxErr err -> formatSyntaxErr err
