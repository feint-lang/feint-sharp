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

    /// Skip next character in queue unconditionally.
    let skipNext () = next () |> ignore

    /// Skip next character then peek at next character in queue.
    let skipNextPeek () =
        skipNext ()
        peek ()

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
                skipNext ()
                skipWhile (predicate)

    let skipWhitespace () = skipWhile (fun c -> c = ' ')

    // Result Constructors ---------------------------------------------

    let makeToken token =
        endPos <- (line, col)

        Token(
            { startPos = startPos
              endPos = endPos
              token = token }
        )

    /// Simplifies the creation of 2-char operator tokens.
    let skipNextMakeToken token =
        skipNext ()
        makeToken token

    let makeSyntaxErr kind =
        endPos <- (line, col)

        SyntaxErr
            { fileName = fileName
              startPos = startPos
              endPos = endPos
              kind = kind }

    // Scanners --------------------------------------------------------
    //
    // Scanners scan forward from the current position to produce a
    // token from one or more characters.

    // TODO: Handle floats
    let scanNumber firstDigit =
        let otherDigits = nextWhile (fun c -> c >= '0' && c <= '9')
        let digits = charsToString (firstDigit :: otherDigits)
        makeToken (Int(bigint.Parse digits))

    let scanStr quoteChar =
        let rec loop chars =
            match next (), peek () with
            | None, _ -> chars
            | Some c, _ when c = quoteChar -> chars
            | Some '\\', Some d ->
                skipNext ()
                processEscapedChar d @ loop chars
            | Some c, _ -> [ c ] @ loop chars

        let str = charsToString (loop [])
        let terminated = lastChar = Some quoteChar

        if terminated then
            makeToken (Str str)
        else
            makeSyntaxErr (UnterminatedStringLiteral $"{quoteChar}{str}")

    let scanKeywordOrIdent firstChar =
        let otherChars = nextWhile (fun c -> Char.IsAsciiLetterOrDigit c || c = '_')
        let word = charsToString (firstChar :: otherChars)

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (Ident word)

    let scanSpecialKeywordOrIdent firstChar =
        let otherChars = nextWhile (fun c -> Char.IsAsciiLetterOrDigit c || c = '_')
        let word = charsToString (firstChar :: otherChars)

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (SpecialIdent word)

    // API -------------------------------------------------------------

    member _.pos = (startPos, endPos)

    member _.nextToken() =
        skipWhitespace ()

        match next (), peek () with
        | None, _ -> EOF
        | Some c, d ->
            startPos <- (line, col)

            // Default end position for token. Will be updated if more
            // characters are consumed.
            endPos <- (line, col)

            match c, d with
            // 2-char Tokens ===========================================

            // Scopes --------------------------------------------------
            | '-', Some '>' -> skipNextMakeToken ScopeStart
            | '=', Some '>' -> skipNextMakeToken FuncStart

            // Misc ----------------------------------------------------
            | '.', Some '.' ->
                match skipNextPeek () with
                | Some '.' -> skipNextMakeToken Ellipsis
                | _ -> makeToken DotDot

            // Unary Operators -----------------------------------------
            | '!', Some '!' -> skipNextMakeToken BangBang

            // Doc comment or floor div
            | '/', Some '/' -> skipNextMakeToken DoubleSlash

            // Comparison Operators ------------------------------------
            | '=', Some '=' -> skipNextMakeToken EqEq
            | '!', Some '=' -> skipNextMakeToken NotEq
            | '~', Some '~' -> skipNextMakeToken TildeTilde
            | '!', Some '~' -> skipNextMakeToken BangTilde

            // Logic Operators -----------------------------------------
            | '&', Some '&' -> skipNextMakeToken And
            | '|', Some '|' -> skipNextMakeToken Or
            | '?', Some '?' -> skipNextMakeToken NilOr

            // Comparison Operators ------------------------------------
            | '<', Some '=' -> skipNextMakeToken LtOrEq
            | '>', Some '=' -> skipNextMakeToken GtOrEq

            // Assignment Operators ------------------------------------
            | '<', Some '-' -> skipNextMakeToken Feed

            // 1-char Tokens ===========================================

            | '\n', _ -> makeToken Newline

            // Misc ----------------------------------------------------
            | ':', _ -> makeToken Colon
            | ',', _ -> makeToken Comma
            | '.', _ -> makeToken Dot

            // Groupings -----------------------------------------------
            | '(', _ -> makeToken LParen
            | ')', _ -> makeToken RParen
            | '[', _ -> makeToken LBrace
            | ']', _ -> makeToken RBrace
            | '{', _ -> makeToken LBracket
            | '}', _ -> makeToken RBracket

            // Unary Operators -----------------------------------------
            | '!', _ -> makeToken BangBang

            // Binary Operators ----------------------------------------
            | '^', _ -> makeToken Caret
            | '*', _ -> makeToken Star
            | '/', _ -> makeToken Slash
            | '+', _ -> makeToken Plus
            | '-', _ -> makeToken Dash

            // Assignment Operators ------------------------------------
            | '=', _ -> makeToken Eq

            // Comparison Operators ------------------------------------
            | '<', _ -> makeToken Lt
            | '>', _ -> makeToken GT

            // Types ---------------------------------------------------
            | '@', _ -> makeToken Always
            | f, _ when Char.IsDigit(f) -> scanNumber f
            | q, _ when q = '"' || q = '\'' -> scanStr q

            // Keywords & Identifiers ----------------------------------
            | f, _ when Char.IsAsciiLetter(f) -> scanKeywordOrIdent f
            | '$', Some f when Char.IsAsciiLetter(f) -> scanSpecialKeywordOrIdent f

            // Errors --------------------------------------------------
            | _ ->
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
