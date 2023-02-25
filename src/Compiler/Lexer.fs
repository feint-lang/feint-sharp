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

/// The `Lexer` converts a stream of chars to tokens. These tokens can
/// be retrieved by repeatedly calling the `nextToken` method until an
/// `EOF` token or `SyntaxErr` is returned. The `tokens` method can be
/// used to get all the tokens as a list of `Result`s for use in tests.
///
/// The `Lexer` is intentionally simple, concerning itself only with
/// generating tokens and detecting simple syntax errors (such as
/// unexpected characters in the input stream and unterminated string
/// literals). The `Parser` is responsible for determining whether the
/// generated tokens represent a valid program.
type Lexer(fileName: string, stream: IO.TextReader) =
    let stream = stream

    /// Chars are read from the `stream` into this queue, which is
    /// refilled as needed when `peek` and `next` are called.
    let queue = new Queue<char>()
    do queue.EnsureCapacity(4096) |> ignore

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
    /// XXX: This is prone to failure if malicious input contains
    ///      extremely long line. It could be made more robust by
    ///      reading a preset number of bytes into a preallocated
    ///      buffer, but this would add complexity.
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
        (str, terminated)

    let scanLiteralStr quoteChar =
        match scanStr quoteChar with
        | (str, true) -> makeToken (Str str)
        | (str, false) -> makeSyntaxErr (UnterminatedLiteralStr $"{quoteChar}{str}")

    let scanFormatStr quoteChar =
        skipNext () // skip quote char

        match scanStr quoteChar with
        | (str, true) -> makeToken (FormatStr str)
        | (str, false) -> makeSyntaxErr (UnterminatedFormatStr $"${quoteChar}{str}")

    let scanKeywordOrIdent firstChar =
        let otherChars = nextWhile (fun c -> Char.IsAsciiLetterOrDigit c || c = '_')
        let word = charsToString (firstChar :: otherChars)

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (Ident word)

    let scanSpecialKeywordOrIdent () =
        let chars = nextWhile (fun c -> Char.IsAsciiLetterOrDigit c || c = '_')
        let word = charsToString chars

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (SpecialIdent word)

    let scanComment () =
        let chars = nextWhile (fun c -> c <> '\n')
        let comment = charsToString chars
        skipNext ()
        makeToken (Comment comment)

    let scanDocComment () =
        let chars = nextWhile (fun c -> c <> '\n')
        let comment = charsToString chars
        skipNext ()
        makeToken (DocComment comment)

    // API -------------------------------------------------------------

    /// Start and end positions of current token.
    member _.pos = (startPos, endPos)

    /// Get all the tokens as a list of `Result`s. If a syntax error is
    /// encountered, the last item will be a `SyntaxErr`; otherwise the
    /// last item will be `EOF`.
    member this.tokens() =
        let rec loop tokens =
            match this.nextToken () with
            | Token _ as result -> [ result ] @ (loop tokens)
            | result -> [ result ]

        loop []

    /// Get the next token. Returns `EOF` when the end of the `stream`
    /// is reached. Returns a `SyntaxErr` when a syntax error is
    /// encountered.
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

            // Doc Comment ---------------------------------------------
            | '/', Some '/' -> scanDocComment ()

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
            | f, _ when Char.IsAsciiDigit(f) -> scanNumber f
            | q, _ when q = '"' || q = '\'' -> scanLiteralStr q
            | '$', Some q when q = '"' || q = '\'' -> scanFormatStr q

            // Keywords & Identifiers ----------------------------------
            | f, _ when Char.IsAsciiLetter(f) -> scanKeywordOrIdent f
            | '$', Some f when Char.IsAsciiLetter(f) -> scanSpecialKeywordOrIdent ()

            // Comment -------------------------------------------------
            | '#', _ -> scanComment ()

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
    lexer.tokens ()

let tokensFromFile (fileName: string) =
    let lexer = fromFile fileName
    lexer.tokens ()

let formatResult result =
    match result with
    | Token token -> formatPosToken token
    | EOF -> "EOF"
    | SyntaxErr err -> formatSyntaxErr err
