module Feint.Compiler.Lexer

open System
open System.Collections.Generic

open Errors
open LexerUtil
open Tokens

let INDENT_SIZE = 4u
let READ_BUF_MAX = 1024

type Result =
    | Token of SpanToken
    | EOF
    | SyntaxErr of SyntaxErr

/// The `Lexer` converts a `stream` of chars to tokens. These tokens can
/// be retrieved by repeatedly calling the `nextToken` method until an
/// `EOF` token or `SyntaxErr` is returned. The `tokens` method can be
/// used to get all the tokens as a list of `Result`s for use in tests.
///
/// The `Lexer` is intentionally simple, concerning itself only with
/// generating tokens and detecting simple syntax errors (such as
/// unexpected characters in the input stream and unterminated string
/// literals). The `Parser` is responsible for determining whether the
/// generated tokens represent a valid program.
type Lexer(fileName: string, text: string option, stream: IO.TextReader) =
    /// Characters are read from the input `stream` into this buffer and
    /// then into a queue, which is refilled as needed when `peek` and
    /// `next` are called.
    let buffer = Array.zeroCreate READ_BUF_MAX

    let charQueue = Queue<char>()

    // NOTE: Positions are 1-based.
    let mutable line = 0u
    let mutable col = 0u

    /// Start position of the current lexeme/token.
    let mutable startPos = (line, col)

    /// End position of the current lexeme/token.
    let mutable endPos = (line, col)

    let mutable lastToken: Token option = None
    let mutable indentLevel = 0u

    /// Action performed when a newline is encountered.
    let newLine () =
        line <- line + 1u
        col <- 0u

    /// Fill `queue` if it's empty by reading up to `READ_BUFFER_MAX`
    /// characters from the input `stream`. Returns a flag indicating
    /// whether `queue` contains at least one character.
    let fill () =
        if charQueue.Count = 0 then
            match stream.Read(buffer, 0, READ_BUF_MAX) with
            | 0 -> false
            | n ->
                for i = 0 to n - 1 do
                    charQueue.Enqueue buffer[i]

                true
        else
            true

    /// Peek at next character in `queue`. Returns `None` if `queue` is
    /// empty and input `stream` is depleted.
    let peek () =
        let c = ref '\000'

        if charQueue.TryPeek(c) then Some(c.Value)
        else if fill () && charQueue.TryPeek(c) then Some(c.Value)
        else None

    /// Read next character from `queue`. Returns `None` if `queue` is
    /// empty and input `stream` is depleted.
    let read () =
        let c = ref '\000'

        if charQueue.TryDequeue(c) then Some(c.Value)
        else if fill () && charQueue.TryDequeue(c) then Some c.Value
        else None

    /// Get next character. `\r\n` newlines are normalized to `\n` in
    /// order to simplify lexing (`\r`s not followed by a `\n` ar left
    /// as-is).
    ///
    /// If a newline is encountered, `line` is incremented and `col` is
    /// set to `0`. Otherwise, `col` is incremented.
    let next () =
        match read () with
        | None -> None
        | Some c ->
            match c with
            | '\r' ->
                // Normalize \r\n to \n
                match peek () with
                | Some '\n' ->
                    read () |> ignore
                    newLine ()
                    Some '\n'
                | _ -> Some '\r'
            | '\n' ->
                newLine ()
                Some '\n'
            | _ ->
                col <- col + 1u
                Some c

    let nextIf predicate =
        match peek () with
        | None -> None
        | Some c ->
            match predicate c with
            | false -> None
            | true -> next ()

    let nextWhile predicate =
        let rec loop collector =
            match nextIf predicate with
            | None -> collector
            | Some c -> loop (collector @ [ c ])

        loop []

    let skip () =
        match next () with
        | None -> false
        | Some _ -> true

    let skipPeek () =
        skip () |> ignore
        peek ()

    let skipIf predicate =
        match peek () with
        | None -> false
        | Some c ->
            match predicate c with
            | false -> false
            | true -> skip ()

    let skipWhile predicate =
        let rec loop count =
            match skipIf predicate with
            | false -> count
            | true -> loop (count + 1u)

        loop 0u

    let skipSpaces () = skipWhile (fun c -> c = ' ')

    // Result Constructors ---------------------------------------------

    let makeToken token =
        endPos <- (line, col)
        lastToken <- Some token
        Token(makePosToken startPos endPos token)

    /// Streamlines creation of 2-character tokens.
    let skipMakeToken token =
        skip () |> ignore
        makeToken token

    let makeSyntaxErr kind =
        endPos <- (line, col)
        SyntaxErr(makeSyntaxErr fileName text startPos endPos kind)

    // Scanners --------------------------------------------------------
    //
    // Scanners scan forward from the current position to produce a
    // token from one or more characters.

    let scanFloat intChars indicator =
        // Scan exponent part after `e`.
        let scanExponentPart floatChars required =
            match nextWhile isDigitBase10, required with
            | [], true ->
                makeSyntaxErr (InvalidFloat "exponent must contain at least one digit")
            | [], false ->
                let chars = floatChars
                makeToken (floatFromChars chars)
            | expPart, _ ->
                let chars = floatChars @ [ 'e' ] @ expPart
                makeToken (floatFromChars chars)

        // Scan fractional part after decimal point then scan for
        // optional exponent part.
        let scanFractionalPart intPart =
            match nextWhile isDigitBase10 with
            | [] ->
                let msg = "decimal point must be followed by at least one digit"
                makeSyntaxErr (InvalidFloat msg)
            | fractionalPart ->
                let floatChars = intPart @ [ '.' ] @ fractionalPart
                scanExponentPart floatChars false

        match indicator with
        | '.' -> scanFractionalPart intChars
        | 'e' -> scanExponentPart intChars true
        // XXX: The fallback case is really an internal error.
        | _ -> makeSyntaxErr (InvalidFloat "expected decimal point or E")

    let scanBase10 firstDigit =
        let intChars = firstDigit :: nextWhile isDigitBase10

        match nextIf isFloatIndicator with
        | Some indicator -> scanFloat intChars indicator
        | _ -> makeToken (intFromChars intChars)

    let scanBase prefix =
        let makeInt digitPredicate =
            match nextWhile digitPredicate with
            | [] -> makeSyntaxErr (InvalidNumber "expected digits following prefix")
            | digits ->
                let token =
                    match prefix with
                    | 'x' -> intFromHexChars digits
                    | _ ->
                        // XXX: This will fail if value is outside int64 range
                        let chars = [ '0'; prefix ] @ digits
                        let value = chars |> stringFromChars |> int64 |> bigint
                        Int value

                makeToken token

        match prefix with
        | 'b' -> makeInt isDigitBase2
        | 'o' -> makeInt isDigitBase8
        | 'x' -> makeInt isDigitBase16
        | d -> makeSyntaxErr (InvalidIntPrefix d)

    let scanNumber firstDigit =
        match firstDigit, peek () with
        | '0', Some d when isDigitBase10 d ->
            makeSyntaxErr (InvalidNumber "leading zeros not allowed")
        | '0', Some d when isAsciiLetter d ->
            skip () |> ignore
            scanBase d
        | _ -> scanBase10 firstDigit

    let scanStr quoteChar =
        let mutable terminated = false

        let rec loop chars =
            match next (), peek () with
            | None, _ -> chars
            | Some c, _ when c = quoteChar ->
                terminated <- true
                chars
            | Some '\\', Some d ->
                skip () |> ignore
                processEscapedChar d @ loop chars
            | Some c, _ -> [ c ] @ loop chars

        let str = loop [] |> stringFromChars
        (str, terminated)

    let scanLiteralStr quoteChar =
        match scanStr quoteChar with
        | str, true -> makeToken (Str str)
        | str, false -> makeSyntaxErr (UnterminatedLiteralStr $"{quoteChar}{str}")

    let scanFormatStr quoteChar =
        skip () |> ignore // skip quote char

        match scanStr quoteChar with
        | str, true -> makeToken (FormatStr str)
        | str, false -> makeSyntaxErr (UnterminatedFormatStr $"${quoteChar}{str}")

    let scanKeywordOrIdent firstChar =
        let word = stringFromChars (firstChar :: nextWhile isIdentChar)

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (Ident word)

    let scanSpecialKeywordOrIdent () =
        let chars = nextWhile isIdentChar
        let word = stringFromChars chars

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (SpecialIdent word)

    // Comments --------------------------------------------------------

    let scanComment start =
        let chars = start @ nextWhile isNotNewline
        let comment = stringFromChars chars
        let token = makeToken (Comment comment)
        token

    let scanDocComment start =
        let chars = start @ nextWhile isNotNewline
        let comment = stringFromChars chars
        let result = makeToken (DocComment comment)
        result

    // Indentation & Whitespace ----------------------------------------

    /// Called when a newline is encountered.
    ///
    /// - Handles blank and comment-only lines
    /// - Checks whether a new indent is expected
    /// - Validates indent
    /// - Decreases indent level if a dedent is detected
    let handleNewline () =
        let spaceCount = skipSpaces ()

        if spaceCount > 0u then
            startPos <- (line, col - spaceCount + 1u)

        let expectNewIndent () =
            match lastToken with
            | Some(ScopeStart | FuncStart) -> true
            | _ -> false

        let getIndentLevel () =
            match spaceCount % INDENT_SIZE = 0u with
            | true -> Some(spaceCount / INDENT_SIZE)
            | false -> None

        match peek () with
        // Line is all whitespace
        | Some '\n' -> None
        | None -> None
        // Line is comment-only (which may be preceded by whitespace)
        | Some '#' -> None
        // Line contains other tokens--check & maybe update indent level
        | _ ->
            if expectNewIndent () then
                match getIndentLevel () with
                | None -> Some(makeSyntaxErr (InvalidIndent spaceCount))
                | Some newLevel when newLevel = indentLevel + 1u ->
                    indentLevel <- newLevel
                    None
                | Some newLevel -> Some(makeSyntaxErr (ExpectedIndent newLevel))
            else
                match getIndentLevel () with
                | None ->
                    let err =
                        match lastToken with
                        | Some _ -> InvalidIndent spaceCount
                        // XXX: Special case for first line of code
                        | None -> UnexpectedWhitespace

                    Some(makeSyntaxErr err)
                | Some newLevel when newLevel < indentLevel ->
                    indentLevel <- newLevel
                    None
                | Some newLevel when newLevel > indentLevel ->
                    Some(makeSyntaxErr (UnexpectedIndent newLevel))
                | _ -> None

    // Main Scanner ----------------------------------------------------

    let rec scan () =
        match next (), peek () with
        | None, _ ->
            indentLevel <- 0u
            EOF
        | Some c, d ->
            startPos <- (line, col)

            // Default end position for token. Will be updated if more
            // characters are consumed.
            endPos <- (line, col)

            match c, d with
            | '\n', _ ->
                match handleNewline () with
                | Some result -> result
                | None -> scan ()

            // Non-indentation whitespace between tokens.
            | ' ', _ ->
                skipSpaces () |> ignore
                scan ()

            // 2-character Tokens ======================================

            // Scopes --------------------------------------------------
            | '-', Some '>' -> skipMakeToken ScopeStart
            | '=', Some '>' -> skipMakeToken FuncStart

            // Misc ----------------------------------------------------
            | '.', Some '.' ->
                match skipPeek () with
                | Some '.' -> skipMakeToken Ellipsis
                | _ -> makeToken DotDot

            // Unary Operators -----------------------------------------
            | '!', Some '!' -> skipMakeToken BangBang

            // Comparison Operators ------------------------------------
            | '=', Some '=' -> skipMakeToken EqEq
            | '!', Some '=' -> skipMakeToken NotEq
            | '~', Some '~' -> skipMakeToken TildeTilde
            | '!', Some '~' -> skipMakeToken BangTilde

            // Logic Operators -----------------------------------------
            | '&', Some '&' -> skipMakeToken And
            | '|', Some '|' -> skipMakeToken Or
            | '?', Some '?' -> skipMakeToken NilOr

            // Comparison Operators ------------------------------------
            | '<', Some '=' -> skipMakeToken LtOrEq
            | '>', Some '=' -> skipMakeToken GtOrEq

            // Assignment Operators ------------------------------------
            | '<', Some '-' -> skipMakeToken Feed

            // Doc Comment ---------------------------------------------
            | '/', Some '/' -> scanDocComment [ '/'; '/' ]

            // 1-character Tokens ======================================

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
            | ('"' | '\'') as q, _ -> scanLiteralStr q
            | '$', Some('"' | '\'' as q) -> scanFormatStr q

            // Keywords & Identifiers ----------------------------------
            | f, _ when Char.IsAsciiLetter(f) -> scanKeywordOrIdent f
            | '$', Some f when Char.IsAsciiLetter(f) -> scanSpecialKeywordOrIdent ()

            // Comment -------------------------------------------------
            | '#', _ -> scanComment [ '#' ]

            // Errors --------------------------------------------------
            | _ ->
                match c with
                | '\t' -> makeSyntaxErr Tab
                | unhandled -> makeSyntaxErr (UnhandledChar unhandled)

    // Initialization --------------------------------------------------

    do
        charQueue.EnsureCapacity(READ_BUF_MAX) |> ignore

        // Always start with a newline to ensure consistent handling of
        // the first line of code.
        charQueue.Enqueue '\n'

    // API -------------------------------------------------------------

    /// Span of current token--start and end positions.
    member _.span = (startPos, endPos)

    /// Get all tokens at once as a list of `Result`s. If a syntax error
    /// is encountered, the last item will be a `SyntaxErr`; otherwise
    /// the last item will be `EOF`.
    member this.tokens() =
        let rec loop tokens =
            match this.nextToken () with
            | Token _ as result -> [ result ] @ (loop tokens)
            | result -> [ result ]

        loop []

    /// Get the next token. Returns `EOF` when the input `stream` is
    /// is depleted. Returns a `SyntaxErr` when a syntax error is
    /// encountered.
    member this.nextToken() = scan ()

let fromText (fileName: string) (text: string) =
    Lexer(fileName, Some text, new IO.StringReader(text))

let fromFile (fileName: string) =
    Lexer(fileName, None, new IO.StreamReader(fileName))

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
