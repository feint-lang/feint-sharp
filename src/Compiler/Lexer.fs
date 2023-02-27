module Feint.Compiler.Lexer

open System
open System.Collections.Generic

open Errors
open LexerUtil
open Tokens

let INDENT_SIZE = 4u
let READ_BUF_MAX = 1024

type Result =
    | Token of PosToken
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
type Lexer(fileName: string, stream: IO.TextReader) =
    /// Characters are read from the input `stream` into this buffer and
    /// then into a queue, which is refilled as needed when `peek` and
    /// `next` are called.
    let buffer = Array.zeroCreate READ_BUF_MAX

    let queue = Queue<char>()
    do queue.EnsureCapacity(READ_BUF_MAX) |> ignore

    // NOTE: Positions are 1-based.
    let mutable line = 1u
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
        if queue.Count = 0 then
            match stream.Read(buffer, 0, READ_BUF_MAX) with
            | 0 -> false
            | n ->
                for i = 0 to n - 1 do
                    queue.Enqueue buffer[i]

                true
        else
            true

    /// Peek at next character in `queue`. Returns `None` if `queue` is
    /// empty and input `stream` is depleted.
    let peek () =
        let c = ref '\000'

        if queue.TryPeek(c) then Some(c.Value)
        else if fill () && queue.TryPeek(c) then Some(c.Value)
        else None

    /// Read next character from `queue`. Returns `None` if `queue` is
    /// empty and input `stream` is depleted.
    let read () =
        let c = ref '\000'

        if queue.TryDequeue(c) then Some(c.Value)
        else if fill () && queue.TryDequeue(c) then Some c.Value
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
            | false -> 0u
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
        SyntaxErr(makeSyntaxErr fileName startPos endPos kind)

    // Scanners --------------------------------------------------------
    //
    // Scanners scan forward from the current position to produce a
    // token from one or more characters.

    // TODO: implement
    let scanNumberBase2 () =
        makeSyntaxErr (NotImplemented "binary number scanner")

    // TODO: implement
    let scanNumberBase8 () =
        makeSyntaxErr (NotImplemented "octal number scanner")

    // TODO: implement
    let scanNumberBase16 () =
        makeSyntaxErr (NotImplemented "hex number scanner")

    let scanFloat intChars indicator =
        // Scan exponent part after `e`.
        let scanExponentPart floatChars required =
            match nextWhile isDigit, required with
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
            match nextWhile isDigit with
            | [] ->
                let msg = "decimal point must be followed by at least one digit"
                makeSyntaxErr (InvalidFloat msg)
            | fractionalPart ->
                let floatChars = intPart @ [ '.' ] @ fractionalPart
                scanExponentPart floatChars false

        match indicator with
        | '.' -> scanFractionalPart intChars
        | ('e' | 'E') -> scanExponentPart intChars true
        // XXX: The fallback case is really an internal error.
        | _ -> makeSyntaxErr (InvalidFloat "expected decimal point or E")

    let scanNumberBase10 firstDigit =
        let intChars = firstDigit :: nextWhile isDigit

        match nextIf isFloatIndicator with
        | Some indicator -> scanFloat intChars indicator
        | _ -> makeToken (intFromChars intChars)

    let scanNumber firstDigit =
        match firstDigit, peek () with
        | '0', Some('b' | 'B') ->
            skip () |> ignore
            scanNumberBase2 ()
        | '0', Some('o' | 'O') ->
            skip () |> ignore
            scanNumberBase8 ()
        | '0', Some('x' | 'X') ->
            skip () |> ignore
            scanNumberBase16 ()
        | '0', Some d when isDigit d ->
            makeSyntaxErr (InvalidNumber "only one leading 0 is allowed")
        | _ -> scanNumberBase10 firstDigit

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

        let str = loop [] |> charsToString
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
        let word = charsToString (firstChar :: nextWhile isIdentChar)

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (Ident word)

    let scanSpecialKeywordOrIdent () =
        let chars = nextWhile isIdentChar
        let word = charsToString chars

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (SpecialIdent word)

    let scanComment start =
        let chars = start @ nextWhile isNotNewline
        let comment = charsToString chars
        let token = makeToken (Comment comment)
        skip () |> ignore // skip newline
        token

    let scanDocComment start =
        let chars = start @ nextWhile isNotNewline
        let comment = charsToString chars
        let result = makeToken (DocComment comment)
        skip () |> ignore // skip newline
        result

    // Indentation & Whitespace ----------------------------------------

    /// Determine whether a new indent is expected. A new indent is
    /// expected at the beginning of a line following a block start
    /// token.
    let expectNewIndent () =
        match col, lastToken with
        | 0u, Some last ->
            match last with
            | ScopeStart
            | FuncStart -> true
            | _ -> false
        | _ -> false

    /// Called when a newline is encountered.
    ///
    /// - Handles blank and comment-only lines
    /// - Checks whether a new indent is expected
    /// - Validates indent
    /// - Decreases indent level if a dedent is detected
    let handleNewline () =
        let spaceCount = skipSpaces ()

        match peek () with
        | Some '\n'
        | Some '#' -> makeToken Whitespace
        | _ ->
            if spaceCount % INDENT_SIZE <> 0u then
                makeSyntaxErr (InvalidIndent spaceCount)
            elif expectNewIndent () then
                let actualLevel = (spaceCount / INDENT_SIZE)
                let expectedLevel = indentLevel + 1u

                if actualLevel <> expectedLevel then
                    makeSyntaxErr (ExpectedIndent actualLevel)
                else
                    makeToken Whitespace
            else
                makeToken Whitespace

    /// Handle non-indentation whitespace between tokens.
    let handleWhitespace () =
        skipSpaces () |> ignore

        match startPos with
        | (1u, 0u) -> makeSyntaxErr (UnexpectedIndent)
        | _ -> makeToken Whitespace

    // API -------------------------------------------------------------

    /// Start and end positions of current token.
    member _.pos = (startPos, endPos)

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
    member _.nextToken() =
        handleWhitespace () |> ignore

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
            | '\n', _ -> handleNewline ()
            | ' ', _ -> handleWhitespace ()

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

let fromText (fileName: string) (text: string) =
    Lexer(fileName, new IO.StringReader(text))

let fromFile (fileName: string) =
    Lexer(fileName, new IO.StreamReader(fileName))

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
