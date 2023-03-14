module Feint.Compiler.Lexer

open System
open System.Collections.Generic

open Errors
open LexerUtil
open Tokens

let INDENT_SIZE = 4u
let READ_BUF_MAX = 1024

type Result =
    | Continue
    | Token of SpanToken
    | EOF
    | SyntaxErr of SyntaxErr

/// The `Lexer` converts a `stream` of chars to tokens. These tokens can
/// be retrieved by repeatedly calling the `next` method until an `EOF`
/// or `SyntaxErr` result is returned. The `all` method can be used to
/// get all the tokens as a list of `Result`s for use in tests.
///
/// The `Lexer` is relatively simple, only concerning itself with
/// generating tokens, handling indents/dedents (since these take the
/// place of explicit block start/end tokens), and reporting simple
/// syntax errors. The `Parser` is responsible for determining whether
/// the generated tokens represent a valid program.
///
/// The `fileName` and `text` properties are only used when constructing
/// errors in order to create more informative error messages.
type Lexer(stream: IO.TextReader, fileName: string, text: string option) =
    /// Characters are read from the input `stream` into this buffer and
    /// then into a queue, which is refilled as needed when `peek` and
    /// `next` are called.
    let buffer = Array.zeroCreate READ_BUF_MAX

    /// Queue of characters filled from stream as needed (see `fill`).
    let charQueue = Queue<char>()

    let bracketStack = Stack<char * (Pos)>()

    // NOTE: Positions are 1-based.
    let mutable line = 0u
    let mutable col = 0u

    /// Start position of the current lexeme/token.
    let mutable startPos = (line, col)

    /// End position of the current lexeme/token.
    let mutable endPos = (line, col)

    let mutable lastToken: Token option = None
    let mutable indentLevel = 0u

    /// Queue of results. This is used only in special cases where
    /// multiple tokens need to be emitted, such as when handling
    /// newlines/indentation.
    let resultQueue = Queue<Result>()

    /// Push result to back of result queue.
    let pushResult result = resultQueue.Enqueue result

    let getResultFromQueue () =
        let result = ref EOF

        match resultQueue.TryDequeue(result) with
        | true -> Some result.Value
        | false -> None

    /// Fill `queue` if it's empty by reading up to `READ_BUFFER_MAX`
    /// characters from the input `stream`. Returns a flag indicating
    /// whether `queue` contains at least one character.
    let fillCharQueue () =
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
    let peekChar () =
        let c = ref '\000'

        if charQueue.TryPeek(c) then
            Some(c.Value)
        else if fillCharQueue () && charQueue.TryPeek(c) then
            Some(c.Value)
        else
            None

    /// Read next character from `queue`. Returns `None` if `queue` is
    /// empty and input `stream` is depleted.
    let readChar () =
        let c = ref '\000'

        if charQueue.TryDequeue(c) then
            Some(c.Value)
        else if fillCharQueue () && charQueue.TryDequeue(c) then
            Some c.Value
        else
            None

    /// Get next character and increment `col`. `\r\n` style newlines
    /// are normalized to `\n` in order to simplify lexing (`\r`s not
    /// followed by a `\n` are left as-is).
    ///
    /// When a newline is encountered, `line` is NOT incremented here
    /// since newline-handling is somewhat complex. See `handleNewline`.
    let nextChar () =
        match readChar () with
        | None -> None
        | Some c ->
            let c =
                match c with
                | '\r' ->
                    // Normalize \r\n to \n
                    match peekChar () with
                    | Some '\n' ->
                        readChar () |> ignore
                        '\n'
                    | _ -> '\r'
                | '\n' -> '\n'
                | _ -> c

            col <- col + 1u
            Some c

    let nextCharIf predicate =
        match peekChar () with
        | None -> None
        | Some c ->
            match predicate c with
            | false -> None
            | true -> nextChar ()

    let nextCharWhile predicate =
        let rec loop collector =
            match nextCharIf predicate with
            | None -> collector
            | Some c -> loop (collector @ [ c ])

        loop []

    let skipNextChar () =
        match nextChar () with
        | None -> false
        | Some _ -> true

    let skipNextCharThenPeek () =
        skipNextChar () |> ignore
        peekChar ()

    let skipNextCharIf predicate =
        match peekChar () with
        | None -> false
        | Some c ->
            match predicate c with
            | false -> false
            | true -> skipNextChar ()

    let skipCharWhile predicate =
        let rec loop count =
            match skipNextCharIf predicate with
            | false -> count
            | true -> loop (count + 1u)

        loop 0u

    let skipSpaces () = skipCharWhile (fun c -> c = ' ')

    // Result Constructors ---------------------------------------------

    let makeToken token =
        endPos <- (line, col)
        lastToken <- Some token
        Token(makePosToken startPos endPos token)

    /// Streamlines creation of 2-character tokens.
    let skipMakeToken token =
        skipNextChar () |> ignore
        makeToken token

    let makeSyntaxErr kind =
        endPos <- (line, col)
        SyntaxErr(makeSyntaxErr fileName text startPos endPos kind)

    let makeSyntaxErrWithStartAndEnd startPos endPos kind =
        SyntaxErr(Errors.makeSyntaxErr fileName text startPos endPos kind)

    // Scanners --------------------------------------------------------
    //
    // Scanners scan forward from the current position to produce a
    // token from one or more characters.

    let scanFloat intChars indicator =
        // Scan exponent part after `e`.
        let scanExponentPart floatChars required =
            match nextCharWhile isDigitBase10, required with
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
            match nextCharWhile isDigitBase10 with
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
        let intChars = firstDigit :: nextCharWhile isDigitBase10

        match nextCharIf isFloatIndicator with
        | Some indicator -> scanFloat intChars indicator
        | _ -> makeToken (intFromChars intChars)

    let scanBase prefix =
        let makeInt digitPredicate =
            match nextCharWhile digitPredicate with
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
        match firstDigit, peekChar () with
        | '0', Some d when isDigitBase10 d ->
            makeSyntaxErr (InvalidNumber "leading zeros not allowed")
        | '0', Some d when isAsciiLetter d ->
            skipNextChar () |> ignore
            scanBase d
        | _ -> scanBase10 firstDigit

    let scanStr quoteChar =
        let mutable terminated = false

        let rec loop chars =
            match nextChar (), peekChar () with
            | None, _ -> chars
            | Some c, _ when c = quoteChar ->
                terminated <- true
                chars
            | Some '\\', Some d ->
                skipNextChar () |> ignore
                processEscapedChar d @ loop chars
            | Some c, _ -> [ c ] @ loop chars

        let str = loop [] |> stringFromChars
        (str, terminated)

    let scanLiteralStr quoteChar =
        match scanStr quoteChar with
        | str, true -> makeToken (Str str)
        | str, false -> makeSyntaxErr (UnterminatedLiteralStr $"{quoteChar}{str}")

    let scanFormatStr quoteChar =
        skipNextChar () |> ignore // skip quote char

        match scanStr quoteChar with
        | str, true -> makeToken (FormatStr str)
        | str, false -> makeSyntaxErr (UnterminatedFormatStr $"${quoteChar}{str}")

    let scanKeywordOrIdent firstChar =
        let word = stringFromChars (firstChar :: nextCharWhile isIdentChar)

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (Ident word)

    let scanSpecialKeywordOrIdent () =
        let chars = nextCharWhile isIdentChar
        let word = stringFromChars chars

        match keywordToken word with
        | Some token -> makeToken token
        | None -> makeToken (SpecialIdent word)

    // Comments --------------------------------------------------------

    let scanComment () =
        let rec collect lines =
            let lineChars = nextCharWhile isNotNewline
            skipNextChar () |> ignore

            match lineChars with
            | [] -> lines
            | lineChars ->
                let line = (stringFromChars lineChars)

                match nextCharIf isCommentIndicator with
                | None -> lines @ [ line ]
                | Some _ -> collect (lines @ [ line ])

        collect [] |> Comment |> makeToken

    let scanDocComment () =
        let rec collect lines =
            let lineChars = nextCharWhile isNotNewline
            skipNextChar () |> ignore

            match lineChars with
            | [] -> lines
            | lineChars ->
                let line = (stringFromChars lineChars)

                match nextCharIf isDocCommentIndicator with
                | None -> lines @ [ line ]
                | Some _ -> collect lines @ [ line ]

        collect [] |> DocComment |> makeToken

    // Indentation -----------------------------------------------------

    let maybePushEndOfStatement () =
        match lastToken with
        | (None | Some(ScopeStart | FuncStart | EndOfStatement | Comment _)) -> ()
        | _ -> pushResult (makeToken EndOfStatement)

    let maybeDedent newLevel =
        while indentLevel > newLevel do
            pushResult (makeToken ScopeEnd)
            pushResult (makeToken EndOfStatement)
            indentLevel <- indentLevel - 1u

        Continue

    /// Called when a newline is encountered.
    ///
    /// - Adds end-of-statement token when appropriate
    /// - Increments `line` and resets `col`
    /// - Handles blank and comment-only lines
    /// - Checks whether a new indent is expected
    /// - Validates indent for line
    /// - Handles dedenting if a dedent is detected
    let handleNewline () =
        maybePushEndOfStatement ()

        // XXX: Must be after emitting end-of-statement token.
        line <- line + 1u
        col <- 0u

        let expectNewIndent () =
            match lastToken with
            | Some(ScopeStart | FuncStart) -> true
            | _ -> false

        // Get indent level for line if indentation is syntactically
        // valid.
        let getIndentLevel () =
            let spaceCount = skipSpaces ()

            match spaceCount % INDENT_SIZE = 0u with
            | true -> Ok(spaceCount / INDENT_SIZE)
            | false ->
                let kind =
                    match lastToken with
                    | Some _ -> InvalidIndent spaceCount
                    // XXX: Special case for first line of code
                    | None -> UnexpectedWhitespace

                // XXX: Set start position for syntax error so that it
                //      points at start of indent.
                startPos <- (line, 1u)

                Error(makeSyntaxErr kind)

        match peekChar () with
        // Blank line
        | (None | Some('\n')) -> Continue
        // Line contains other tokens--check & maybe update indent level
        | _ ->
            if expectNewIndent () then
                let expectedLevel = indentLevel + 1u

                match getIndentLevel () with
                | Ok newLevel when newLevel = expectedLevel ->
                    indentLevel <- newLevel
                    Continue
                | Ok newLevel -> makeSyntaxErr (ExpectedIndent newLevel)
                | Error err -> err
            else
                match getIndentLevel () with
                | Error err -> err
                | Ok newLevel when newLevel > indentLevel ->
                    makeSyntaxErr (UnexpectedIndent newLevel)
                | Ok newLevel when newLevel < indentLevel -> maybeDedent newLevel
                // Do nothing when indent level doesn't change
                | _ -> Continue

    /// When the end of the stream is reached, it's first handled like a
    /// newline for consistency and then `EOF` is emitted.
    let mutable emitEOF = false

    let handleEOF () =
        match emitEOF with
        | false ->
            emitEOF <- true
            maybePushEndOfStatement ()
            maybeDedent 0u
        | true -> EOF

    // Groupings -------------------------------------------------------

    let handleBracket c =
        let push openChar =
            bracketStack.Push(openChar, startPos)

            match openChar with
            | '(' -> makeToken LParen
            | ')' -> makeToken RParen
            | '[' -> makeToken LBrace
            | _ -> failwith $"Invalid group open character: {c}"

        let pop actualCloseChar =
            let opening = ref ('\000', (0u, 0u))

            let makeErr () =
                let (openChar, openPos) = opening.Value

                let expectedCloseChar =
                    match openChar with
                    | '(' -> ')'
                    | '[' -> ']'
                    | '{' -> '}'
                    | _ -> failwith $"Invalid group open character: {c}"

                let kind =
                    MismatchedBracket(
                        openChar,
                        openPos,
                        expectedCloseChar,
                        actualCloseChar,
                        startPos
                    )

                makeSyntaxErr kind

            if bracketStack.TryPop(opening) then
                match actualCloseChar with
                | ')' ->
                    match opening.Value with
                    | '(', _ -> makeToken RParen
                    | _ -> makeErr ()
                | ']' ->
                    match opening.Value with
                    | '[', _ -> makeToken RBracket
                    | _ -> makeErr ()
                | '}' ->
                    match opening.Value with
                    | '{', _ -> makeToken RBrace
                    | _ -> makeErr ()
                | _ -> failwith $"Invalid group close character: {c}"
            else
                makeSyntaxErr (UnmatchedClosingBracket actualCloseChar)

        match c with
        | ('(' | '[' | '{') as openChar -> push openChar
        | (')' | ']' | '}') as closeChar -> pop closeChar
        | _ -> failwith $"Invalid group character: {c}"

    // Main Scanner ----------------------------------------------------

    let scan () =
        match nextChar (), peekChar () with
        | None, _ -> handleEOF ()
        | Some c, d ->
            startPos <- (line, col)

            // Default end position for token. Will be updated if more
            // characters are consumed.
            endPos <- (line, col)

            match c, d with
            | '\n', _ ->
                match bracketStack.Count with
                | 0 -> handleNewline ()
                | _ -> Continue

            // Non-indentation whitespace between tokens.
            | ' ', _ ->
                skipSpaces () |> ignore
                Continue

            // 2-character Tokens ======================================

            // Scopes --------------------------------------------------
            | '-', Some '>' -> skipMakeToken ScopeStart
            | '=', Some '>' -> skipMakeToken FuncStart

            // Misc ----------------------------------------------------
            | '.', Some '.' ->
                match skipNextCharThenPeek () with
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

            // 1-character Tokens ======================================

            // Misc ----------------------------------------------------
            | ':', _ -> makeToken Colon
            | ',', _ -> makeToken Comma
            | '.', _ -> makeToken Dot

            // Groupings -----------------------------------------------
            | ('(' | ')' | '[' | ']' | '{' | '}') as c, _ -> handleBracket c

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

            // Comments -------------------------------------------------
            | '#', _ -> scanComment ()
            | ';', _ -> scanDocComment ()

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

    /// Span of current token.
    member _.span = (startPos, endPos)

    /// Get the next token. Returns `EOF` when the input `stream` is
    /// is depleted. Returns a `SyntaxErr` when a syntax error is
    /// encountered.
    member this.next() =
        match getResultFromQueue () with
        | Some result -> result
        | None ->
            match scan () with
            | Continue -> this.next ()
            | result -> result

    /// Get all tokens at once as a list of `Result`s. If a syntax error
    /// is encountered, the last item will be a `SyntaxErr`; otherwise
    /// the last item will be `EOF`.
    member this.all() =
        let rec loop results =
            match this.next () with
            | Continue -> loop results
            | Token _ as result -> [ result ] @ (loop results)
            | result -> [ result ]

        loop []

let fromText (fileName: string) (text: string) =
    let stream = new IO.StringReader(text)
    Lexer(stream, fileName, Some text)

let fromFile (path: string) =
    let stream = new IO.StreamReader(path)
    Lexer(stream, path, None)

let tokensFromText (fileName: string) (text: string) =
    let lexer = fromText fileName text
    lexer.all ()

let tokensFromFile (fileName: string) =
    let lexer = fromFile fileName
    lexer.all ()

let formatResult result =
    match result with
    | Continue -> "<continue>"
    | Token token -> formatPosToken token
    | EOF -> "EOF"
    | SyntaxErr err -> formatSyntaxErr err
