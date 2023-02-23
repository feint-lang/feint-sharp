module Feint.Compiler.Lexer

open System

open Token

type Pos = uint * uint

type SyntaxErrKind =
    | Tab
    | UnhandledChar of char
    | UnterminatedStringLiteral of string

type Result =
    | Token of PosToken
    | EOF
    | SyntaxErr of fileName: string * startPos: Pos * endPos: Pos * kind: SyntaxErrKind

type Lexer(fileName: string, stream: IO.TextReader) =
    let stream = stream
    let mutable line = 1u
    let mutable col = 0u

    // Start and end positions of the current lexeme/token
    let mutable startPos = (line, col)
    let mutable endPos = (line, col)

    let peekChar () =
        match stream.Peek() with
        | -1 -> None
        | c -> Some(char c)

    let nextChar () =
        match stream.Read() with
        | -1 -> None
        | c when c = (int '\n') ->
            line <- line + 1u
            col <- 1u
            Some '\n'
        | c ->
            col <- col + 1u
            Some(char c)

    let nextCharIf predicate =
        match peekChar () with
        | Some c ->
            match predicate c with
            | true -> nextChar ()
            | false -> None
        | None -> None

    let skipNextChar () = nextChar () |> ignore

    let rec nextCharWhile predicate =
        match peekChar () with
        | None -> []
        | Some c ->
            match predicate c with
            | false -> []
            | true ->
                match nextChar () with
                | None -> []
                | Some c -> [ c ] @ nextCharWhile (predicate)

    let rec skipCharWhile predicate =
        match peekChar () with
        | None -> ()
        | Some c ->
            match predicate c with
            | false -> ()
            | true ->
                nextChar () |> ignore
                skipCharWhile (predicate)

    let skipWhitespace () = skipCharWhile (fun c -> c = ' ')

    // Error Handlers --------------------------------------------------

    let makeSyntaxErr message =
        endPos <- (line, col)
        SyntaxErr(fileName, startPos, endPos, message)

    // Handlers --------------------------------------------------------

    let makeToken token =
        endPos <- (line, col)
        Token(startPos, endPos, token)

    let handleInt firstDigit =
        let otherDigits = nextCharWhile (fun c -> c >= '0' && c <= '9')
        let str = firstDigit :: otherDigits |> List.toArray |> String
        makeToken (Int(bigint.Parse str))

    let handleStr quoteChar =
        // TODO: Handle escaped characters
        let chars = nextCharWhile (fun c -> c <> quoteChar)
        let str = chars |> List.toArray |> String

        match nextChar () with
        | Some c when c = quoteChar -> makeToken (Str(str))
        | Some c -> makeSyntaxErr (UnhandledChar c) // should be unreachable
        | None -> makeSyntaxErr (UnterminatedStringLiteral $"{quoteChar}{str}")

    // API -------------------------------------------------------------

    member _.pos = (startPos, endPos)

    member _.nextToken() =
        skipWhitespace ()

        match nextChar () with
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
                match nextCharIf (fun d -> d = '=') with
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

let formatSyntaxErrKind =
    function
    | Tab -> "TAB cannot be used for indentation or whitespace"
    | UnhandledChar c -> $"unhandled character: {c}"
    | UnterminatedStringLiteral s -> $"unterminated string literal: {s}"

let formatResult fileName text result =
    match result with
    | Token((sLine, sCol), (eLine, eCol), t) ->
        $"{sLine}:{sCol} -> {eLine}:{eCol} = {t}"
    | EOF -> "EOF"
    | SyntaxErr(fileName, (sLine, sCol), (eLine, eCol), kind) ->
        let kind = formatSyntaxErrKind kind

        let msg =
            $"Syntax error in '{fileName}' on line {sLine} at column {sCol}: {kind}"

        // if not (fileName.StartsWith("<")) then
        //     msg <- $"{msg}\n\n"
        //     letIO.StreamReader(fileName).ReadToEnd()


        msg
