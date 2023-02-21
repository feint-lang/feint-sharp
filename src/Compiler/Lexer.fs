module Feint.Compiler.Lexer

open System

open Token

type Pos = uint * uint

type Result =
    | Token of PosToken
    | SyntaxErr of Pos * string
    | EOF

type Lexer(stream: IO.TextReader) =
    let stream = stream
    let mutable line = 1u
    let mutable col = 0u

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

    let makeToken startPos rawToken =
        Token
            { startPos = startPos
              endPos = (line, col)
              token = rawToken }

    let makeSyntaxErr message = SyntaxErr((line, col), message)

    let handleInt startPos firstDigit =
        let otherDigits = nextCharWhile (fun c -> c >= '0' && c <= '9')
        let str = firstDigit :: otherDigits |> List.toArray |> String
        let token = Int(bigint.Parse str)
        makeToken startPos token

    // API -------------------------------------------------------------

    member _.pos = (line, col)

    member this.nextToken() =
        skipWhitespace ()

        match nextChar () with
        | None -> EOF
        | Some c when System.Char.IsDigit(c) -> handleInt this.pos c
        | Some '*' -> makeToken this.pos Star
        | Some '/' -> makeToken this.pos Slash
        | Some '+' -> makeToken this.pos Plus
        | Some '-' -> makeToken this.pos Dash
        | Some '\t' -> makeSyntaxErr ("Cannot use TAB for indentation or whitespace")
        | Some c -> makeSyntaxErr ($"Unhandled character: {c}")

let fromString (str: string) = new Lexer(new IO.StringReader(str))

let fromFile (fileName: string) =
    new Lexer(new IO.StreamReader(fileName))
