open System

open PowerArgs

open Feint
open Feint.Compiler
open Feint.Interpreter

type Operation =
    | RunCode
    | RunFile
    | PrintCodeTokens
    | PrintFileTokens
    // | PrintCodeAst
    // | PrintFileAst
    | RunRepl

type Result =
    | Success
    | ParseErr of string
    | InterpretErr of string
    | ReplErr of string

// XXX: This is used as a hacky way to "return" a result from Main()
//      since it's required to return void/unit.
exception Exit of Result

[<ArgExceptionBehavior(ArgExceptionPolicy.StandardExceptionHandling)>]
type Argv() =
    // Args ------------------------------------------------------------

    [<HelpHook>]
    [<ArgDescription("Show this help")>]
    [<ArgShortcut("-h")>]
    [<ArgShortcut("--help")>]
    member val help = false with get, set

    [<ArgDescription("Run code snippet")>]
    [<ArgShortcut("-c")>]
    [<ArgShortcut("--code")>]
    member val code: string = null with get, set

    [<ArgDescription("Run file")>]
    [<ArgExistingFile>]
    [<ArgPosition(0)>]
    member val fileName: string = null with get, set

    [<ArgDescription("Print tokens")>]
    [<ArgShortcut("-t")>]
    [<ArgShortcut("--tokens")>]
    member val tokens = false with get, set

    [<ArgDescription("Print AST instead of interpreting when running code or file")>]
    [<ArgShortcut("-a")>]
    [<ArgShortcut("--ast")>]
    member val ast = false with get, set

    // Operations ------------------------------------------------------

    member this.selectOperation() =
        if not (isNull this.code) then
            if this.tokens then
                PrintCodeTokens
            // elif this.ast then PrintCodeAst
            else
                RunCode
        elif not (isNull this.fileName) then
            if this.tokens then
                PrintFileTokens
            // elif this.ast then PrintFileAst
            else
                RunFile
        else
            RunRepl

    member this.runCode() =
        Interpreter.interpretText this.code "<code>" |> this.handleRunResult

    member this.runFile() =
        Interpreter.interpretFile this.fileName |> this.handleRunResult

    member _.handleRunResult result =
        match result with
        | Interpreter.Success -> Success
        | Interpreter.ParseErr err -> ParseErr err
        | Interpreter.InterpretErr err -> InterpretErr err

    member _.printTokens results =
        let handle result =
            Lexer.formatResult result |> Console.WriteLine

        List.iter handle results
        Success

    member this.printCodeTokens() =
        Lexer.tokensFromText "<code>" this.code |> this.printTokens


    member this.printFileTokens() =
        Lexer.tokensFromFile this.fileName |> this.printTokens

    // member this.printCodeAst() =
    //     Driver.printAstFromText this.code "<code>"
    //     Success

    // member this.printFileAst() =
    //     Driver.printAstFromFile this.fileName
    //     Success

    member _.runRepl() =
        match Repl.startRepl () with
        | Repl.Exit -> Success
        | Repl.ReadLineErr exc -> ReplErr $"{exc}"

    // Main ------------------------------------------------------------

    member this.Main() : unit =
        let result =
            match this.selectOperation () with
            | RunCode -> this.runCode ()
            | RunFile -> this.runFile ()
            | PrintCodeTokens -> this.printCodeTokens ()
            | PrintFileTokens -> this.printFileTokens ()
            // | PrintCodeAst -> this.printCodeAst ()
            // | PrintFileAst -> this.printFileAst ()
            | RunRepl -> this.runRepl ()

        raise (Exit result)

[<EntryPoint>]
let main argv =
    // XXX: Required to avoid exception due to BackgroundColor and
    //      ForegroundColor not being set on Unix platforms.
    if (int) System.Console.BackgroundColor = -1 then
        System.Console.BackgroundColor <- System.ConsoleColor.Black

    if (int) System.Console.ForegroundColor = -1 then
        System.Console.ForegroundColor <- System.ConsoleColor.White

    try
        let result = PowerArgs.Args.InvokeMain<Argv>(argv)

        match result.Cancelled with
        | true -> 0 // --help
        | false ->
            match result.HandledException with
            | null -> 254 // unreachable?
            | _ -> 255 // bad args
    with Exit result ->
        match result with
        | Success -> 0
        | ParseErr msg ->
            eprintfn "%s" msg
            1
        | InterpretErr msg ->
            eprintfn "%s" msg
            2
        | ReplErr msg ->
            eprintfn "%s" msg
            3
