open PowerArgs

open Feint.Compiler
open Feint.Compiler.Driver
open Feint.Interpreter.Interpreter
open Feint.Interpreter.Repl

// XXX: This is used as a hacky way to return an exit code from Main()
//      since PowerArgs doesn't seem to support exit codes when using
//      InvokeMain.
exception Exit of int

[<ArgExceptionBehavior(ArgExceptionPolicy.StandardExceptionHandling)>]
type Argv() =
    [<HelpHook>]
    [<ArgDescription("Show this help")>]
    [<ArgShortcut("-h")>]
    [<ArgShortcut("--help")>]
    member val help = false with get, set

    [<ArgDescription("Run code snippet")>]
    [<ArgShortcut("-c")>]
    [<ArgShortcut("--code")>]
    member val code = null with get, set

    [<ArgDescription("Run file")>]
    [<ArgExistingFile>]
    [<ArgPosition(0)>]
    member val fileName = null with get, set

    [<ArgDescription("Print tokens")>]
    [<ArgShortcut("-t")>]
    [<ArgShortcut("--tokens")>]
    member val tokens = false with get, set

    [<ArgDescription("Print AST instead of interpreting when running code or file")>]
    [<ArgShortcut("-a")>]
    [<ArgShortcut("--ast")>]
    member val ast = false with get, set

    member this.interpret maybeStatements =
        match maybeStatements with
        | Ok statements ->
            match this.ast with
            | true ->
                Ast.printStatements statements
                0
            | false ->
                let intepreter = Interpreter false

                try
                    intepreter.interpret statements
                    0
                with InterpreterErr msg ->
                    eprintfn "Error: %s" msg
                    1
        | Error msg ->
            // Lex or parse error
            eprintfn "%s" msg
            2

    member this.Main() : unit =
        let exitCode =
            match this.code with
            | null ->
                match this.fileName with
                // Launch REPL
                | null ->
                    let repl = Repl()
                    repl.start ()
                // Run file
                | _ ->
                    match this.tokens with
                    | true ->
                        printTokensFromFile this.fileName
                        0
                    | false -> this.interpret (parseFile this.fileName)
            // Run code
            | _ ->
                match this.tokens with
                | true ->
                    printTokensFromText this.code "<code>"
                    0
                | false -> this.interpret (parseText this.code)

        raise (Exit exitCode)

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
        | true ->
            // This will happen on --help
            0
        | _ ->
            match result.HandledException with
            | null ->
                // XXX: Unreachable?
                254
            | _ ->
                // This will happen when bad args are passed
                255
    with Exit exitCode ->
        exitCode
