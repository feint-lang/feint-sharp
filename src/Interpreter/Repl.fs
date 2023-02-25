module Feint.Interpreter.Repl

open System

open Feint.Interpreter.Interpreter

type Result =
    | ReadLineErr of Exception
    | Exit

type Repl() =
    let interpreter = Interpreter(true)

    // TODO: Implement history
    let mutable history_path: string option = None

    let rec readLine () =
        Console.Write("#> ")
        let line = Console.ReadLine()
        if line.Length > 0 then line else readLine ()

    let handleCommand =
        function
        | ".help"
        | "?" ->
            [ "Feint Help\n"
              ".help | ? -> show this help"
              ".exit -> exit"
              ".stack -> show interpreter stack (top first)" ]
            |> List.iter Console.Error.WriteLine

            None
        | ".exit"
        | ".quit" -> Some Exit
        | ".stack" ->
            interpreter.displayStack ()
            None
        | command ->
            Console.Error.WriteLine $"Unknown REPL command: {command}"
            None

    let interpretLine line : Result option = Some Exit
    // match parseText line "<repl>" with
    // | ParseResult.Statements statements ->
    //     try
    //         interpreter.interpret statements
    //     with InterpreterExc msg ->
    //         Console.Error.WriteLine msg
    // | ParseResult.Error err -> Console.Error.WriteLine err

    // None

    // Returns a `Result` to indicate that the REPL should shut down.
    let handleLine (line: string) =
        match line.StartsWith "." || line = "?" with
        | true -> handleCommand line
        | false -> interpretLine line

    let rec loop () =
        try
            let line = readLine ()

            match handleLine line with
            | Some result -> result
            | None -> loop ()
        with
        | :? NullReferenceException ->
            printfn ""
            Exit
        | exc -> ReadLineErr exc

    member _.start() =
        [ "Welcome to the Feint REPL"
          "Type a line of code, then hit Enter to evaluate it"
          "Enter .exit or .quit to exit" ]
        |> List.iter Console.Error.WriteLine

        Console.CancelKeyPress.Add(fun arg ->
            arg.Cancel <- true
            Console.Error.WriteLine "\nUse .exit, .quit, or Ctrl-D to exit"
            Console.Write("#> "))

        loop ()

let startRepl () =
    let repl = Repl()
    repl.start ()
