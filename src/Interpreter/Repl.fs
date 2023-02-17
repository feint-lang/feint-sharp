module Feint.Interpreter.Repl

open System

open Feint.Compiler.Driver
open Feint.Interpreter.Interpreter

type Repl() =
    let interpreter = Interpreter(true)
    let mutable history_path: string option = None

    let rec read_line () =
        Console.Write("#> ")

        try
            let line = Console.ReadLine()
            if line.Length > 0 then Some(line) else read_line ()
        with :? NullReferenceException ->
            None

    let handle_command =
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
        | ".quit" -> Some 0
        | ".stack" ->
            interpreter.display_stack ()
            None
        | command ->
            Console.Error.WriteLine $"Unknown REPL command: {command}"
            None

    let interpret_line (line: string) : option<int> =
        match parseText line with
        | Ok statements ->
            try
                interpreter.interpret statements
            with InterpreterErr msg ->
                Console.Error.WriteLine msg
        | Error msg -> Console.Error.WriteLine msg

        None

    let handle_line (line: string) =
        match line.StartsWith "." || line = "?" with
        | true -> handle_command line
        | false -> interpret_line line

    let rec loop () =
        match read_line () with
        | Some line ->
            match handle_line line with
            | Some code -> code
            | None -> loop ()
        | None ->
            Console.Error.WriteLine()
            0

    member this.start() =
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
    // TODO
    ()
