module Feint.Interpreter.Interpreter

open System
open System.Collections.Generic

open Feint.Compiler.Ast

exception InterpreterExc of string

let raiseErr msg = raise (InterpreterExc msg)

type Result =
    | Success
    | ParseErr of string
    | InterpretErr of string

type StackVal =
    | NilVal
    | BoolVal of bool
    | IntVal of bigint
    | FloatVal of float
    | StrVal of string

let displayVal =
    function
    | NilVal -> "nil"
    | BoolVal v -> v.ToString().ToLower()
    | IntVal v -> v.ToString()
    | FloatVal v -> v.ToString()
    | StrVal v -> v

let debugVal =
    function
    | StrVal v -> $"\"{v}\""
    | v -> displayVal v

let bigintOfInt b =
    match b >= bigint Int32.MinValue && b <= bigint Int32.MaxValue with
    | true -> Some(int b)
    | false -> None

type Interpreter(showStatementResult) =
    let stack: Stack<StackVal> = Stack()
    let names: Dictionary<string, StackVal> = Dictionary()

    let push v = stack.Push(v)
    let pushNil () = stack.Push(NilVal)
    let pushBool v = stack.Push(BoolVal v)
    let pushTrue () = stack.Push(BoolVal true)
    let pushFalse () = stack.Push(BoolVal false)
    let pushInt v = stack.Push(IntVal v)
    let pushFloat v = stack.Push(FloatVal v)
    let pushStr v = stack.Push(StrVal v)

    let peek () = stack.Peek()
    let pop () = stack.Pop()

    member _.displayStack() =
        match stack.Count with
        | 0 -> Console.Error.WriteLine "[EMPTY]"
        | _ ->
            for v in stack do
                Console.Error.WriteLine $"{debugVal v}"

    member this.interpret statements =
        List.iter (fun statement -> this.interpretStatement statement) statements

    member this.interpretStatement statement =
        match statement with
        | Comment _ -> ()
        | DocComment _ -> ()
        | Return _ -> ()
        | Newline -> ()
        | ExprStatement e ->
            this.interpretExpr e
            let top = pop ()

            if showStatementResult && top <> NilVal then
                Console.Error.WriteLine $"-> {debugVal top}"

    member this.interpretExpr expr =
        match expr with
        | Nil _ -> pushNil ()
        | Bool v -> pushBool v
        | Int v -> pushInt v
        | Float v -> pushFloat v
        | Str v -> pushStr v
        | Ident name ->
            match names.TryGetValue name with
            | true, v -> push v
            | _ -> raiseErr $"Name not found: {name}"
        | Assignment a ->
            this.interpretExpr a.value
            names.Add(a.name, peek ())
        | Reassignment a ->
            match (names.ContainsKey a.name) with
            | true ->
                this.interpretExpr a.value
                names.Add(a.name, peek ())
            | false ->
                raiseErr $"Cannot reassign {a.name} because it's not already assigned"
        | BinaryOp op -> this.interpretBinaryOp op.lhs op.op op.rhs
        // | ShortCircuitOp op -> this.interpretShortCircuitOp op.lhs op.op op.rhs
        | CompareOp op -> this.interpretCompareOp op.lhs op.op op.rhs
        | Block statements -> this.interpret statements
        | Print args ->
            let handleArg arg =
                this.interpretExpr arg
                displayVal (pop ())

            let args = List.map handleArg args
            let args = String.concat " " args
            printfn $"{args}"
            pushNil ()
        | expr ->
            let msg = formatExpr expr 0
            raiseErr $"Expression not handled by interpreter: {msg}"

    // Binary Operations -----------------------------------------------

    member this.interpretBinaryOp lhs op rhs =
        this.interpretExpr lhs
        this.interpretExpr rhs

        let rhs = pop ()
        let lhs = pop ()

        match op with
        | Pow -> this.interpretPow lhs rhs
        | Mul -> this.interpretMul lhs rhs
        | Div -> this.interpretDiv lhs rhs
        | Add -> this.interpretAdd lhs rhs
        | Sub -> this.interpretSub lhs rhs
        | op -> raiseErr $"Unhandled binary op: {formatBinaryOp op}"

    member _.interpretPow lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b when (bigintOfInt b).IsSome -> pushInt (a ** int b)
        | FloatVal a, FloatVal b -> pushFloat (a ** b)
        | _ -> raiseErr $"Cannot raise {debugVal lhs} to {debugVal rhs}"

    member _.interpretMul lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a * b)
        | FloatVal a, FloatVal b -> pushFloat (a * b)
        | _ -> raiseErr $"Cannot multiply {debugVal lhs} by {debugVal rhs}"

    member _.interpretDiv lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a / b)
        | FloatVal a, FloatVal b -> pushFloat (a / b)
        | _ -> raiseErr $"Cannot divide {debugVal lhs} by {debugVal rhs}"

    member _.interpretAdd lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a + b)
        | FloatVal a, FloatVal b -> pushFloat (a + b)
        | _ -> raiseErr $"Cannot add {debugVal rhs} to {debugVal lhs}"

    member _.interpretSub lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a - b)
        | FloatVal a, FloatVal b -> pushFloat (a - b)
        | _ -> raiseErr $"Cannot subtract {debugVal rhs} from {debugVal lhs}"

    // Short Circuiting Binary Operations ------------------------------

    member this.interpretShortCircuitOp lhs op rhs =
        this.interpretExpr lhs
        let lhs = pop ()

        match op with
        | And -> this.interpretAnd lhs rhs
        | Or -> this.interpretOr lhs rhs
        | NilOr -> this.interpretNilOr lhs rhs

    member this.interpretAnd lhs rhs =
        match lhs with
        | BoolVal true ->
            this.interpretExpr rhs
            let rhs = pop ()

            match rhs with
            | BoolVal true -> pushTrue ()
            | BoolVal false -> pushFalse ()
            | _ -> raiseErr $"Cannot apply {debugVal lhs} && {debugVal rhs}"
        | BoolVal false -> pushFalse ()
        | _ -> raiseErr $"Cannot apply && to {debugVal lhs}"

    member this.interpretOr lhs rhs =
        match lhs with
        | BoolVal true -> pushTrue ()
        | BoolVal false ->
            this.interpretExpr rhs
            let rhs = pop ()

            match rhs with
            | BoolVal true -> pushTrue ()
            | BoolVal false -> pushFalse ()
            | _ -> raiseErr $"Cannot apply {debugVal lhs} || {debugVal rhs}"
        | _ -> raiseErr $"Cannot apply || to {debugVal lhs}"

    member this.interpretNilOr lhs rhs =
        match lhs with
        | NilVal -> this.interpretExpr rhs
        | _ -> push lhs

    // Comparison Operations -------------------------------------------

    member this.interpretCompareOp lhs op rhs =
        this.interpretExpr lhs
        this.interpretExpr rhs
        let rhs = pop ()
        let lhs = pop ()

        let result =
            match op with
            | EqEq -> this.interpretEq lhs rhs
            | op -> raiseErr $"Unhandled comparison op: {formatCompareOp op}"

        pushBool result

    member _.interpretEq lhs rhs =
        match (lhs, rhs) with
        | NilVal, NilVal -> true
        | BoolVal a, BoolVal b -> a = b
        | IntVal a, IntVal b -> a = b
        | FloatVal a, FloatVal b -> a = b
        | StrVal a, StrVal b -> a = b
        | _ -> false

// Interpreter Entrypoints ---------------------------------------------

let interpret statements =
    let interpreter = Interpreter false

    try
        interpreter.interpret statements
        Success
    with InterpreterExc msg ->
        InterpretErr msg

let handleParseResultForInterpret =
    InterpretErr "handleParseResultForInterpret not implemented"
// function
// | Driver.Statements statements -> interpret statements
// | Driver.Error err -> ParseErr err

let interpretText text fileName =
    InterpretErr "interpretFile not implemented"
// Driver.parseText text fileName |> handleParseResultForInterpret

let interpretFile fileName =
    InterpretErr "interpretFile not implemented"
// Driver.parseFile fileName |> handleParseResultForInterpret
