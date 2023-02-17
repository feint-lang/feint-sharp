module Feint.Interpreter.Interpreter

open System
open System.Collections.Generic

//open FSharp.Text.Lexing

open Feint.Compiler.Ast
open Feint.Compiler.Driver

exception InterpreterErr of string

let raiseErr msg = raise (InterpreterErr msg)

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

// Interpreter ---------------------------------------------------------

type Interpreter(show_statement_result) =
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

    let popDiscard () =
        let _ = pop ()
        ()

    member this.display_stack() =
        match stack.Count with
        | 0 -> Console.Error.WriteLine "[EMPTY]"
        | _ ->
            for v in stack do
                Console.Error.WriteLine $"{debugVal v}"

    member this.interpret statements =
        List.iter (fun statement -> this.interpret_statement statement) statements

    member this.interpret_statement statement =
        match statement with
        | Comment _ -> ()
        | DocComment _ -> ()
        | Return _ -> ()
        | Newline -> ()
        | ExprStatement e ->
            this.interpret_expr e
            let top = pop ()

            if show_statement_result && top <> NilVal then
                Console.Error.WriteLine $"-> {debugVal top}"

    member this.interpret_expr expr =
        match expr with
        | Nil _ -> pushNil ()
        | Bool v -> pushBool v
        | Int v -> pushInt v
        | Float v -> pushFloat v
        | Str v -> pushStr v
        | Ident name ->
            match names.TryGetValue name with
            | true, v -> push (v)
            | _ -> raiseErr $"Name not found: {name}"
        | Assignment a ->
            this.interpret_expr a.value
            names.Add(a.name, peek ())
        | Reassignment a ->
            match (names.ContainsKey a.name) with
            | true ->
                this.interpret_expr a.value
                names.Add(a.name, peek ())
            | false -> raiseErr $"Cannot reassign {a.name} because it's not already assigned"
        | BinaryOp op -> this.interpret_binary_op op.lhs op.op op.rhs
        | ShortCircuitOp op -> this.interpret_short_circuit_op op.lhs op.op op.rhs
        | CompareOp op -> this.interpret_compare_op op.lhs op.op op.rhs
        | Block statements -> this.interpret statements
        | Print args ->
            let handle_arg arg =
                this.interpret_expr arg
                displayVal (pop ())

            let args = List.map handle_arg args
            let args = String.concat " " args
            printfn $"{args}"
            pushNil ()
        | expr ->
            let msg = formatExpr expr
            raiseErr $"Unhandled expression: {msg}"

    // Binary Operations -----------------------------------------------

    member this.interpret_binary_op lhs op rhs =
        this.interpret_expr lhs
        this.interpret_expr rhs

        let rhs = pop ()
        let lhs = pop ()

        match op with
        | Pow -> this.interpret_pow lhs rhs
        | Mul -> this.interpret_mul lhs rhs
        | Div -> this.interpret_div lhs rhs
        | Add -> this.interpret_add lhs rhs
        | Sub -> this.interpret_sub lhs rhs
        | op -> raiseErr $"Unhandled binary op: {formatBinaryOp op}"

    member this.interpret_pow lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b when b >= bigint Int32.MinValue && b <= bigint Int32.MaxValue -> pushInt (a ** (int) b)
        | FloatVal a, FloatVal b -> pushFloat (a ** b)
        | _ -> raiseErr $"Cannot raise {debugVal lhs} to {debugVal rhs}"

    member this.interpret_mul lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a * b)
        | FloatVal a, FloatVal b -> pushFloat (a * b)
        | _ -> raiseErr $"Cannot multiply {debugVal lhs} by {debugVal rhs}"

    member this.interpret_div lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a / b)
        | FloatVal a, FloatVal b -> pushFloat (a / b)
        | _ -> raiseErr $"Cannot divide {debugVal lhs} by {debugVal rhs}"

    member this.interpret_add lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a + b)
        | FloatVal a, FloatVal b -> pushFloat (a + b)
        | _ -> raiseErr $"Cannot add {debugVal rhs} to {debugVal lhs}"

    member this.interpret_sub lhs rhs =
        match (lhs, rhs) with
        | IntVal a, IntVal b -> pushInt (a - b)
        | FloatVal a, FloatVal b -> pushFloat (a - b)
        | _ -> raiseErr $"Cannot subtract {debugVal rhs} from {debugVal lhs}"

    // Short Circuiting Binary Operations ------------------------------

    member this.interpret_short_circuit_op lhs op rhs =
        this.interpret_expr lhs
        let lhs = pop ()

        match op with
        | And -> this.interpret_and lhs rhs
        | Or -> this.interpret_or lhs rhs
        | NilOr -> this.interpret_nil_or lhs rhs

    member this.interpret_and lhs rhs =
        match lhs with
        | BoolVal true ->
            this.interpret_expr rhs
            let rhs = pop ()

            match rhs with
            | BoolVal true -> pushTrue ()
            | BoolVal false -> pushFalse ()
            | _ -> raiseErr $"Cannot apply {debugVal lhs} && {debugVal rhs}"
        | BoolVal false -> pushFalse ()
        | _ -> raiseErr $"Cannot apply && to {debugVal lhs}"

    member this.interpret_or lhs rhs =
        match lhs with
        | BoolVal true -> pushTrue ()
        | BoolVal false ->
            this.interpret_expr rhs
            let rhs = pop ()

            match rhs with
            | BoolVal true -> pushTrue ()
            | BoolVal false -> pushFalse ()
            | _ -> raiseErr $"Cannot apply {debugVal lhs} || {debugVal rhs}"
        | _ -> raiseErr $"Cannot apply || to {debugVal lhs}"

    member this.interpret_nil_or lhs rhs =
        match lhs with
        | NilVal -> this.interpret_expr rhs
        | _ -> push lhs

    // Comparison Operations -------------------------------------------

    member this.interpret_compare_op lhs op rhs =
        this.interpret_expr lhs
        this.interpret_expr rhs
        let rhs = pop ()
        let lhs = pop ()

        let result =
            match op with
            | EqEq -> this.interpret_eq lhs rhs
            | op -> raiseErr $"Unhandled comparison op: {formatCompareOp op}"

        pushBool result

    member this.interpret_eq lhs rhs =
        match (lhs, rhs) with
        | NilVal, NilVal -> true
        | BoolVal a, BoolVal b -> a = b
        | IntVal a, IntVal b -> a = b
        | FloatVal a, FloatVal b -> a = b
        | StrVal a, StrVal b -> a = b
        | _ -> false

let interpret maybeStatements =
    // TODO
    ()

let interpretText text fileName =
    // TODO
    ()

let interpretFile fileName =
    // TODO
    ()
