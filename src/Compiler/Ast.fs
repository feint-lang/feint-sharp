module Feint.Compiler.Ast

// Operations ----------------------------------------------------------

type OpUnary =
    | NoOp
    | Negate
    | Not
    | AsBool

type OpBinary =
    | Pow
    | Mul
    | Div
    | FloorDiv
    | Mod
    | Add
    | Sub
    | Dot

type OpShortCircuit =
    | And
    | Or
    | NilOr

type OpCompare =
    | DollarDollar
    | DollarNot
    | EqEqEq
    | NotEqEq
    | EqEq
    | NotEq
    | LessThan
    | LessThanEq
    | GreaterThan
    | GreaterThanEq

type OpInPlace =
    | MulEq
    | DivEq
    | AddEq
    | SubEq

// AST -----------------------------------------------------------------

type Statement =
    | Comment of string
    | DocComment of string
    | Return of Expr option
    | ExprStatement of Expr
    | Newline

and Expr =
    | Nil
    | Bool of bool
    | Int of bigint
    | Float of float
    | Str of string
    // Identifiers
    | Ident of string
    | SpecialIdent of string
    // Operations
    | UnaryOp of UnaryOp
    | BinaryOp of BinaryOp
    | ShortCircuitOp of ShortCircuitOp
    | CompareOp of CompareOp
    | InPlaceOp of InPlaceOp
    // Assignment
    | Assignment of Assignment
    | Reassignment of Reassignment
    // Block
    | Block of Statement list
    // Print
    | Print of Expr list

and UnaryOp = { op: OpUnary; rhs: Expr }
and BinaryOp = { lhs: Expr; op: OpBinary; rhs: Expr }

and ShortCircuitOp =
    { lhs: Expr
      op: OpShortCircuit
      rhs: Expr }

and CompareOp = { lhs: Expr; op: OpCompare; rhs: Expr }
and InPlaceOp = { lhs: Expr; op: OpInPlace; rhs: Expr }
and Assignment = { name: string; value: Expr }
and Reassignment = { name: string; value: Expr }

// Display -------------------------------------------------------------

let formatUnaryOp =
    function
    | NoOp -> "+"
    | Negate -> "-"
    | Not -> "!"
    | AsBool -> "!!"

let formatBinaryOp =
    function
    | Pow -> "^"
    | Mul -> "*"
    | Div -> "/"
    | FloorDiv -> "//"
    | Mod -> "%"
    | Add -> "+"
    | Sub -> "-"
    | Dot -> "."

let formatShortCircuitOp =
    function
    | And -> "&&"
    | Or -> "||"
    | NilOr -> "??"

let formatCompareOp =
    function
    | DollarDollar -> "$$"
    | DollarNot -> "$!"
    | EqEqEq -> "==="
    | NotEqEq -> "!=="
    | EqEq -> "=="
    | NotEq -> "!="
    | LessThan -> "<"
    | LessThanEq -> "<="
    | GreaterThan -> ">"
    | GreaterThanEq -> ">="

let formatInPlaceOp =
    function
    | MulEq -> "*="
    | DivEq -> "/="
    | AddEq -> "+="
    | SubEq -> "-="

let makeIndent level = String.replicate (level * 4) " "

let rec formatStatements statements level =
    let formatter = fun statement -> formatStatement statement level
    let statements = List.map formatter statements
    String.concat "" statements

and formatStatement statement level =
    let indent = makeIndent level

    let result =
        match statement with
        | Comment content -> content
        | DocComment content -> content
        | Return maybe_expr ->
            match maybe_expr with
            | Some expr ->
                let expr = (formatExpr expr level)
                $"return {expr}"
            | None -> "return"
        | ExprStatement expr -> (formatExpr expr level)
        | Newline -> ""

    $"{indent}{result.TrimEnd()}\n"

and formatExpr expr level =
    let indent = makeIndent level

    match expr with
    // Types
    | Nil -> "nil"
    | Bool v -> v.ToString().ToLower()
    | Int v -> v.ToString()
    | Float v -> v.ToString()
    | Str v -> v
    // Operations
    | UnaryOp op -> $"{formatUnaryOp op.op}{formatExpr op.rhs level}"
    | BinaryOp op -> $"{formatBinOpExpr op.lhs (formatBinaryOp op.op) op.rhs}"
    | ShortCircuitOp op -> $"{formatBinOpExpr op.lhs (formatShortCircuitOp op.op) op.rhs}"
    | CompareOp op -> $"{formatBinOpExpr op.lhs (formatCompareOp op.op) op.rhs}"
    | InPlaceOp op -> $"{formatBinOpExpr op.lhs (formatInPlaceOp op.op) op.rhs}"
    // Assignments
    | Ident name -> name
    | SpecialIdent name -> name
    // Blocks
    | Block statements ->
        let statements = formatStatements statements (level + 1)
        $"\n{indent}block ->\n{statements}"
    // Print
    | Print args ->
        let formatter = fun expr -> formatExpr expr 0
        let args = String.concat ", " (List.map formatter args)
        $"$print {args}"
    | _ -> "unknown expr"

and formatBinOpExpr lhs op rhs level =
    $"{formatExpr lhs level} {op} {formatExpr rhs level}"

let printStatements statements =
    let statements = (formatStatements statements 0)
    printfn $"{statements.TrimEnd()}"
