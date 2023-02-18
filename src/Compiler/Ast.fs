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
    // Assignment
    | Assignment of Assignment
    | Reassignment of Reassignment
    // Operations
    | UnaryOp of UnaryOp
    | BinaryOp of BinaryOp
    | ShortCircuitOp of ShortCircuitOp
    | CompareOp of CompareOp
    | InPlaceOp of InPlaceOp
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
    match expr with
    // Types
    | Nil -> "nil"
    | Bool v -> v.ToString().ToLower()
    | Int v -> v.ToString()
    | Float v -> v.ToString()
    | Str v -> v
    // Identifiers
    | Ident name -> name
    | SpecialIdent name -> name
    // Assigments
    | Assignment a -> $"{a.name} = {a.value}"
    | Reassignment a -> $"{a.name} <- {a.value}"
    // Operations
    | UnaryOp op -> $"{formatUnaryOpExpr op}"
    | BinaryOp op -> $"{formatBinOpExpr op}"
    | ShortCircuitOp op -> $"{formatShortCircuitOpExpr op}"
    | CompareOp op -> $"{formatCompareOpExpr op}"
    | InPlaceOp op -> $"{formatInPlaceOpExpr op}"
    // Other
    | Block statements -> formatBlockExpr statements level
    | Print args -> formatPrintExpr args

and formatUnaryOpExpr op =
    $"{formatUnaryOp op.op} {formatExpr op.rhs 0}"

and formatBinOpExpr op =
    $"{formatExpr op.lhs 0} {formatBinaryOp op.op} {formatExpr op.rhs 0}"

and formatShortCircuitOpExpr op =
    $"{formatExpr op.lhs 0} {formatShortCircuitOp op.op} {formatExpr op.rhs 0}"

and formatCompareOpExpr op =
    $"{formatExpr op.lhs 0} {formatCompareOp op.op} {formatExpr op.rhs 0}"

and formatInPlaceOpExpr op =
    $"{formatExpr op.lhs 0} {formatInPlaceOp op.op} {formatExpr op.rhs 0}"

and formatBlockExpr statements level =
    let indent = makeIndent level
    let statements = formatStatements statements (level + 1)
    $"\n{indent}block ->\n{statements}"

and formatPrintExpr args =
    let formatter = fun expr -> formatExpr expr 0
    let args = String.concat ", " (List.map formatter args)
    $"$print {args}"

let printStatements statements =
    let statements = (formatStatements statements 0)
    printfn $"{statements.TrimEnd()}"
