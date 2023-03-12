module Feint.Compiler.Lexer.Tests

open System.IO

open Xunit

open Feint.Compiler

let makeLexer = fromText "<test>"

let makeLexerFromFile fileName =
    fromFile (Path.Combine(__SOURCE_DIRECTORY__, fileName))

let assertTokenEqual result span token =
    match result with
    | Token posToken ->
        Assert.Equal(span, posToken.span)
        Assert.Equal(token, posToken.token)
    | other -> Assert.True(false, $"Expected Token; got {other}")

let assertSyntaxErrEqual result span kind =
    match result with
    | SyntaxErr err ->
        Assert.Equal("<test>", err.fileName)
        Assert.Equal(span, err.span)
        Assert.Equal(kind, err.kind)
    | other -> Assert.True(false, $"Expected SyntaxErr; got {other}")

[<Fact>]
let ``from string, check tokens`` () =
    let lexer = makeLexer "1 + 2"
    Assert.Equal(((0u, 0u), (0u, 0u)), lexer.span)
    assertTokenEqual (lexer.next ()) ((1u, 1u), (1u, 1u)) (Tokens.Int(bigint 1))
    assertTokenEqual (lexer.next ()) ((1u, 3u), (1u, 3u)) Tokens.Plus
    assertTokenEqual (lexer.next ()) ((1u, 5u), (1u, 5u)) (Tokens.Int(bigint 2))
    Assert.Equal(((1u, 5u), (1u, 5u)), lexer.span)

[<Fact>]
let ``tab causes syntax error`` () =
    let lexer = makeLexer "\t"

    assertSyntaxErrEqual (lexer.next ()) ((1u, 1u), (1u, 1u)) Errors.Tab

[<Fact>]
let ``unknown character causes syntax error`` () =
    let lexer = makeLexer "~"

    assertSyntaxErrEqual (lexer.next ()) ((1u, 1u), (1u, 1u)) (Errors.UnhandledChar '~')

[<Fact>]
let ``lex string`` () =
    let lexer = makeLexer "\"string\""
    assertTokenEqual (lexer.next ()) ((1u, 1u), (1u, 8u)) (Tokens.Str "string")

[<Fact>]
let ``lex file`` () =
    let lexer = makeLexerFromFile "example.fi"

    assertTokenEqual (lexer.next ()) ((1u, 1u), (1u, 3u)) Tokens.Nil
    assertTokenEqual (lexer.next ()) ((1u, 4u), (1u, 4u)) Tokens.EndOfStatement
    assertTokenEqual (lexer.next ()) ((2u, 1u), (2u, 9u)) (Tokens.Comment "# comment")

    let remainingTokens = lexer.tokens ()
    let lastToken = List.last remainingTokens
    Assert.Equal(EOF, lastToken)

[<Fact>]
let ``unterminated string causes syntax error`` () =
    let lexer = makeLexer "\"string"

    assertSyntaxErrEqual
        (lexer.next ())
        ((1u, 1u), (1u, 7u))
        (Errors.UnterminatedLiteralStr "\"string")
