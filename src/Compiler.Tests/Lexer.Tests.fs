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
        Assert.Equal(posToken.span, span)
        Assert.Equal(posToken.token, token)
    | other -> Assert.True(false, $"Expected Token; got {other}")

let assertSyntaxErrEqual result span kind =
    match result with
    | SyntaxErr err ->
        Assert.Equal(err.fileName, "<test>")
        Assert.Equal(err.span, span)
        Assert.Equal(err.kind, kind)
    | other -> Assert.True(false, $"Expected SyntaxErr; got {other}")

[<Fact>]
let ``from string, check tokens`` () =
    let lexer = makeLexer "1 + 2"
    Assert.Equal(((0u, 0u), (0u, 0u)), lexer.span)
    assertTokenEqual (lexer.nextToken ()) ((1u, 1u), (1u, 1u)) (Tokens.Int(bigint 1))
    assertTokenEqual (lexer.nextToken ()) ((1u, 3u), (1u, 3u)) Tokens.Plus
    assertTokenEqual (lexer.nextToken ()) ((1u, 5u), (1u, 5u)) (Tokens.Int(bigint 2))
    Assert.Equal(((1u, 5u), (1u, 5u)), lexer.span)

[<Fact>]
let ``tab causes syntax error`` () =
    let lexer = makeLexer "\t"

    assertSyntaxErrEqual (lexer.nextToken ()) ((1u, 1u), (1u, 1u)) Errors.Tab

[<Fact>]
let ``unknown character causes syntax error`` () =
    let lexer = makeLexer "~"

    assertSyntaxErrEqual
        (lexer.nextToken ())
        ((1u, 1u), (1u, 1u))
        (Errors.UnhandledChar '~')

[<Fact>]
let ``lex string`` () =
    let lexer = makeLexer "\"string\""
    assertTokenEqual (lexer.nextToken ()) ((1u, 1u), (1u, 8u)) (Tokens.Str "string")

[<Fact>]
let ``lex file`` () =
    let lexer = makeLexerFromFile "example.fi"
    assertTokenEqual (lexer.nextToken ()) ((1u, 1u), (1u, 3u)) Tokens.Nil

    // assertTokenEqual (lexer.nextToken ()) ((2u, 0u), (1u, 3u)) Tokens.EndOfStatement
    // assertTokenEqual (lexer.nextToken ()) ((2u, 1u), (2u, 9u)) (Tokens.Comment "# comment")

    let remainingTokens = lexer.tokens ()
    let lastToken = List.last remainingTokens
    Assert.Equal(lastToken, EOF)

[<Fact>]
let ``unterminated string causes syntax error`` () =
    let lexer = makeLexer "\"string"

    assertSyntaxErrEqual
        (lexer.nextToken ())
        ((1u, 1u), (1u, 7u))
        (Errors.UnterminatedLiteralStr "\"string")
