module Feint.Compiler.Lexer.Tests

open System.IO

open Xunit

open Feint.Compiler

let makeLexer = Lexer.fromText "<test>"

let makeLexerFromFile fileName =
    Lexer.fromFile (Path.Combine(__SOURCE_DIRECTORY__, fileName))

let assertTokenEqual result startPos endPos token =
    Assert.Equal(
        result,
        Lexer.Token(
            { startPos = startPos
              endPos = endPos
              token = token }
        )
    )

let assertSyntaxErrEqual result startPos endPos kind =
    Assert.Equal(
        result,
        Lexer.SyntaxErr(
            { fileName = "<test>"
              startPos = startPos
              endPos = endPos
              kind = kind }
        )
    )

[<Fact>]
let ``from string, check tokens`` () =
    let lexer = makeLexer "1 + 2"
    Assert.Equal(lexer.pos, ((1u, 0u), (1u, 0u)))
    assertTokenEqual (lexer.nextToken ()) (1u, 1u) (1u, 1u) (Token.Int(bigint 1))
    assertTokenEqual (lexer.nextToken ()) (1u, 3u) (1u, 3u) Token.Plus
    assertTokenEqual (lexer.nextToken ()) (1u, 5u) (1u, 5u) (Token.Int(bigint 2))
    Assert.Equal(lexer.pos, ((1u, 5u), (1u, 5u)))

[<Fact>]
let ``tab causes syntax error`` () =
    let lexer = makeLexer "\t"

    assertSyntaxErrEqual (lexer.nextToken ()) (1u, 1u) (1u, 1u) Errors.Tab

[<Fact>]
let ``unknown character causes syntax error`` () =
    let lexer = makeLexer "~"

    assertSyntaxErrEqual
        (lexer.nextToken ())
        (1u, 1u)
        (1u, 1u)
        (Errors.UnhandledChar '~')

[<Fact>]
let ``lex string`` () =
    let lexer = makeLexer "\"string\""
    assertTokenEqual (lexer.nextToken ()) (1u, 1u) (1u, 8u) (Token.Str "string")

[<Fact>]
let ``lex file`` () =
    let lexer = makeLexerFromFile "example.fi"
    assertTokenEqual (lexer.nextToken ()) (1u, 1u) (1u, 3u) Token.Nil
    assertTokenEqual (lexer.nextToken ()) (2u, 0u) (2u, 0u) Token.Newline
    assertTokenEqual (lexer.nextToken ()) (2u, 1u) (2u, 9u) (Token.Comment "# comment")
    let remainingTokens = lexer.tokens ()
    let lastToken = List.last remainingTokens
    Assert.Equal(lastToken, EOF)

[<Fact>]
let ``unterminated string causes syntax error`` () =
    let lexer = makeLexer "\"string"

    assertSyntaxErrEqual
        (lexer.nextToken ())
        (1u, 1u)
        (1u, 7u)
        (Errors.UnterminatedLiteralStr "\"string")
