module Feint.Compiler.Lexer.Tests

open Xunit

open Feint.Compiler
open Feint.Compiler.Token

let makeLexer = Lexer.fromText "<test>"

let assertTokenEqual result startPos endPos token =
    Assert.Equal(result, Lexer.Token(startPos, endPos, token))

let assertSyntaxErrEqual result startPos endPos kind =
    Assert.Equal(result, SyntaxErr("<test>", startPos, endPos, kind))

[<Fact>]
let ``from string, check tokens`` () =
    let lexer = makeLexer "1 + 2"
    Assert.Equal(lexer.pos, ((1u, 0u), (1u, 0u)))
    assertTokenEqual (lexer.nextToken ()) (1u, 1u) (1u, 1u) (Int(bigint 1))
    assertTokenEqual (lexer.nextToken ()) (1u, 3u) (1u, 3u) Plus
    assertTokenEqual (lexer.nextToken ()) (1u, 5u) (1u, 5u) (Int(bigint 2))
    Assert.Equal(lexer.pos, ((1u, 5u), (1u, 5u)))

[<Fact>]
let ``tab causes syntax error`` () =
    let lexer = makeLexer "\t"

    assertSyntaxErrEqual (lexer.nextToken ()) (1u, 1u) (1u, 1u) Tab

[<Fact>]
let ``unknown character causes syntax error`` () =
    let lexer = makeLexer "~"
    assertSyntaxErrEqual (lexer.nextToken ()) (1u, 1u) (1u, 1u) (UnhandledChar '~')

[<Fact>]
let ``parse string`` () =
    let lexer = makeLexer "\"string\""
    assertTokenEqual (lexer.nextToken ()) (1u, 1u) (1u, 8u) (Str("string"))

[<Fact>]
let ``unterminated string causes syntax error`` () =
    let lexer = makeLexer "\"string"

    assertSyntaxErrEqual
        (lexer.nextToken ())
        (1u, 1u)
        (1u, 7u)
        (UnterminatedStringLiteral "\"string")
