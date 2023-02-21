module Feint.Compiler.Lexer.Tests

open Xunit

open Feint.Compiler
open Feint.Compiler.Token

[<Fact>]
let ``from string, check tokens`` () =
    let lexer = Lexer.fromString "1 + 1"

    Assert.Equal(lexer.pos, (1u, 0u))

    Assert.Equal(
        lexer.nextToken (),
        Token
            { startPos = (1u, 1u)
              endPos = (1u, 1u)
              token = (Int(bigint 1)) }
    )

    Assert.Equal(
        lexer.nextToken (),
        Token
            { startPos = (1u, 3u)
              endPos = (1u, 3u)
              token = Plus }
    )

    Assert.Equal(
        lexer.nextToken (),
        Token
            { startPos = (1u, 5u)
              endPos = (1u, 5u)
              token = (Int(bigint 1)) }
    )

    Assert.Equal(lexer.pos, (1u, 5u))

[<Fact>]
let ``unknown char causes syntax error`` () =
    let lexer = Lexer.fromString "~"
    Assert.Equal(lexer.nextToken (), SyntaxErr((1u, 1u), "Unhandled character: ~"))
