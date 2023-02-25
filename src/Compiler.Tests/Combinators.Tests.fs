module Feint.Compiler.Combinators.Tests

open Xunit

open Feint.Compiler
open Feint.Compiler.Combinators
open Feint.Compiler.Tokens

let assertIsSuccess (actual: Result<'a>) (expectedValue: 'a) =
    match actual with
    | Success actualValue -> Assert.Equal<'a>(expectedValue, actualValue)
    | _ -> Assert.True(false, $"Expected Success; got {actual}")

// Pass `Some(expectedMessage)` to check the failure message.
// Pass `None` to skip checking the failure message.
let assertIsParseErr result expectedMessage =
    match (result, expectedMessage) with
    | (ParseErr _, None) -> Assert.True(true)
    | (ParseErr actualMessage, Some expectedMessage) ->
        Assert.Equal(expectedMessage, actualMessage)
    | _ -> Assert.False(false, "Expected ParseErr")

[<Fact>]
let ``matching a single nil token should succeed`` () =
    let lexer = Lexer.fromText "<test>" "nil"
    let expected = makePosToken (1u, 1u) (1u, 3u) Nil
    let actual = run (matchToken Nil) lexer
    assertIsSuccess actual expected
