module Feint.Compiler.Combinators.Tests

open Xunit

open Feint.Compiler
open Feint.Compiler.Combinators

let assertIsSuccess (actual: Result<'a * string>) (expectedData: 'a) expectedInput =
    match actual with
    | Success(actualData, actualInput) ->
        Assert.Equal<'a>(expectedData, actualData)
        Assert.Equal(actualInput, expectedInput)
    | _ -> Assert.False(false, "Expected Success")

// Pass `Some(expectedMessage)` to check the failure message.
// Pass `None` to skip checking the failure message.
let assertIsFailure result expectedMessage =
    match (result, expectedMessage) with
    | (Failure _, None) -> Assert.True(true)
    | (Failure actualMessage, Some expectedMessage) ->
        Assert.Equal(expectedMessage, actualMessage)
    | _ -> Assert.False(false, "Expected Failure")

// Char ----------------------------------------------------------------

[<Fact>]
let ``match single character`` () =
    let actual = run (matchChar 'x') "x"
    assertIsSuccess actual 'x' ""

[<Fact>]
let ``match single character from string`` () =
    let actual = run (matchChar 'x') "xyz"
    assertIsSuccess actual 'x' "yz"

[<Fact>]
let ``fail to match single character`` () =
    let actual = run (matchChar 'x') "y"
    assertIsFailure actual (Some "Expected char 'x'; got 'y'")

[<Fact>]
let ``fail to match single character from string`` () =
    let actual = run (matchChar 'x') "yx"
    assertIsFailure actual (Some "Expected char 'x'; got 'y'")

// Digit ---------------------------------------------------------------

[<Fact>]
let ``match zero`` () =
    let actual = run (zero) "0"
    assertIsSuccess actual '0' ""

[<Fact>]
let ``match single digit`` () =
    let actual = run (digit) "1"
    assertIsSuccess actual '1' ""

[<Fact>]
let ``match sequence of two digits`` () =
    let actual = run (sequence [ digit; digit ]) "12"
    assertIsSuccess actual [ '1'; '2' ] ""

    let actual = run (sequence [ digit; digit ]) "12a"
    assertIsSuccess actual [ '1'; '2' ] "a"

// Numbers -------------------------------------------------------------
