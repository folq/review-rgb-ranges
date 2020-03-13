module NoInvalidRGBValuesTest exposing (all)

import NoInvalidRGBValues exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoInvalidRGBValues"
        [ testValidRgbValues
        , testValidRgb255Values
        , testInvalidRgbValues
        , testInvalidRgb255Values
        ]


testValidRgbValues : Test
testValidRgbValues =
    [ "rgb 1 1 1"
    , "rgb 0.1 1 0.9"
    , "rgb 0 0 0"
    , "rgb 0.5 0.5 0.5"
    ]
        |> List.map (testAnd ExpectNoErrors)
        |> describe "Valid rgb values"


testValidRgb255Values : Test
testValidRgb255Values =
    [ "rgb255 0 20 255"
    , "rgb255 255 255 255"
    ]
        |> List.map (testAnd ExpectNoErrors)
        |> describe "Valid rgb 255values"


testInvalidRgbValues : Test
testInvalidRgbValues =
    [ "rgb 1 1 2"
    , "rgb 1 1 1.2"
    , "rgb 0 -1 1"
    , "rgb 0 -1 -2"
    , "rgb 0 valueDefinedSomewhereElse 1.2"
    , "rgb 0 valueDefinedSomewhereElse 2"
    , "rgb 0 valueDefinedSomewhereElse -2"
    , "rgb valueDefinedSomewhereElse anotherValue -2"
    ]
        |> List.map
            (\expression ->
                testAnd
                    (ExpectErrors
                        { message = "Invalid rgb value."
                        , details = [ "Each component must be within [0, 1]" ]
                        , under = expression
                        }
                    )
                    expression
            )
        |> describe "Invalid rgb values"


testInvalidRgb255Values : Test
testInvalidRgb255Values =
    [ "rgb255 128 128 -1"
    , "rgb255 128 128 256"
    , "rgb255 256 128 1"
    , "rgb255 0 -128 1"
    , "rgb255 0 valueDefinedSomewhereElse -1"
    , "rgb255 0 valueDefinedSomewhereElse 256"
    , "rgb255 valueDefinedSomewhereElse anotherValue 256"
    , "rgb255 valueDefinedSomewhereElse anotherValue -1"
    ]
        |> List.map
            (\expression ->
                testAnd
                    (ExpectErrors
                        { message = "Invalid rgb255 value."
                        , details = [ "Each component must be within [0, 255]" ]
                        , under = expression
                        }
                    )
                    expression
            )
        |> describe "Invalid rgb255 values"


testAnd : ExpectedOutcome -> String -> Test
testAnd expectedOutcome expression =
    let
        result =
            Review.Test.run rule <| "module TestModule exposing (..)\n\nvalue = " ++ expression
    in
    case expectedOutcome of
        ExpectNoErrors ->
            test ("should not report valid value '" ++ expression ++ "'") <| \() -> Review.Test.expectNoErrors result

        ExpectErrors errorFields ->
            test ("should report invalid value '" ++ expression ++ "'") <| \() -> Review.Test.expectErrors [ Review.Test.error errorFields ] result


type ExpectedOutcome
    = ExpectNoErrors
    | ExpectErrors { message : String, details : List String, under : String }
