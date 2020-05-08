module NoInvalidRGBValuesTest exposing (all)

import NoInvalidRGBValues exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoInvalidRGBValues"
        [ testValidRgbValues
        , testValidRgb255Values
        , testValidRgbaValues
        , testValidRgba255Values
        , testInvalidRgbValues
        , testInvalidRgbValuesWithFixes
        , testInvalidRgb255Values
        , testInvalidRgbaValues
        , testInvalidRgba255Values
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
        |> describe "Valid rgb255 values"


testValidRgbaValues : Test
testValidRgbaValues =
    [ "rgba 1 1 1"
    , "rgba 0.1 1 0.9"
    , "rgba 0 0 0"
    , "rgba 0.5 0.5 0.5"
    ]
        |> List.map (testAnd ExpectNoErrors)
        |> describe "Valid rgba values"


testValidRgba255Values : Test
testValidRgba255Values =
    [ "rgba255 0 20 255"
    , "rgba255 255 255 255"
    ]
        |> List.map (testAnd ExpectNoErrors)
        |> describe "Valid rgba255 values"


testInvalidRgbValues : Test
testInvalidRgbValues =
    [ "rgb 0 -1 1"
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
                        , details = [ "Each component must be within [0, 1]." ]
                        , under = expression
                        }
                    )
                    expression
            )
        |> describe "Invalid rgb values"


testInvalidRgbValuesWithFixes : Test
testInvalidRgbValuesWithFixes =
    [ ( "rgb 255 255 255", "rgb255 255 255 255" )
    , ( "rgb 128 255 255", "rgb255 128 255 255" )
    , ( "rgb 0 255 255", "rgb255 0 255 255" )
    , ( "rgb 128 10 124", "rgb255 128 10 124" )
    ]
        |> List.map
            (\( expression, fixedExpression ) ->
                testAnd
                    (ExpectErrorsAndThenAFixedExpression
                        { message = "Invalid rgb value."
                        , details = [ "Each component must be within [0, 1]." ]
                        , under = expression
                        }
                        { fixedExpression = fixedExpression }
                    )
                    expression
            )
        |> describe "Invalid rgb values, with fixes"


testInvalidRgbaValues : Test
testInvalidRgbaValues =
    [ "rgba 1 1 1 2"
    , "rgba 0 1 1 -1"
    , "rgba 0 -1 -2 1"
    , "rgba 0 valueDefinedSomewhereElse 1.2 1"
    , "rgba 0 valueDefinedSomewhereElse 2 1"
    , "rgba 0 valueDefinedSomewhereElse -2 1"
    , "rgba valueDefinedSomewhereElse anotherValue -2 -1"
    ]
        |> List.map
            (\expression ->
                testAnd
                    (ExpectErrors
                        { message = "Invalid rgba value."
                        , details = [ "Each component must be within [0, 1], including alpha value." ]
                        , under = expression
                        }
                    )
                    expression
            )
        |> describe "Invalid rgba values"


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
                        , details = [ "Each component must be within [0, 255]." ]
                        , under = expression
                        }
                    )
                    expression
            )
        |> describe "Invalid rgb255 values"


testInvalidRgba255Values : Test
testInvalidRgba255Values =
    [ "rgba255 128 128 1 -1"
    , "rgba255 128 128 255 255"
    , "rgba255 256 128 1 -1"
    , "rgba255 0 -128 1 1"
    , "rgba255 0 valueDefinedSomewhereElse -1 1"
    , "rgba255 0 valueDefinedSomewhereElse 256 1"
    , "rgba255 valueDefinedSomewhereElse anotherValue 256 1"
    , "rgba255 valueDefinedSomewhereElse anotherValue -1 1"
    ]
        |> List.map
            (\expression ->
                testAnd
                    (ExpectErrors
                        { message = "Invalid rgba255 value."
                        , details = [ "Each component must be within [0, 255], and alpha value must be within [0, 1]." ]
                        , under = expression
                        }
                    )
                    expression
            )
        |> describe "Invalid rgba255 values"


testAnd : ExpectedOutcome -> String -> Test
testAnd expectedOutcome expression =
    let
        result =
            Review.Test.run rule <| fileContentWithSimpleExpression expression
    in
    case expectedOutcome of
        ExpectNoErrors ->
            test ("should not report valid value '" ++ expression ++ "'") <| \() -> Review.Test.expectNoErrors result

        ExpectErrors errorFields ->
            test ("should report invalid value '" ++ expression ++ "'") <| \() -> Review.Test.expectErrors [ Review.Test.error errorFields ] result

        ExpectErrorsAndThenAFixedExpression errorFields { fixedExpression } ->
            test ("should report invalid value '" ++ expression ++ "' but also suggest a fix") <|
                \() ->
                    Review.Test.expectErrors
                        [ Review.Test.error errorFields
                            |> Review.Test.whenFixed (fileContentWithSimpleExpression fixedExpression)
                        ]
                        result


fileContentWithSimpleExpression : String -> String
fileContentWithSimpleExpression expression =
    "module TestModule exposing (..)\n\nvalue = " ++ expression


type ExpectedOutcome
    = ExpectNoErrors
    | ExpectErrors { message : String, details : List String, under : String }
    | ExpectErrorsAndThenAFixedExpression { message : String, details : List String, under : String } { fixedExpression : String }
