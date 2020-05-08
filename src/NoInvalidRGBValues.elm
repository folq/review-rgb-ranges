module NoInvalidRGBValues exposing (rule)

{-| Make sure rgb and rgb255 arguments are within the ranges [0, 1] and [0, 255] respectively.

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Make sure that `rgb` and `rgb255` values make sense.

The rule is meant to be used with the color functions `rgb` and `rgb255` from `elm-ui`, and makes sure that the 3 arguments
passed to `rgb` are within [0, 1] and that the arguments to `rgb255` are within [0, 255].

If you want to use this rule, add it to `config : List Rule` in `review/ReviewConfig.elm`

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "RgbColorsInValidRange" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Application app ->
            case List.map Node.value app of
                (FunctionOrValue _ "rgb255") :: er :: eg :: eb :: [] ->
                    if atLeastOneInvalidExpression expressionToInt validRgb255 [ er, eg, eb ] then
                        [ Rule.error
                            { message = "Invalid rgb255 value."
                            , details = [ "Each component must be within [0, 255]." ]
                            }
                            (Node.range node)
                        ]

                    else
                        []

                (FunctionOrValue _ "rgb") :: er :: eg :: eb :: [] ->
                    let
                        defaultError =
                            [ Rule.error
                                { message = "Invalid rgb value."
                                , details = [ "Each component must be within [0, 1]." ]
                                }
                                (Node.range node)
                            ]
                    in
                    if atLeastOneInvalidExpression expressionToFloat validRgb [ er, eg, eb ] then
                        case List.map expressionToInt [ er, eg, eb ] of
                            [ Just r, Just g, Just b ] ->
                                if List.all validRgb255 [ r, g, b ] then
                                    [ Rule.errorWithFix
                                        { message = "Invalid rgb value."
                                        , details = [ "Each component must be within [0, 1]." ]
                                        }
                                        (Node.range node)
                                        [ Fix.replaceRangeBy (Node.range node) <| "rgb255 " ++ (String.join " " <| List.map String.fromInt [ r, g, b ]) ]
                                    ]

                                else
                                    defaultError

                            _ ->
                                defaultError

                    else
                        []

                (FunctionOrValue _ "rgba") :: er :: eg :: eb :: ea :: [] ->
                    if atLeastOneInvalidExpression expressionToFloat validRgb [ er, eg, eb ] || atLeastOneInvalidExpression expressionToFloat validRgb [ ea ] then
                        [ Rule.error
                            { message = "Invalid rgba value."
                            , details = [ "Each component must be within [0, 1], including alpha value." ]
                            }
                            (Node.range node)
                        ]

                    else
                        []

                (FunctionOrValue _ "rgba255") :: er :: eg :: eb :: ea :: [] ->
                    if atLeastOneInvalidExpression expressionToInt validRgb255 [ er, eg, eb ] || atLeastOneInvalidExpression expressionToFloat validRgb [ ea ] then
                        [ Rule.error
                            { message = "Invalid rgba255 value."
                            , details = [ "Each component must be within [0, 255], and alpha value must be within [0, 1]." ]
                            }
                            (Node.range node)
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


atLeastOneInvalidExpression : (Expression -> Maybe a) -> (a -> Bool) -> List Expression -> Bool
atLeastOneInvalidExpression convertFunction isValid =
    List.any ((==) (Just True) << Maybe.map (not << isValid) << convertFunction)


expressionToInt : Expression -> Maybe Int
expressionToInt expression =
    case expression of
        Integer n ->
            Just n

        Negation e ->
            Maybe.map ((*) -1) <| expressionToInt <| Node.value e

        _ ->
            Nothing


expressionToFloat : Expression -> Maybe Float
expressionToFloat expression =
    case expression of
        Integer n ->
            Just <| toFloat n

        Floatable n ->
            Just n

        Negation e ->
            Maybe.map ((*) -1) <| expressionToFloat <| Node.value e

        _ ->
            Nothing


validRgb255 : Int -> Bool
validRgb255 n =
    n >= 0 && n <= 255


validRgb : Float -> Bool
validRgb n =
    n >= 0 && n <= 1
