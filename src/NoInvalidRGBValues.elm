module NoInvalidRGBValues exposing (rule)

{-| Make sure rgb and rgb255 arguments are within the ranges [0, 1] and [0, 255] respectively.

To be used with <https://package.elm-lang.org/packages/jfmengels/elm-review/latest/>


# Rule

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Make sure that `rgb` and `rgb255` values make sense.

The rule is meant to be used with the color functions `rgb` and `rgb255` from `elm-ui`, and makes sure that the 3 arguments
passed to `rgb` are within [0, 1] and that the arguments to `rgb255` are within [0, 255].

If you want to use this rule, add it to `config : List Rule` in `review/ReviewConfig.elm`

-}
rule : Rule
rule =
    Rule.newSchema "RgbColorsInValidRange"
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        Application app ->
            case List.map Node.value app of
                (FunctionOrValue _ "rgb255") :: er :: eg :: eb :: [] ->
                    if invalidTriplet expressionToInt validRgb255 ( er, eg, eb ) then
                        [ Rule.error
                            { message = "Invalid rgb255 value."
                            , details = [ "Each component must be within [0, 255]" ]
                            }
                            (Node.range node)
                        ]

                    else
                        []

                (FunctionOrValue _ "rgb") :: er :: eg :: eb :: [] ->
                    if invalidTriplet expressionToFloat validRgb ( er, eg, eb ) then
                        [ Rule.error
                            { message = "Invalid rgb value."
                            , details = [ "Each component must be within [0, 1]" ]
                            }
                            (Node.range node)
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


invalidTriplet : (Expression -> Maybe a) -> (a -> Bool) -> ( Expression, Expression, Expression ) -> Bool
invalidTriplet convertFunction isValid ( e1, e2, e3 ) =
    case ( convertFunction e1, convertFunction e2, convertFunction e3 ) of
        ( Just r, Just g, Just b ) ->
            List.any (not << isValid) [ r, g, b ]

        _ ->
            False


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
