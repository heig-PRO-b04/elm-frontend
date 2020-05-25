module Page.PollStatistics.Charts exposing (answersChart)

{-| A page sub-module that displays line graph statistics


# views

@docs answersChart

-}

import Api.Statistics as Api
import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HAttribute
import Svg
import Svg.Attributes as SAttribute



-- ARGUMENTS


colors : Array String
colors =
    Array.fromList <|
        [ "#80D8FF"
        , "#40C4FF"
        , "#00B0FF"
        , "#0091EA"
        ]


color : Int -> String
color number =
    if number <= 0 then
        "#81D4FA"

    else
        Array.get (modBy (Array.length colors) number) colors
            |> Maybe.withDefault "#81D4FA"



-- VIEW


answersChart : Api.QuestionStatistics -> Html msg
answersChart question =
    let
        answersCount =
            totalVotes question
    in
    Html.div
        [ HAttribute.class "m-4" ]
        ([ Svg.svg
            [ SAttribute.width "100%", SAttribute.height "25" ]
            [ Svg.g []
                [ Svg.g [ SAttribute.class "bars" ]
                    (rects question.answers)
                ]
            ]
         , Html.div [ HAttribute.class "font-semibold mt-4" ]
            [ Html.text question.title
            , Html.span [ HAttribute.class "text-gray-600" ]
                [ Html.text " / "
                , Html.text (String.fromInt answersCount)
                , Html.text " votes"
                ]
            ]
         ]
            ++ caption question.answers
        )



-- GRAPHICS


rects : List Api.AnswerStatistics -> List (Html msg)
rects answers =
    if (List.map .positive answers |> List.sum) == 0 then
        List.singleton <|
            Svg.rect
                [ SAttribute.fill "#E2E8F0"
                , SAttribute.width "100%"
                , SAttribute.height "25"
                ]
                []

    else
        List.sortBy .title answers
            |> List.map .positive
            |> staircase
            |> List.indexedMap (\i pos -> rect i pos (List.map .positive answers |> List.sum))
            |> List.reverse


rect : Int -> Int -> Int -> Html msg
rect index total max =
    let
        range =
            100 * toFloat total / toFloat max
    in
    Svg.rect
        [ SAttribute.fill <| color index
        , SAttribute.width <| String.fromFloat range ++ "%"
        , SAttribute.height "25"
        ]
        []


caption : List Api.AnswerStatistics -> List (Html msg)
caption answers =
    List.sortBy .title answers
        |> List.indexedMap (\i a -> ( a, color i ))
        |> List.map
            (\( a, c ) ->
                Html.div
                    [ HAttribute.class "font-semibold text-gray-600 flex flex-row items-center" ]
                    [ Html.text a.title
                    , Html.span [ HAttribute.class "text-gray-400" ]
                        [ Html.text "\u{00A0}/\u{00A0}"
                        , Html.text (String.fromInt a.positive)
                        , Html.text "\u{00A0}votes"
                        ]
                    , Html.div [ HAttribute.class "flex-grow" ] []
                    , Html.div [ HAttribute.class "w-4 h-4 rounded inline-block", HAttribute.style "background-color" c ] []
                    ]
            )



-- UTILS


totalVotes : Api.QuestionStatistics -> Int
totalVotes question =
    List.map .positive question.answers
        |> List.sum



-- STAIRCASE DATA ACCUMULATION


staircase : List number -> List number
staircase numbers =
    staircaseHelper 0 numbers


staircaseHelper : number -> List number -> List number
staircaseHelper current values =
    case values of
        [] ->
            values

        x :: xs ->
            (current + x) :: staircaseHelper (current + x) xs
