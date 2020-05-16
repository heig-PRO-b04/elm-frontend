module Page.QuestionStatistics exposing
    ( Message
    , Model
    , init
    , subscriptions
    , update
    , view
    )

import Api exposing (Credentials)
import Api.QuestionStatistics as Api
import Array
import Cmd.Extra
import Color exposing (Color)
import Dict
import Html exposing (Html)
import Html.Attributes as Attribute
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Session exposing (Viewer)
import Task
import Task.Extra
import Time



-- MODEL


{-| How many seconds are elapsed between two measurements of the question live metrics.
-}
frequency : number
frequency =
    15


{-| How many intervals should be displayed.
-}
count : number
count =
    10


type alias Model =
    { viewer : Viewer
    , idPoll : Int
    , idQuestion : Int
    , response : Maybe Api.Response
    , timezone : Time.Zone
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    ( { viewer = viewer
      , idPoll = discriminator.idPoll
      , idQuestion = discriminator.idQuestion
      , response = Nothing
      , timezone = Time.utc
      }
    , Cmd.batch [ Cmd.Extra.succeed RequestUpdate, cmdTimezone ]
    )



-- UPDATE


type Message
    = RequestUpdate
    | GotResponse Api.Response
    | GotTimeZone Time.Zone
    | GotError Api.StatisticsError


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every (frequency * 1000) (always RequestUpdate)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        RequestUpdate ->
            ( model, cmdLatestResponse (Session.viewerCredentials model.viewer) model )

        GotResponse response ->
            ( { model | response = Just response }, Cmd.none )

        GotError _ ->
            ( { model | response = Nothing }, Cmd.none )

        GotTimeZone zone ->
            ( { model | timezone = zone }, Cmd.none )



-- EFFECTS


cmdTimezone : Cmd Message
cmdTimezone =
    Task.perform GotTimeZone <| Time.here


cmdLatestResponse : Credentials -> { d | idPoll : Int, idQuestion : Int } -> Cmd Message
cmdLatestResponse credentials discriminator =
    let
        times now remaining =
            if remaining <= 0 then
                []

            else
                now :: times (now - frequency) (remaining - 1)
    in
    Time.now
        |> Task.map Time.posixToMillis
        |> Task.map (\time -> time // 1000)
        |> Task.andThen
            (\time ->
                Api.getStatisticsForQuestion credentials
                    discriminator
                    (times time count)
                    GotResponse
            )
        |> Task.mapError GotError
        |> Task.Extra.execute



-- VIEW


view : Model -> Html Message
view model =
    case model.response of
        Just data ->
            Html.div [ Attribute.class "w-full bg-gray-100 border-b" ]
                [ Html.div [ Attribute.class "mx-auto w-1/2" ]
                    [ LineChart.viewCustom (chartConfig model)
                        (responseToSeries data)
                    ]
                ]

        Nothing ->
            Html.div [] []


responseToSeries : Api.Response -> List (LineChart.Series ( Int, Int ))
responseToSeries response =
    let
        titles : Dict.Dict Int String
        titles =
            response.answers
                |> List.map (\answer -> ( answer.idAnswer, answer.title ))
                |> Dict.fromList

        answers : Dict.Dict Int (List a)
        answers =
            response.answers
                |> List.map .idAnswer
                |> List.map (\tuple -> ( tuple, [] ))
                |> Dict.fromList

        data : Dict.Dict Int (List ( Int, Int ))
        data =
            response.timestamps
                |> List.foldr
                    -- Iterate on all the timestamps.
                    (\timestamp outer ->
                        List.foldr
                            -- Iterate on all the votes.
                            (\vote inner ->
                                Dict.update vote.idAnswer
                                    (Maybe.map (\v -> ( timestamp.seconds, vote.count ) :: v))
                                    inner
                            )
                            outer
                            timestamp.votes
                    )
                    answers

        lines : List (LineChart.Series ( Int, Int ))
        lines =
            Dict.toList data
                |> List.indexedMap
                    (\index ( id, values ) ->
                        LineChart.dash (color index)
                            Dots.none
                            (Dict.get id titles |> Maybe.withDefault "Series")
                            [ 4, 2 ]
                            values
                    )
    in
    lines


color : Int -> Color
color number =
    let
        array =
            Array.fromList <|
                --[ Color.rgb255 0x80 0xD8 0xFF
                --, Color.rgb255 0x40 0xC4 0xFF
                --, Color.rgb255 0x00 0xB0 0xFF
                --, Color.rgb255 0x00 0x91 0xEA
                --]
                [ Colors.cyan
                , Colors.pink
                , Colors.teal
                , Colors.gold
                ]
    in
    if number <= 0 then
        Color.rgb255 0x81 0xD4 0xFA

    else
        Array.get (modBy (Array.length array) number) array
            |> Maybe.withDefault (Color.rgb255 0x81 0xD4 0xFA)



-- CHART CONFIGURATION


chartConfig : Model -> LineChart.Config ( Int, Int ) msg
chartConfig model =
    let
        axis =
            Axis.custom
                { title = Title.default "Votes"
                , variable = Just << (Tuple.second >> toFloat)
                , pixels = 400
                , range = Range.padded 16 16
                , axisLine = AxisLine.default
                , ticks = Ticks.int 1
                }
    in
    { x = Axis.time model.timezone 1400 "Time" (Tuple.first >> toFloat >> (*) 1000)
    , y = axis
    , container = Container.responsive "votes"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.wider 8
    , dots = Dots.default
    }
