module Api.Statistics exposing
    ( AnswerStatistics
    , QuestionStatistics
    , StatisticsError(..)
    , getStatisticsForPoll
    )

import Api exposing (Credentials)
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)



-- DATA


type StatisticsError
    = GotNotFound
    | GotBadCredentials
    | GotBadNetwork


type alias AnswerStatistics =
    { title : String
    , positive : Int
    , negative : Int
    }


type alias QuestionStatistics =
    { title : String
    , answers : List AnswerStatistics
    }



-- ENDPOINTS


pollEndpoint : { d | idPoll : Int } -> Credentials -> Api.Endpoint
pollEndpoint discriminator credentials =
    Api.authenticated credentials
        |> Api.withPath "mod/"
        |> Api.withPath (String.fromInt (Api.moderatorId credentials))
        |> Api.withPath "/poll/"
        |> Api.withPath (String.fromInt discriminator.idPoll)
        |> Api.withPath "/statistics"


{-| Returns some statistics for a particular point. The returned data should be displayed with
as little processing as possible, and can't be linked back to the original questions - it rather
consists of aggregated information about the poll votes.
-}
getStatisticsForPoll :
    Credentials
    -> { d | idPoll : Int }
    -> (List QuestionStatistics -> a)
    -> Task StatisticsError a
getStatisticsForPoll credentials discriminator transform =
    let
        endpoint =
            pollEndpoint discriminator credentials
    in
    Api.get
        { body = Json.Encode.null
        , endpoint = endpoint
        , decoder = Json.Decode.list <| questionStatisticsDecoder
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 404 ->
                        GotNotFound

                    Http.BadStatus 403 ->
                        GotBadCredentials

                    _ ->
                        GotBadNetwork
            )
        |> Task.map transform



-- DECODING


answerStatisticsDecoder : Json.Decode.Decoder AnswerStatistics
answerStatisticsDecoder =
    Json.Decode.map3 AnswerStatistics
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "positive" Json.Decode.int)
        (Json.Decode.field "negative" Json.Decode.int)


questionStatisticsDecoder : Json.Decode.Decoder QuestionStatistics
questionStatisticsDecoder =
    Json.Decode.map2 QuestionStatistics
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "answers" <| Json.Decode.list answerStatisticsDecoder)
