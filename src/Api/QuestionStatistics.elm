module Api.QuestionStatistics exposing
    ( Answer
    , Response
    , StatisticsError(..)
    , Timestamp
    , Vote
    , getStatisticsForQuestion
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


type alias Response =
    { answers : List Answer
    , timestamps : List Timestamp
    }


type alias Answer =
    { idAnswer : Int
    , title : String
    }


type alias Timestamp =
    { seconds : Int
    , votes : List Vote
    }


type alias Vote =
    { idAnswer : Int
    , count : Int
    }



-- ENDPOINTS


questionStatisticsEndpoint : { d | idPoll : Int, idQuestion : Int } -> Credentials -> Api.Endpoint
questionStatisticsEndpoint discriminator credentials =
    Api.authenticated credentials
        |> Api.withPath "mod/"
        |> Api.withPath (String.fromInt (Api.moderatorId credentials))
        |> Api.withPath "/poll/"
        |> Api.withPath (String.fromInt discriminator.idPoll)
        |> Api.withPath "/question/"
        |> Api.withPath (String.fromInt discriminator.idQuestion)
        |> Api.withPath "/statistics"



-- REQUESTS


{-| Returns some statistics for a specific question, for a certain list of timestamps. This gives
snapshot information about the number of votes of the question, and allows for drawing some nice
statistics and reactive graphs that change over time.
-}
getStatisticsForQuestion :
    Credentials
    -> { d | idPoll : Int, idQuestion : Int }
    -> List Int
    -> (Response -> a)
    -> Task StatisticsError a
getStatisticsForQuestion credentials discriminator timestamps transform =
    let
        endpoint =
            questionStatisticsEndpoint discriminator credentials
    in
    Api.post
        { body = Json.Encode.list Json.Encode.int timestamps
        , endpoint = endpoint
        , decoder = responseDecoder
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


responseDecoder : Json.Decode.Decoder Response
responseDecoder =
    Json.Decode.map2 Response
        (Json.Decode.field "answers" <| Json.Decode.list answerDecoder)
        (Json.Decode.field "timestamps" <| Json.Decode.list timestampDecoder)


answerDecoder : Json.Decode.Decoder Answer
answerDecoder =
    Json.Decode.map2 Answer
        (Json.Decode.field "idAnswer" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)


timestampDecoder : Json.Decode.Decoder Timestamp
timestampDecoder =
    Json.Decode.map2 Timestamp
        (Json.Decode.field "seconds" Json.Decode.int)
        (Json.Decode.field "votes" <| Json.Decode.list voteDecoder)


voteDecoder : Json.Decode.Decoder Vote
voteDecoder =
    Json.Decode.map2 Vote
        (Json.Decode.field "idAnswer" Json.Decode.int)
        (Json.Decode.field "count" Json.Decode.int)
