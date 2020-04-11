module Api.Polls exposing (..)

{-| A command that will try to register the user in to the app, and tell what
the issue was if it did not work.

-- Usage
TODO

-}

import Api exposing (Credentials, authenticated, get, withPath)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import Task exposing (Task)


getPolls : Credentials -> (Poll -> a) -> Task GetError a
getPolls credentials transform =
    get
        { body =
            Json.Encode.object
                []
        , endpoint = authenticated credentials |> withPath "/mod/{idModerator}/poll"
        , decoder = pollDecoder
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 404 ->
                        NotFoundError

                    _ ->
                        GetNetworkError
            )
        |> Task.map transform


type GetError
    = NotFoundError
    | GetNetworkError


type alias Poll =
    { idModerator : Int
    , idPoll : Int
    , title : String
    }


type alias PollList =
    { polls : List Poll
    }


pollDecoder : Decoder Poll
pollDecoder =
    Json.Decode.map3 Poll
        (field "idModerator" Json.Decode.int)
        (field "idPoll" Json.Decode.int)
        (field "title" Json.Decode.string)
