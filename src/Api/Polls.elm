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


getPolls : Credentials -> (List Poll -> a) -> Task GetError a
getPolls credentials transform =
    get
        { body =
            Json.Encode.null
        , endpoint = authenticated credentials |> withPath "/mod/{idModerator}/poll"
        , decoder = pollDecoder
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 404 ->
                        NotFoundError

                    _ ->
                        NetworkError
            )
        |> Task.map transform


type GetError
    = NotFoundError
    | NetworkError


type alias Poll =
    { idModerator : Int
    , idPoll : Int
    , title : String
    }


pollDecoder : Decoder (List Poll)
pollDecoder =
    Json.Decode.list <|
        Json.Decode.map3 Poll
            (field "idModerator" Json.Decode.int)
            (field "idPoll" Json.Decode.int)
            (field "title" Json.Decode.string)
