module Api.Polls exposing
    ( GetError(..)
    , Poll
    , getPolls
    )

{-| A module that provides ways to manipulate and to communicate with the backend about everything polls

@docs GetError
@docs Poll
@docs getPolls

-}

import Api exposing (Credentials, authenticated, get, moderatorId, withPath)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import Task exposing (Task)


type
    GetError
    -- TODO: is this where this belongs?
    = NotFoundError
    | NetworkError


type alias Poll =
    { idModerator : Int
    , idPoll : Int
    , title : String
    }


{-| A command that will try to request the list of polls existing for a logged in moderator, and tell what
the issue was if it did not work.
-}
getPolls : Credentials -> (List Poll -> a) -> Task GetError a
getPolls credentials transform =
    get
        { body =
            Json.Encode.null
        , endpoint = authenticated credentials |> withPath ("/mod/" ++ String.fromInt (moderatorId credentials) ++ "/poll")
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


pollDecoder : Decoder (List Poll)
pollDecoder =
    Json.Decode.list <|
        Json.Decode.map3 Poll
            (field "idModerator" Json.Decode.int)
            (field "idPoll" Json.Decode.int)
            (field "title" Json.Decode.string)
