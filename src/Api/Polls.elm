module Api.Polls exposing
    ( Poll, PollError(..)
    , getAllPolls, delete
    )

{-| A module that provides ways to manipulate and to communicate with the
backend about everything polls


# Types

@docs Poll, PollError


# Endpoints

@docs getAllPolls, delete

-}

import Api exposing (Credentials, authenticated, get, moderatorId, withPath)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import Task exposing (Task)


type PollError
    = GotNotFound
    | GotBadCredentials
    | GotBadNetwork


type alias Poll =
    { idModerator : Int
    , idPoll : Int
    , title : String
    }


{-| A command that will try to request the list of polls existing for a logged
in moderator, and tell what
the issue was if it did not work.
-}
getAllPolls : Credentials -> (List Poll -> a) -> Task PollError a
getAllPolls credentials transform =
    let
        path =
            "mod/" ++ String.fromInt (moderatorId credentials) ++ "/poll"
    in
    get
        { body =
            Json.Encode.null
        , endpoint =
            authenticated credentials
                |> withPath path
        , decoder = pollListDecoder
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


{-| Deletes a provided poll from the backend, and returns the specified value on success.
-}
delete : Credentials -> Poll -> a -> Task PollError a
delete credentials poll return =
    let
        path =
            "mod/" ++ String.fromInt poll.idModerator ++ "/poll/" ++ String.fromInt poll.idPoll
    in
    Api.delete
        { body = Json.Encode.null
        , endpoint = authenticated credentials |> withPath path
        , decoder = Json.Decode.succeed return
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


{-| Create a poll with a specified title, and returns the created poll on success
-}
create : Credentials -> String -> (Poll -> a) -> Task PollError a
create credentials title return =
    let
        path =
            "mod/" ++ String.fromInt (Api.moderatorId credentials) ++ "/poll"
    in
    Api.post
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string title ) ]
        , endpoint = authenticated credentials |> withPath path
        , decoder = pollDecoder
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
        |> Task.map return


pollDecoder : Decoder Poll
pollDecoder =
    Json.Decode.map3 Poll
        (field "idModerator" Json.Decode.int)
        (field "idPoll" Json.Decode.int)
        (field "title" Json.Decode.string)


pollListDecoder : Decoder (List Poll)
pollListDecoder =
    Json.Decode.list <|
        pollDecoder
