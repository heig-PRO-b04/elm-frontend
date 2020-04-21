module Api.Questions exposing
    ( ServerQuestion, ClientQuestion, QuestionDiscriminator, QuestionError(..)
    , getQuestionList, create, update, delete
    )

{-| A module that provides ways to manipulate and to communicate with the
backend about everything polls


# Types

@docs ServerQuestion, ClientQuestion, QuestionDiscriminator, QuestionError


# Endpoints

@docs getQuestionList, create, update, delete

-}

import Api exposing (Credentials, authenticated, get, moderatorId, withPath)
import Api.Polls exposing (PollDiscriminator)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import Task exposing (Task)


type QuestionError
    = GotNotFound
    | GotBadCredentials
    | GotBadNetwork


type alias ServerQuestion =
    { idModerator : Int
    , idPoll : Int
    , idQuestion : Int
    , title : String
    , details : String
    , visibility : String
    , answersMin : Int
    , answersMax : Int
    }


type alias ClientQuestion =
    { title : String
    , details : String
    , visibility : String
    , answersMin : Int
    , answersMax : Int
    }


type alias QuestionDiscriminator =
    { idPoll : Int
    , idQuestion : Int
    }


{-| A command that will try to request the list of questions existing for a specific poll, for a logged
in moderator, and tell what the issue was if it did not work.
-}
getQuestionList : Credentials -> PollDiscriminator -> (List ServerQuestion -> a) -> Task QuestionError a
getQuestionList credentials pollDiscriminator transform =
    let
        path =
            "mod/" ++ String.fromInt (moderatorId credentials) ++ "/poll/" ++ String.fromInt pollDiscriminator.idPoll ++ "/question"
    in
    get
        { body =
            Json.Encode.null
        , endpoint =
            authenticated credentials
                |> withPath path
        , decoder = questionListDecoder
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


{-| Create a question with a specified title, and returns the created poll on success
-}
create : Credentials -> ClientQuestion -> (ServerQuestion -> a) -> Task QuestionError a
create credentials clientQuestion transform =
    let
        path =
            "mod/" ++ String.fromInt (Api.moderatorId credentials) ++ "/poll"
    in
    Api.post
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string clientQuestion.title )
                , ( "details", Json.Encode.string clientQuestion.details )
                , ( "visibility", Json.Encode.string clientQuestion.visibility )
                , ( "answersMin", Json.Encode.int clientQuestion.answersMin )
                , ( "answersMax", Json.Encode.int clientQuestion.answersMax )
                ]
        , endpoint = authenticated credentials |> withPath path
        , decoder = questionDecoder
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 403 ->
                        GotBadCredentials

                    _ ->
                        GotBadNetwork
            )
        |> Task.map transform


{-| Deletes a provided question for a specific poll from the backend, and returns the specified value on success.
-}
delete : Credentials -> QuestionDiscriminator -> a -> Task QuestionError a
delete credentials question return =
    let
        path =
            "mod/" ++ String.fromInt (Api.moderatorId credentials) ++ "/poll/" ++ String.fromInt question.idPoll ++ "/question/" ++ String.fromInt question.idQuestion
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


{-| Updates a poll with a specified title, and returns the created poll on success
-}
update : Credentials -> QuestionDiscriminator -> ClientQuestion -> (ServerQuestion -> a) -> Task QuestionError a
update credentials questionDiscriminator clientQuestion transform =
    let
        path =
            "mod/" ++ String.fromInt (Api.moderatorId credentials) ++ "/poll/" ++ String.fromInt questionDiscriminator.idPoll
    in
    Api.put
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string clientQuestion.title )
                , ( "details", Json.Encode.string clientQuestion.details )
                , ( "visibility", Json.Encode.string clientQuestion.visibility )
                , ( "answersMin", Json.Encode.int clientQuestion.answersMin )
                , ( "answersMax", Json.Encode.int clientQuestion.answersMax )
                ]
        , endpoint = authenticated credentials |> withPath path
        , decoder = questionDecoder
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 403 ->
                        GotBadCredentials

                    _ ->
                        GotBadNetwork
            )
        |> Task.map transform


questionDecoder : Decoder ServerQuestion
questionDecoder =
    Json.Decode.map8 ServerQuestion
        (field "idModerator" Json.Decode.int)
        (field "idPoll" Json.Decode.int)
        (field "idQuestion" Json.Decode.int)
        (field "title" Json.Decode.string)
        (field "details" Json.Decode.string)
        (field "visibility" Json.Decode.string)
        (field "answersMin" Json.Decode.int)
        (field "answersMax" Json.Decode.int)


questionListDecoder : Decoder (List ServerQuestion)
questionListDecoder =
    Json.Decode.list <|
        questionDecoder
