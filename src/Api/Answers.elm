module Api.Questions exposing
    ( ServerAnswer, ClientAnswer, AnswerDiscriminator, AnswerError(..)
    , getAnswerList, getAnswer, create, update, delete
    )

{-| A module that provides ways to manipulate and to communicate with the
backend about everything polls


# Types

@docs ServerAnswer, ClientAnswer, AnswerDiscriminator, AnswerError


# Endpoints

@docs getAnswerList, getAnswer, create, update, delete

-}

import Api exposing (Credentials)
import Api.Questions exposing (QuestionDiscriminator)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode
import Task exposing (Task)


type AnswerError
    = GotNotFound
    | GotBadCredentials
    | GotBadNetwork


type alias ServerAnswer =
    { idModerator : Int
    , idPoll : Int
    , idQuestion : Int
    , idAnswer : Int
    , title : String
    , description : String
    }


type alias ClientAnswer =
    { title : String
    , description : String
    }


type alias AnswerDiscriminator =
    { idPoll : Int
    , idQuestion : Int
    , idAnswer : Int
    }


{-| A command that will try to request the list of answers existing for a specific question, for a logged
in moderator, and tell what the issue was if it did not work.
-}
getAnswerList : Credentials -> QuestionDiscriminator -> (List ServerAnswer -> a) -> Task AnswerError a
getAnswerList credentials questionDiscriminator transform =
    let
        endpoint =
            genericAnswerEndpoint questionDiscriminator credentials
    in
    Api.get
        { body = Json.Encode.null
        , endpoint = endpoint
        , decoder = answerListDecoder
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


getAnswer : Credentials -> AnswerDiscriminator -> (ServerAnswer -> a) -> Task AnswerError a
getAnswer credentials answerDiscriminator transform =
    let
        endpoint =
            specificAnswerEndpoint answerDiscriminator credentials
    in
    Api.get
        { body =
            Json.Encode.null
        , endpoint = endpoint
        , decoder = answerDecoder
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


{-| Create an answer with a specified title and description for a specific question, and returns the created answer on success
-}
create : Credentials -> QuestionDiscriminator -> ClientAnswer -> (ServerAnswer -> a) -> Task AnswerError a
create credentials questionDiscriminator clientAnswer transform =
    let
        endpoint =
            genericAnswerEndpoint questionDiscriminator credentials
    in
    Api.post
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string clientAnswer.title )
                , ( "description", Json.Encode.string clientAnswer.description )
                ]
        , endpoint = endpoint
        , decoder = answerDecoder
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


{-| Deletes a provided answer for a specific question from the backend, and returns the specified value on success.
-}
delete : Credentials -> AnswerDiscriminator -> a -> Task AnswerError a
delete credentials answerDiscriminator return =
    let
        endpoint =
            specificAnswerEndpoint answerDiscriminator credentials
    in
    Api.delete
        { body = Json.Encode.null
        , endpoint = endpoint
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


{-| Updates an answer for a specific question with the content of a ClientAnswer, and returns the updated answer on success
-}
update : Credentials -> AnswerDiscriminator -> ClientAnswer -> (ServerAnswer -> a) -> Task AnswerError a
update credentials answerDiscriminator clientAnswer transform =
    let
        endpoint =
            specificAnswerEndpoint answerDiscriminator credentials
    in
    Api.put
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string clientAnswer.title )
                , ( "description", Json.Encode.string clientAnswer.description )
                ]
        , endpoint = endpoint
        , decoder = answerDecoder
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


genericAnswerEndpoint : QuestionDiscriminator -> (Credentials -> Api.Endpoint)
genericAnswerEndpoint questionDiscriminator credentials =
    Api.authenticated credentials
        |> Api.withPath "mod/"
        |> Api.withPath (String.fromInt (Api.moderatorId credentials))
        |> Api.withPath "/poll/"
        |> Api.withPath (String.fromInt questionDiscriminator.idPoll)
        |> Api.withPath "/question/"
        |> Api.withPath (String.fromInt questionDiscriminator.idQuestion)
        |> Api.withPath "/answer"


specificAnswerEndpoint : AnswerDiscriminator -> Credentials -> Api.Endpoint
specificAnswerEndpoint answerDiscriminator credentials =
    let
        questionDiscriminator =
            QuestionDiscriminator answerDiscriminator.idPoll answerDiscriminator.idQuestion
    in
    genericAnswerEndpoint questionDiscriminator credentials
        |> Api.withPath ("/" ++ String.fromInt answerDiscriminator.idQuestion)


answerDecoder : Decoder ServerAnswer
answerDecoder =
    Json.Decode.map6 ServerAnswer
        (field "idModerator" Json.Decode.int)
        (field "idPoll" Json.Decode.int)
        (field "idQuestion" Json.Decode.int)
        (field "idAnswer" Json.Decode.int)
        (field "title" Json.Decode.string)
        (field "description" Json.Decode.string)


answerListDecoder : Decoder (List ServerAnswer)
answerListDecoder =
    Json.Decode.list <|
        answerDecoder
