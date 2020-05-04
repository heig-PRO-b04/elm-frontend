module Api.Questions exposing
    ( ServerQuestion, ClientQuestion, QuestionDiscriminator, QuestionVisibility(..), QuestionError(..)
    , getQuestionList, getQuestion, create, update, delete
    )

{-| A module that provides ways to manipulate and to communicate with the
backend about everything polls


# Types

@docs ServerQuestion, ClientQuestion, QuestionDiscriminator, QuestionVisibility, QuestionError


# Endpoints

@docs getQuestionList, getQuestion, create, update, delete

-}

import Api exposing (Credentials)
import Api.Polls exposing (PollDiscriminator)
import Http
import Json.Decode exposing (Decoder, field)
import Json.Decode.Extra
import Json.Encode
import Task exposing (Task)


type QuestionError
    = GotNotFound
    | GotBadCredentials
    | GotBadNetwork


type QuestionVisibility
    = Hidden
    | Archived
    | Visible


type alias ServerQuestion =
    { idModerator : Int
    , idPoll : Int
    , idQuestion : Int
    , title : String
    , details : String
    , visibility : QuestionVisibility
    , index : Float
    , answersMin : Int
    , answersMax : Int
    }


type alias ClientQuestion =
    { title : String
    , details : String
    , visibility : QuestionVisibility
    , index : Float
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
        endpoint =
            anyQuestion pollDiscriminator credentials
    in
    Api.get
        { body = Json.Encode.null
        , endpoint = endpoint
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


getQuestion : Credentials -> QuestionDiscriminator -> (ServerQuestion -> a) -> Task QuestionError a
getQuestion credentials questionDiscriminator transform =
    let
        endpoint =
            someQuestion questionDiscriminator credentials
    in
    Api.get
        { body =
            Json.Encode.null
        , endpoint = endpoint
        , decoder = questionDecoder
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


{-| Create a question with a specified title for a specific poll, and returns the created question on success
-}
create : Credentials -> PollDiscriminator -> ClientQuestion -> (ServerQuestion -> a) -> Task QuestionError a
create credentials pollDiscriminator clientQuestion transform =
    let
        endpoint =
            anyQuestion pollDiscriminator credentials
    in
    Api.post
        { body = questionEncode clientQuestion
        , endpoint = endpoint
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
delete credentials questionDiscriminator return =
    let
        endpoint =
            someQuestion questionDiscriminator credentials
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


{-| Updates a question for a specific poll with the content of a ClientQuestion, and returns the updated question on success
-}
update : Credentials -> QuestionDiscriminator -> ClientQuestion -> (ServerQuestion -> a) -> Task QuestionError a
update credentials questionDiscriminator clientQuestion transform =
    let
        endpoint =
            someQuestion questionDiscriminator credentials
    in
    Api.put
        { body = questionEncode clientQuestion
        , endpoint = endpoint
        , decoder = questionDecoder
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


anyQuestion : PollDiscriminator -> Credentials -> Api.Endpoint
anyQuestion pollDiscriminator credentials =
    Api.authenticated credentials
        |> Api.withPath "mod/"
        |> Api.withPath (String.fromInt (Api.moderatorId credentials))
        |> Api.withPath "/poll/"
        |> Api.withPath (String.fromInt pollDiscriminator.idPoll)
        |> Api.withPath "/question"


someQuestion : QuestionDiscriminator -> Credentials -> Api.Endpoint
someQuestion questionDiscriminator credentials =
    let
        pollDiscriminator =
            PollDiscriminator questionDiscriminator.idPoll
    in
    anyQuestion pollDiscriminator credentials
        |> Api.withPath ("/" ++ String.fromInt questionDiscriminator.idQuestion)


questionVisibilityToString : QuestionVisibility -> String
questionVisibilityToString questionVisibility =
    case questionVisibility of
        Hidden ->
            "hidden"

        Archived ->
            "archived"

        Visible ->
            "visible"


questionVisibilityDecoder : Decoder QuestionVisibility
questionVisibilityDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "hidden" ->
                        Json.Decode.succeed Hidden

                    "archived" ->
                        Json.Decode.succeed Archived

                    "visible" ->
                        Json.Decode.succeed Visible

                    error ->
                        Json.Decode.fail <| "Unknown visibility: " ++ error
            )


questionEncode : ClientQuestion -> Json.Encode.Value
questionEncode clientQuestion =
    Json.Encode.object
        [ ( "title", Json.Encode.string clientQuestion.title )
        , ( "details", Json.Encode.string clientQuestion.details )
        , ( "visibility", Json.Encode.string (questionVisibilityToString clientQuestion.visibility) )
        , ( "indexInPoll", Json.Encode.float clientQuestion.index )
        , ( "answersMin", Json.Encode.int clientQuestion.answersMin )
        , ( "answersMax", Json.Encode.int clientQuestion.answersMax )
        ]


questionDecoder : Decoder ServerQuestion
questionDecoder =
    Json.Decode.Extra.map9 ServerQuestion
        (field "idModerator" Json.Decode.int)
        (field "idPoll" Json.Decode.int)
        (field "idQuestion" Json.Decode.int)
        (field "title" Json.Decode.string)
        (field "details" Json.Decode.string)
        (field "visibility" questionVisibilityDecoder)
        (field "indexInPoll" Json.Decode.float)
        (field "answerMin" Json.Decode.int)
        (field "answerMax" Json.Decode.int)


questionListDecoder : Decoder (List ServerQuestion)
questionListDecoder =
    Json.Decode.list <|
        questionDecoder
