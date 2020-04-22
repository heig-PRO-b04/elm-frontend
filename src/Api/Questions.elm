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
import Json.Decode exposing (Decoder, andThen, field)
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
    , answersMin : Int
    , answersMax : Int
    }


type alias ClientQuestion =
    { title : String
    , details : String
    , visibility : QuestionVisibility
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

        endpoint =
            authenticated credentials |> withPath path
    in
    Api.post
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string clientQuestion.title )
                , ( "details", Json.Encode.string clientQuestion.details )
                , ( "visibility", Json.Encode.string (questionVisibilityToString clientQuestion.visibility) )
                , ( "answersMin", Json.Encode.int clientQuestion.answersMin )
                , ( "answersMax", Json.Encode.int clientQuestion.answersMax )
                ]
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
delete credentials question return =
    let
        pathEnd =
            "/" ++ String.fromInt question.idQuestion

        endpoint =
            questionEndpoint pathEnd question credentials
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
update credentials question clientQuestion transform =
    let
        pathEnd =
            "/" ++ String.fromInt question.idQuestion

        endpoint =
            questionEndpoint pathEnd question credentials
    in
    Api.put
        { body =
            Json.Encode.object
                [ ( "title", Json.Encode.string clientQuestion.title )
                , ( "details", Json.Encode.string clientQuestion.details )
                , ( "visibility", Json.Encode.string (questionVisibilityToString clientQuestion.visibility) )
                , ( "answersMin", Json.Encode.int clientQuestion.answersMin )
                , ( "answersMax", Json.Encode.int clientQuestion.answersMax )
                ]
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


questionEndpoint : String -> QuestionDiscriminator -> (Credentials -> Api.Endpoint)
questionEndpoint path discriminator credentials =
    Api.authenticated credentials
        |> Api.withPath "mod/"
        |> Api.withPath (String.fromInt (Api.moderatorId credentials))
        |> Api.withPath "/poll/"
        |> Api.withPath (String.fromInt discriminator.idPoll)
        |> Api.withPath "/question"
        |> Api.withPath path


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


questionDecoder : Decoder ServerQuestion
questionDecoder =
    Json.Decode.map8 ServerQuestion
        (field "idModerator" Json.Decode.int)
        (field "idPoll" Json.Decode.int)
        (field "idQuestion" Json.Decode.int)
        (field "title" Json.Decode.string)
        (field "details" Json.Decode.string)
        (field "visibility" questionVisibilityDecoder)
        (field "answersMin" Json.Decode.int)
        (field "answersMax" Json.Decode.int)


questionListDecoder : Decoder (List ServerQuestion)
questionListDecoder =
    Json.Decode.list <|
        questionDecoder
