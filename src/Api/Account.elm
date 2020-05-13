module Api.Account exposing
    ( Error(..)
    , ModeratorIdentifier
    , deleteAccount
    , updatePassword
    , updateUsername
    )

import Api exposing (Credentials)
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)



-- DATA TYPES


type Error
    = GotNotFound
    | GotInvalidCredentials
    | GotBadNetwork


type alias ModeratorIdentifier =
    Int


type alias PasswordConfirmation =
    String



-- API


{-| Deletes the moderator account associated with the provided identifier. A password must be
specified, to ensure the user "approval" of the deletion request.
-}
deleteAccount :
    Credentials
    -> ModeratorIdentifier
    -> PasswordConfirmation
    -> (String -> a)
    -> Task Error a
deleteAccount credentials identifier passwordConfirmation transform =
    Api.delete
        { body =
            Json.Encode.object
                [ ( "currentPassword", Json.Encode.string passwordConfirmation )
                ]
        , endpoint = moderator credentials identifier
        , decoder = messageDecoder
        }
        |> Task.map transform
        |> Task.mapError handleError


{-| Updates the username of a certain moderator. Invalidates existing session tokens.
-}
updateUsername :
    Credentials
    -> ModeratorIdentifier
    -> String
    -> PasswordConfirmation
    -> (String -> a)
    -> Task Error a
updateUsername credentials identifier username passwordConfirmation transform =
    Api.put
        { body =
            Json.Encode.object
                [ ( "currentPassword", Json.Encode.string passwordConfirmation )
                , ( "newUsername", Json.Encode.string username )
                ]
        , endpoint =
            moderator credentials identifier
                |> Api.withPath "/username"
        , decoder = messageDecoder
        }
        |> Task.map transform
        |> Task.mapError handleError


{-| Updates the password of a certain moderator. Invalidates existing session tokens.
-}
updatePassword :
    Credentials
    -> ModeratorIdentifier
    -> String
    -> PasswordConfirmation
    -> (String -> a)
    -> Task Error a
updatePassword credentials identifier password passwordConfirmation transform =
    Api.put
        { body =
            Json.Encode.object
                [ ( "currentPassword", Json.Encode.string passwordConfirmation )
                , ( "newPassword", Json.Encode.string password )
                ]
        , endpoint =
            moderator credentials identifier
                |> Api.withPath "/password"
        , decoder = messageDecoder
        }
        |> Task.map transform
        |> Task.mapError handleError



-- ENDPOINTS


moderator : Credentials -> ModeratorIdentifier -> Api.Endpoint
moderator credentials moderatorId =
    Api.authenticated credentials
        |> Api.withPath "/mod/"
        |> Api.withPath (String.fromInt moderatorId)



-- ERROR HANDLING


handleError : Http.Error -> Error
handleError error =
    case error of
        Http.BadStatus 404 ->
            GotNotFound

        Http.BadStatus 403 ->
            GotInvalidCredentials

        _ ->
            GotBadNetwork



-- DECODERS


messageDecoder : Json.Decode.Decoder String
messageDecoder =
    Json.Decode.field "message" <| Json.Decode.string
