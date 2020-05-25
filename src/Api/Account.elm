module Api.Account exposing
    ( updateUsername, updatePassword
    , deleteAccount
    , Error(..)
    )

{-| A module that provides ways to modify a moderator account


# Endpoints

@docs updateUsername, updatePassword
@docs deleteAccount


# Types

@docs Error

-}

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


type alias PasswordConfirmation =
    String



-- API


{-| Deletes the moderator account associated with the provided identifier. A password must bes
specified, to ensure the user "approval" of the deletion request.
-}
deleteAccount :
    Credentials
    -> PasswordConfirmation
    -> (String -> a)
    -> Task Error a
deleteAccount credentials passwordConfirmation transform =
    Api.delete
        { body =
            Json.Encode.object
                [ ( "currentPassword", Json.Encode.string passwordConfirmation )
                ]
        , endpoint = moderator credentials
        , decoder = messageDecoder
        }
        |> Task.map transform
        |> Task.mapError handleError


{-| Updates the username of a certain moderator. Invalidates existing session tokens.
-}
updateUsername :
    Credentials
    -> String
    -> PasswordConfirmation
    -> (String -> a)
    -> Task Error a
updateUsername credentials username passwordConfirmation transform =
    Api.put
        { body =
            Json.Encode.object
                [ ( "currentPassword", Json.Encode.string passwordConfirmation )
                , ( "newUsername", Json.Encode.string username )
                ]
        , endpoint =
            moderator credentials
                |> Api.withPath "/username"
        , decoder = messageDecoder
        }
        |> Task.map transform
        |> Task.mapError handleError


{-| Updates the password of a certain moderator. Invalidates existing session tokens.
-}
updatePassword :
    Credentials
    -> String
    -> PasswordConfirmation
    -> (String -> a)
    -> Task Error a
updatePassword credentials password passwordConfirmation transform =
    Api.put
        { body =
            Json.Encode.object
                [ ( "currentPassword", Json.Encode.string passwordConfirmation )
                , ( "newPassword", Json.Encode.string password )
                ]
        , endpoint =
            moderator credentials
                |> Api.withPath "/password"
        , decoder = messageDecoder
        }
        |> Task.map transform
        |> Task.mapError handleError



-- ENDPOINTS


moderator : Credentials -> Api.Endpoint
moderator credentials =
    Api.authenticated credentials
        |> Api.withPath "/mod/"
        |> Api.withPath (String.fromInt <| Api.moderatorId credentials)



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
