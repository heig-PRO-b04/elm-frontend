module Api exposing
    ( Endpoint, rootEndpoint
    , Credentials, username
    , login, LoginError(..)
    )

{-| A module that provides interactions with the outside world, and in
particular with whatever backend powers the web application.

TODO : Define if we want to let users post arbitrary content to the API or not.


# Endpoints

It's easy to compose endpoints from the root endpoint, and not possible to
move outside of the base domain.

@docs Endpoint, rootEndpoint


# Authentication

It's common to mess things up when performing authentication, and in particular,
it's **very easy** to start doing stupid things inadvertedly (for instance by
logging the authentication credentials or posting them everywhere).

To avoid these problems, access to the value of the authentication tokens is
managed only in this module, and it is not possible to extract the value of the
token and expose it to the outside world !

@docs Credentials, username
@docs login, LoginError

-}

import Http
import Json.Decode
import Json.Encode
import Process
import Task exposing (Task)
import Username exposing (Username)



-- ENDPOINTS


{-| TODO : Use some Urls directly instead =
-}
type Endpoint
    = FromUrl String


{-| TODO : Provide more choices for endpoints ?
-}
rootEndpoint : Endpoint
rootEndpoint =
    FromUrl "https://api.example.org"



-- AUTHENTICATION


{-| A type defining the credentials that will be used to connect to the backend
of the application.
-}
type Credentials
    = Token Username String


{-| Returns the username of some credentials.
-}
username : Credentials -> Username
username (Token name _) =
    name



-- LOGIN


{-| The different types of errors that might arise when one tries to login into
the application.
-}
type LoginError
    = BadCredentials
    | NetworkError


{-| A command that will try to log the user in to the app, and tell what the
issue was if it did not work.

TODO : Connect to an actual endpoint.

    -- Usage
    login "hello@email.org" "password" identity
        |> Task.attempt
        |> Result.toMaybe

-}
login : String -> String -> (Credentials -> a) -> Task LoginError a
login user pwd transform =
    Process.sleep 1000
        |> Task.andThen
            (\_ ->
                case ( user, pwd ) of
                    ( "hello@email.org", "password" ) ->
                        case
                            Json.Decode.decodeValue
                                Username.decoder
                                (Json.Encode.string "hello@email.org")
                                |> Result.mapError (always BadCredentials)
                                |> Result.map transform
                        of
                            Ok v ->
                                Task.succeed v

                            Err e ->
                                Task.fail e

                    _ ->
                        Task.fail NetworkError
            )



-- REQUESTS


request :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , resolver : Http.Resolver x a
    , timeout : Maybe Float
    }
    -> Task a x
request =
    Debug.todo "Not implemented yet."