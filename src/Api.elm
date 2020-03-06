module Api exposing
    ( Endpoint
    , Credentials, username
    , login, LoginError(..)
    , endpoint
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

import Json.Decode
import Json.Encode
import Process
import Task exposing (Task)
import Url exposing (Url)
import Username exposing (Username)



-- ENDPOINTS


{-| An opaque type representing the endpoints that will be available throughout
the application. The general idea is that an endpoint will necessarily point to
one of the API endpoints that are defined for the app, and that if you obtain
an instance of endpoint, you're guaranteed to point to a location where it's
safe to send your data.

This guarantees that you can't send any credentials to an unexpected endpoint,
and leak the credentials contents by mistake.

-}
type Endpoint
    = FromUrl Url


{-| TODO : Provide more choices for endpoints
-}
endpoint : Endpoint
endpoint =
    FromUrl
        { protocol = Url.Https
        , host = "api.example.org"
        , port_ = Nothing
        , path = "/"
        , query = Nothing
        , fragment = Nothing
        }



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
TODO : Issue a command instead ?
TODO : Provide a better testing Api.

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
                    ( "nonetwork", _ ) ->
                        Task.fail NetworkError

                    ( "username", "password" ) ->
                        case
                            Json.Decode.decodeValue
                                Username.decoder
                                (Json.Encode.string user)
                                |> Result.mapError (always NetworkError)
                                |> Result.map (\u -> Token u "token")
                                |> Result.map transform
                        of
                            Ok v ->
                                Task.succeed v

                            Err e ->
                                Task.fail e

                    ( _, _ ) ->
                        Task.fail BadCredentials
            )



-- REQUESTS
