module Api exposing
    ( Endpoint, authenticated, withCredentials, withPath
    , Credentials, username
    , login, register, AuthError(..)
    )

{-| A module that provides interactions with the outside world, and in
particular with whatever backend powers the web application.


# Endpoints

It's easy to compose endpoints from the root authenticated endpoint, and not
possible to move outside of the base domain. Enforcing authentication means that
user-composable endpoints logically require a credentials instance, and that
can therefore enforces better defined application state.

@docs Endpoint, authenticated, withCredentials, withPath


# Authentication

It's common to mess things up when performing authentication, and in particular,
it's **very easy** to start doing stupid things inadvertedly (for instance by
logging the authentication credentials or posting them everywhere).

To avoid these problems, access to the value of the authentication tokens is
managed only in this module, and it is not possible to extract the value of the
token and expose it to the outside world !

@docs Credentials, username
@docs login, register, AuthError


# Requests

To perform some requests, you must use one of the different methods that are
offered in this API.

@docs post

-}

import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)
import Url



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
    = NonAuthenticated String
    | Authenticated Credentials String


{-| Returns a root endpoint that is authenticated. By default, all of the
endpoints that can be composed outside of the Api module are authenticated, and
the only unauthenticated endpoint is the login.

This measure ensures that it's not possible to make requests that would be
authenticated without possessing a credentials instance.

-}
authenticated : Credentials -> Endpoint
authenticated credentials =
    root |> withCredentials credentials


{-| Set the credentials for the provided endpoint. The previously set
credentials are replaced if they were previously set.
-}
withCredentials : Credentials -> Endpoint -> Endpoint
withCredentials credentials endpoint =
    case endpoint of
        NonAuthenticated path ->
            Authenticated credentials path

        Authenticated _ path ->
            Authenticated credentials path


{-| Appends a certain path to the root endpoint of the application.
-}
withPath : String -> Endpoint -> Endpoint
withPath path endpoint =
    case endpoint of
        Authenticated credentials url ->
            Authenticated credentials (url ++ path)

        NonAuthenticated url ->
            NonAuthenticated (url ++ path)



-- ENDPOINTS INTERNAL


{-| Returns the root endpoint of the application.
-}
root : Endpoint
root =
    NonAuthenticated "/"


{-| Unwraps the token string to be used for authentication, if it exists for the
provided endpoint.
-}
unwrapToken : Endpoint -> Maybe String
unwrapToken endpoint =
    case endpoint of
        NonAuthenticated _ ->
            Nothing

        Authenticated (Token _ token) _ ->
            Just token


{-| Unwraps the path part of the endpoint. This acts as the resource identifier
for the API.
-}
unwrapPath : Endpoint -> String
unwrapPath endpoint =
    case endpoint of
        NonAuthenticated path ->
            path

        Authenticated _ path ->
            path


{-| Unwraps an endpoint to a specific url, and includes authentication
information.
-}
unwrap : Endpoint -> String
unwrap endpoint =
    { protocol = Url.Https
    , host = "api.rockin.app"
    , port_ = Nothing
    , path = unwrapPath endpoint
    , query = unwrapToken endpoint |> Maybe.map (\p -> "token=" ++ p)
    , fragment = Nothing
    }
        |> Url.toString



-- AUTHENTICATION


{-| A type defining the credentials that will be used to connect to the backend
of the application.
-}
type Credentials
    = Token String String


credentialsDecoder : String -> Json.Decode.Decoder Credentials
credentialsDecoder forName =
    Json.Decode.map2
        Token
        (Json.Decode.succeed forName)
        (Json.Decode.field "token" Json.Decode.string)


{-| Returns the username of some credentials.
-}
username : Credentials -> String
username (Token name _) =
    name



-- AUTHENTICATION


{-| The different types of errors that might arise when one tries to
authenticate into the application.
-}
type AuthError
    = BadCredentials
    | NetworkError


{-| A command that will try to log the user in to the app, and tell what the
issue was if it did not work.

    -- Usage
    login "hello@email.org" "password" identity
        |> Task.attempt
        |> Result.toMaybe

-}
login : String -> String -> (Credentials -> a) -> Task AuthError a
login user pwd transform =
    post
        { body =
            Json.Encode.object
                [ ( "username", Json.Encode.string user )
                , ( "password", Json.Encode.string pwd )
                ]
        , endpoint = root |> withPath "/auth"
        , decoder = credentialsDecoder user
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 403 ->
                        BadCredentials

                    _ ->
                        NetworkError
            )
        |> Task.map transform


{-| A command that will try to register the user in to the app, and tell what
the issue was if it did not work.

    -- Usage
    register "hello@email.org" "password" identity
        |> Task.attempt
        |> Result.toMaybe

-}
register : String -> String -> (Credentials -> a) -> Task AuthError a
register user pwd transform =
    post
        { body =
            Json.Encode.object
                [ ( "username", Json.Encode.string user )
                , ( "password", Json.Encode.string pwd )
                ]
        , endpoint = root |> withPath "/register"
        , decoder = credentialsDecoder user
        }
        |> Task.mapError
            (\error ->
                case error of
                    Http.BadStatus 403 ->
                        BadCredentials

                    _ ->
                        NetworkError
            )
        |> Task.map transform



-- REQUESTS


{-| Exposes the possibility to perform some POST Http requests to the
application. A request always happens on a valid endpoint, with a Json body and
returns a value that can be decoded.
-}
post :
    { body : Json.Encode.Value
    , endpoint : Endpoint
    , decoder : Decoder a
    }
    -> Task Http.Error a
post elements =
    Http.task
        { method = "POST"
        , headers = []
        , url = unwrap elements.endpoint
        , body = Http.jsonBody elements.body
        , resolver =
            Http.stringResolver <|
                handleJsonResponse <|
                    elements.decoder
        , timeout = Nothing
        }


{-| Handles an Http.Response with a certain Decoder, and transforms it into
an Http.Error.
-}
handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result
