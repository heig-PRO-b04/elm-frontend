module Api.Sessions exposing
    ( ClientSession
    , ServerSession
    , SessionError(..)
    , SessionStatus(..)
    , getSession
    , putSession
    )

import Api exposing (Credentials)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)



-- DATA TYPES


type alias ServerSession =
    { idModerator : Int
    , idPoll : Int
    , idSession : Int
    , code : String
    , status : SessionStatus
    }


type alias ClientSession =
    { status : SessionStatus }


type SessionStatus
    = Open
    | Closed
    | Quarantined


type SessionError
    = GotNotFound
    | GotBadCredentials
    | GotBadNetwork



-- ENDPOINTS


sessionEndpoint :
    { d | idPoll : Int }
    -> (Credentials -> Api.Endpoint)
sessionEndpoint discriminator credentials =
    Api.authenticated credentials
        |> Api.withPath "mod/"
        |> Api.withPath (String.fromInt (Api.moderatorId credentials))
        |> Api.withPath "/poll/"
        |> Api.withPath (String.fromInt discriminator.idPoll)
        |> Api.withPath "/session"



-- EXPOSED API


getSession :
    Credentials
    -> (ServerSession -> a)
    -> { d | idPoll : Int }
    -> Task SessionError a
getSession credentials transform discriminator =
    Api.get
        { body = Json.Encode.null
        , endpoint = sessionEndpoint discriminator credentials
        , decoder = sessionDecoder
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


putSession :
    Credentials
    -> ClientSession
    -> { d | idPoll : Int }
    -> (ServerSession -> a)
    -> Task SessionError a
putSession credentials session discriminator transform =
    Api.put
        { body = encodeSession session
        , endpoint = sessionEndpoint discriminator credentials
        , decoder = sessionDecoder
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



-- JSON ENCODING


encodeStatus : SessionStatus -> Json.Encode.Value
encodeStatus status =
    case status of
        Open ->
            Json.Encode.string "open"

        Closed ->
            Json.Encode.string "closed"

        Quarantined ->
            Json.Encode.string "quarantined"


encodeSession : ClientSession -> Json.Encode.Value
encodeSession session =
    Json.Encode.object
        [ ( "status", encodeStatus session.status )
        ]



-- JSON DECODING


sessionStatusDecoder : Decoder SessionStatus
sessionStatusDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\contents ->
                case contents of
                    "open" ->
                        Json.Decode.succeed Open

                    "closed" ->
                        Json.Decode.succeed Closed

                    "quarantined" ->
                        Json.Decode.succeed Quarantined

                    _ ->
                        Json.Decode.fail <| "Invalid session state " ++ contents
            )


sessionDecoder : Decoder ServerSession
sessionDecoder =
    Json.Decode.map5
        ServerSession
        (Json.Decode.field "idModerator" Json.Decode.int)
        (Json.Decode.field "idPoll" Json.Decode.int)
        (Json.Decode.field "idSession" Json.Decode.int)
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "status" sessionStatusDecoder)
