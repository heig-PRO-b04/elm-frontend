module Api.Sessions exposing
    ( Emoji(..), ServerSession, ClientSession, SessionStatus(..), SessionError(..)
    , getSession, putSession
    )

{-| A module that provides ways to interact with a poll Session


# Types

@docs Emoji, ServerSession, ClientSession, SessionStatus, SessionError


# Endpoints

@docs getSession, putSession

-}

import Api exposing (Credentials)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import QRCode exposing (QRCode)
import Task exposing (Task)



-- DATA TYPES


type Emoji
    = Emoji0
    | Emoji1
    | Emoji2
    | Emoji3
    | Emoji4
    | Emoji5
    | Emoji6
    | Emoji7
    | Emoji8
    | Emoji9
    | EmojiA
    | EmojiB
    | EmojiC
    | EmojiD
    | EmojiE
    | EmojiF


{-| Transforms a String code into an emoji code.
-}
parseEmojiCode : String -> Maybe (List Emoji)
parseEmojiCode text =
    let
        transform : Char -> Maybe Emoji
        transform char =
            case char of
                '0' ->
                    Just Emoji0

                '1' ->
                    Just Emoji1

                '2' ->
                    Just Emoji2

                '3' ->
                    Just Emoji3

                '4' ->
                    Just Emoji4

                '5' ->
                    Just Emoji5

                '6' ->
                    Just Emoji6

                '7' ->
                    Just Emoji7

                '8' ->
                    Just Emoji8

                '9' ->
                    Just Emoji9

                'A' ->
                    Just EmojiA

                'B' ->
                    Just EmojiB

                'C' ->
                    Just EmojiC

                'D' ->
                    Just EmojiD

                'E' ->
                    Just EmojiE

                'F' ->
                    Just EmojiF

                _ ->
                    Nothing
    in
    if String.startsWith "0x" text == True then
        String.dropLeft 2 text
            |> String.toList
            |> List.map transform
            |> List.foldr (Maybe.map2 (::)) (Just [])

    else
        Nothing


type alias ServerSession =
    { idModerator : Int
    , idPoll : Int
    , idSession : Int
    , code : List Emoji
    , qr : QRCode
    , status : SessionStatus
    }


type alias ClientSession =
    SessionStatus


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
        [ ( "status", encodeStatus session )
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


emojiListDecoder : Decoder (List Emoji)
emojiListDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\chars ->
                case parseEmojiCode chars of
                    Just list ->
                        Json.Decode.succeed list

                    Nothing ->
                        Json.Decode.fail "Malformed code."
            )


qrCodeDecoder : Decoder QRCode
qrCodeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\chars ->
                case QRCode.encode chars of
                    Ok code ->
                        Json.Decode.succeed code

                    Err _ ->
                        Json.Decode.fail "Could not build QRCode"
            )


sessionDecoder : Decoder ServerSession
sessionDecoder =
    Json.Decode.map6
        ServerSession
        (Json.Decode.field "idModerator" Json.Decode.int)
        (Json.Decode.field "idPoll" Json.Decode.int)
        (Json.Decode.field "idSession" Json.Decode.int)
        (Json.Decode.field "code" emojiListDecoder)
        (Json.Decode.field "code" qrCodeDecoder)
        (Json.Decode.field "status" sessionStatusDecoder)
