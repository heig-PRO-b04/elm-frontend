module Page.DisplayPoll.Session exposing
    ( Message
    , Model
    , init
    , moderatorView
    , participantView
    , subscriptions
    , update
    )

import Api.Sessions as Api
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, target)
import Html.Events exposing (onClick)
import Page.DisplayPoll.Session.Emoji as Emoji
import Picasso.Button as Picasso
import QRCode
import Route
import Session exposing (Viewer)
import Task
import Task.Extra
import Time



-- MODEL


type alias Model =
    { viewer : Viewer
    , idPoll : Int
    , session : Maybe Api.ServerSession
    }


init : Viewer -> { p | idPoll : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    { viewer = viewer, idPoll = discriminator.idPoll, session = Nothing }
        |> withCmd [ Cmd.succeed RequestSession ]



-- UPDATE


type Message
    = ClickStatus Api.SessionStatus
    | GotSession (Maybe Api.ServerSession)
    | RequestSession


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        ClickStatus status ->
            model
                |> withCmd
                    [ Api.putSession (Session.viewerCredentials model.viewer) { status = status } model identity
                        -- TODO : Factorize this
                        |> Task.mapError (always <| GotSession Nothing)
                        |> Task.map (\session -> GotSession <| Just session)
                        |> Task.Extra.execute
                    ]

        RequestSession ->
            model
                |> withCmd
                    [ Api.getSession (Session.viewerCredentials model.viewer) identity model
                        -- TODO : Factorize this
                        |> Task.mapError (always <| GotSession Nothing)
                        |> Task.map (\session -> GotSession <| Just session)
                        |> Task.Extra.execute
                    ]

        GotSession session ->
            { model | session = session } |> withNoCmd


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 1000 (always <| RequestSession)



-- VIEW


participantView : Model -> List (Html Message)
participantView model =
    [ div
        [ class "bg-white shadow m-0 mx-auto w-full md:w-1/2 md:max-w-lg"
        , class "md:rounded-lg"
        , class "flex flex-col items-center"
        , class "p-8"
        , class "mt-4"
        ]
        [ viewTitle [ class "self-start" ] model
        , extractQrCode model |> Maybe.withDefault (div [] [])
        , extractEmojiCode model |> Maybe.withDefault (div [] [])
        ]
    ]


moderatorView : Model -> List (Html Message)
moderatorView model =
    [ div
        [ class "bg-white shadow m-0 mx-auto w-full md:w-1/2 md:max-w-lg"
        , class "md:rounded-lg"
        , class "flex flex-col items-center"
        , class "p-8"
        , class "mt-4"
        ]
        [ viewTitle [ class "self-start" ] model
        , extractEmojiCode model |> Maybe.withDefault (div [] [])
        , moderatorViewButtons model
        ]
    ]


moderatorViewButtons : Model -> Html Message
moderatorViewButtons model =
    let
        status =
            Maybe.map .status model.session |> Maybe.withDefault Api.Closed
    in
    case status of
        Api.Closed ->
            div [ class "flex flex-row justify-between" ]
                [ switchMode Api.Open "Open"
                , openParticipantView model
                ]

        Api.Quarantined ->
            div [ class "flex flex-row justify-between" ]
                [ switchMode Api.Closed "Close"
                , openParticipantView model
                ]

        Api.Open ->
            div [ class "flex flex-row justify-between" ]
                [ switchMode Api.Closed "Close"
                , switchMode Api.Quarantined "Close to newcomers"
                , openParticipantView model
                ]


viewTitle : List (Html.Attribute msg) -> Model -> Html msg
viewTitle attrs model =
    let
        state =
            model.session
                |> Maybe.map .status
                |> Maybe.withDefault Api.Closed

        spanAttrs =
            [ class "font-archivo text-2xl font-semibold" ]

        span =
            case state of
                Api.Closed ->
                    Html.span ([ class "text-red-500" ] ++ spanAttrs) [ text "closed ⛔️" ]

                Api.Quarantined ->
                    Html.span ([ class "text-orange-500" ] ++ spanAttrs) [ text "closed to newcomers ⚠️" ]

                Api.Open ->
                    Html.span ([ class "text-green-500" ] ++ spanAttrs) [ text "open ✅" ]
    in
    div attrs
        [ Html.span spanAttrs [ text "This poll is " ]
        , span
        ]


switchMode : Api.SessionStatus -> String -> Html Message
switchMode status contents =
    Picasso.button
        (Picasso.filled ++ [ onClick (ClickStatus status) ])
        [ text contents ]


openParticipantView : Model -> Html Message
openParticipantView model =
    Picasso.a
        (Picasso.filled
            ++ [ Route.href <| Route.LivePoll { idPoll = model.idPoll }
               , target "_blank"
               ]
        )
        [ text "Participants" ]


extractQrCode : Model -> Maybe (Html msg)
extractQrCode model =
    case model.session of
        Just session ->
            case session.status of
                Api.Open ->
                    Just (QRCode.toSvg session.qr)

                _ ->
                    Nothing

        Nothing ->
            Nothing


extractEmojiCode : Model -> Maybe (Html msg)
extractEmojiCode model =
    case model.session of
        Just session ->
            case session.status of
                Api.Open ->
                    List.map (Emoji.img [ class "inline-block mx-2 bg-seaside-050" ] []) session.code
                        |> div []
                        |> Just

                _ ->
                    Nothing

        Nothing ->
            Nothing
