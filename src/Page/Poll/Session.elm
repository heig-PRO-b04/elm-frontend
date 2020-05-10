module Page.Poll.Session exposing
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
import Html.Attributes as Attribute
import Html.Events exposing (onClick)
import List.Extra
import Page.Poll.Session.Emoji as Emoji
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
    let
        toCmd : Task.Task error Api.ServerSession -> Cmd Message
        toCmd task =
            Task.mapError (always <| GotSession Nothing) task
                |> Task.map (\session -> GotSession <| Just session)
                |> Task.Extra.execute
    in
    case msg of
        ClickStatus status ->
            model
                |> withCmd
                    [ Api.putSession (Session.viewerCredentials model.viewer) { status = status } model identity
                        |> toCmd
                    ]

        RequestSession ->
            model
                |> withCmd
                    [ Api.getSession (Session.viewerCredentials model.viewer) identity model
                        |> toCmd
                    ]

        GotSession session ->
            { model | session = session } |> withNoCmd


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 1000 (always <| RequestSession)



-- VIEW


{-| Displays the emoji code as an adaptive grid. If the items do not fix on a horizontal line, they
will be displayed on two lines, as a 2x2 grid.
-}
code : List Api.Emoji -> Html msg
code emojis =
    let
        grouped =
            List.Extra.grouped 2 emojis
                |> List.map (List.map (Emoji.img [ Attribute.class "inline-block m-2 bg-seaside-050" ] []))
                |> List.map (div [])
    in
    div [ Attribute.class "flex flex-wrap justify-center flex-row" ] grouped



-- MODERATOR VIEW


moderatorView : Model -> List (Html Message)
moderatorView model =
    case model.session of
        Nothing ->
            moderatorClosedView

        Just value ->
            case value.status of
                Api.Closed ->
                    moderatorClosedView

                Api.Open ->
                    moderatorOpenView value

                Api.Quarantined ->
                    moderatorQuarantinedView


moderatorClosedView : List (Html Message)
moderatorClosedView =
    [ Html.div [ Attribute.class "w-full rounded-lg text-gray-500 font-archivo font-normal mt-8" ]
        [ Html.div [ Attribute.class "w-full mx-auto" ]
            [ Html.p
                []
                [ Html.text "Your poll is currently closed. This means that participants will not be able to access it and gives you some time to prepare questions. "
                , Html.span [ Attribute.class "block mt-4" ] [ Html.text "Once open, the poll will become available with a QR code or an emoji code !" ]
                , Html.span
                    [ Attribute.class "mt-4 mb-2 text-center block font-semibold text-seaside-600 hover:underline cursor-pointer"
                    , onClick <| ClickStatus Api.Open
                    ]
                    [ Html.text "Open" ]
                ]
            ]
        ]
    ]


moderatorOpenView : Api.ServerSession -> List (Html Message)
moderatorOpenView session =
    [ Html.div [ Attribute.class "w-full font-archivo font-normal mt-8" ]
        [ Html.div [ Attribute.class "w-full mx-auto" ]
            [ Html.p
                [ Attribute.class "text-gray-500"
                ]
                [ Html.text "Your poll is open and live ! Announce the emoji code to your participants to let them join, or "
                , Html.a
                    [ Attribute.class "font-semibold underline cursor-pointer text-seaside-500"
                    , Route.href <| Route.LivePoll { idPoll = session.idPoll }
                    , Attribute.target "_blank"
                    ]
                    [ Html.text "open up the participant view in a new window." ]
                , Html.div [ Attribute.class "mt-4 w-full" ] [ code session.code ]
                , Html.div [ Attribute.class "mt-4 mb-2 flex flex-row justify-center items-center" ]
                    [ Html.span
                        [ Attribute.class "text-yellow-500 text-center font-semibold hover:underline cursor-pointer mr-4"
                        , onClick <| ClickStatus Api.Quarantined
                        ]
                        [ Html.text "Close to newcomers" ]
                    , Html.span
                        [ Attribute.class "text-center block font-semibold hover:underline cursor-pointer ml-4"
                        , Attribute.class "text-red-600"
                        , onClick <| ClickStatus Api.Closed
                        ]
                        [ Html.text "Close" ]
                    ]
                ]
            ]
        ]
    ]


moderatorQuarantinedView : List (Html Message)
moderatorQuarantinedView =
    [ Html.div [ Attribute.class "w-full text-gray-500 font-archivo font-normal mt-8" ]
        [ Html.div [ Attribute.class "w-full md:w-1/2 mx-auto" ]
            [ Html.p
                []
                [ Html.text "Your poll is closed to newcomers. You need to close it before you can accept new participants !"
                , Html.span [ Attribute.class "block mt-4" ] [ Html.text "The current emoji and QR codes have been revoked. Closing the poll will kick out the current participants." ]
                , Html.span
                    [ Attribute.class "mt-4 mb-2 text-center text-red-600 block font-semibold hover:underline cursor-pointer"
                    , onClick <| ClickStatus Api.Closed
                    ]
                    [ Html.text "Close" ]
                ]
            ]
        ]
    ]



-- PARTICIPANT VIEW


participantView : Model -> List (Html Message)
participantView model =
    [ div
        [ Attribute.class "bg-white shadow m-0 mx-auto w-full md:w-1/2 md:max-w-lg"
        , Attribute.class "md:rounded-lg"
        , Attribute.class "flex flex-col items-center"
        , Attribute.class "p-8"
        , Attribute.class "mt-4"
        ]
        [ viewTitle [ Attribute.class "self-start" ] model
        , extractQrCode model |> Maybe.withDefault (div [] [])
        , extractEmojiCode model |> Maybe.withDefault (div [] [])
        ]
    ]


viewTitle : List (Html.Attribute msg) -> Model -> Html msg
viewTitle attrs model =
    let
        state =
            model.session
                |> Maybe.map .status
                |> Maybe.withDefault Api.Closed

        spanAttrs =
            [ Attribute.class "font-archivo text-2xl font-semibold" ]

        span =
            case state of
                Api.Closed ->
                    Html.span ([ Attribute.class "text-red-500" ] ++ spanAttrs) [ text "closed ⛔️" ]

                Api.Quarantined ->
                    Html.span ([ Attribute.class "text-orange-500" ] ++ spanAttrs) [ text "closed to newcomers ⚠️" ]

                Api.Open ->
                    Html.span ([ Attribute.class "text-green-500" ] ++ spanAttrs) [ text "open ✅" ]
    in
    div attrs
        [ Html.span spanAttrs [ text "This poll is " ]
        , span
        ]


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
                    Just <| code session.code

                _ ->
                    Nothing

        Nothing ->
            Nothing
