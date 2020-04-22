module Page.DisplayPoll.Session exposing
    ( Message
    , Model
    , init
    , subscriptions
    , update
    , view
    )

import Api.Sessions as Api
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Picasso.Button as Picasso
import Session exposing (Viewer)
import Task
import Task.Extra
import Time


type alias Model =
    { viewer : Viewer
    , idPoll : Int
    , session : Maybe Api.ServerSession
    }


type Message
    = ClickStatus Api.SessionStatus
    | GotSession (Maybe Api.ServerSession)
    | RequestSession


init : Viewer -> { p | idPoll : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    { viewer = viewer, idPoll = discriminator.idPoll, session = Nothing }
        |> withCmd [ Cmd.succeed RequestSession ]


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

        GotSession session ->
            { model | session = session } |> withNoCmd

        RequestSession ->
            model
                |> withCmd
                    [ Api.getSession (Session.viewerCredentials model.viewer) identity model
                        -- TODO : Factorize this
                        |> Task.mapError (always <| GotSession Nothing)
                        |> Task.map (\session -> GotSession <| Just session)
                        |> Task.Extra.execute
                    ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 1000 (always <| RequestSession)



-- VIEW


view : Model -> List (Html Message)
view model =
    [ div []
        [ text <| extractTitle model
        , extractEmojiCode model
        , switchMode Api.Open "Open"
        , switchMode Api.Closed "Close"
        , switchMode Api.Quarantined "Close to newcomers"
        ]
    ]


switchMode : Api.SessionStatus -> String -> Html Message
switchMode status contents =
    Picasso.button
        (Picasso.filled ++ [ class "block m-4", onClick (ClickStatus status) ])
        [ text contents ]


extractTitle : Model -> String
extractTitle model =
    case model.session of
        Just session ->
            case session.status of
                Api.Closed ->
                    " Closed"

                Api.Quarantined ->
                    " Quarantined"

                Api.Open ->
                    " Open"

        _ ->
            ""


extractEmojiCode : Model -> Html msg
extractEmojiCode model =
    case model.session of
        Just session ->
            let
                mapper emoji =
                    case emoji of
                        Api.Emoji0 ->
                            "0"

                        Api.Emoji1 ->
                            "1"

                        Api.Emoji2 ->
                            "2"

                        Api.Emoji3 ->
                            "3"

                        Api.Emoji4 ->
                            "4"

                        Api.Emoji5 ->
                            "5"

                        Api.Emoji6 ->
                            "6"

                        Api.Emoji7 ->
                            "7"

                        Api.Emoji8 ->
                            "8"

                        Api.Emoji9 ->
                            "9"

                        Api.EmojiA ->
                            "a"

                        Api.EmojiB ->
                            "b"

                        Api.EmojiC ->
                            "c"

                        Api.EmojiD ->
                            "d"

                        Api.EmojiE ->
                            "e"

                        Api.EmojiF ->
                            "f"
            in
            List.map mapper session.code
                |> List.map (\letter -> "/emoji/emoji_" ++ letter ++ ".png")
                |> List.map (\path -> Html.img [ Html.Attributes.src path, class "w-8 h-8 inline-block" ] [])
                |> div []

        _ ->
            text "NO CODE"
