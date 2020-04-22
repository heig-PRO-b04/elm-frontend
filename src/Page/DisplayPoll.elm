module Page.DisplayPoll exposing
    ( Message
    , Model
    , initCreate
    , initDisplay
    , subscriptions
    , update
    , view
    )

import Api.Polls exposing (Poll, PollDiscriminator)
import Api.Sessions
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Page.DisplayPoll.Questions as Questions
import Picasso.Button as Picasso exposing (button, elevated, filled, filledDisabled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Route
import Session exposing (Session, Viewer)
import Task
import Task.Extra
import Time



-- MODEL


type PollError
    = DisplayError
    | UpdateError
    | CreateError


type State
    = CreatingNew
    | LoadingFromExisting
    | Loaded Poll Questions.Model (Maybe Api.Sessions.ServerSession)
    | Error PollError


type alias Model =
    { viewer : Viewer
    , titleInput : String
    , state : State
    }


subscriptions : Model -> Sub Message
subscriptions model =
    case model.state of
        Loaded poll _ _ ->
            Time.every 1000 (always <| RequestSession { idPoll = poll.idPoll })

        _ ->
            Sub.none


initCreate : Viewer -> ( Model, Cmd Message )
initCreate viewer =
    { viewer = viewer
    , titleInput = ""
    , state = CreatingNew
    }
        |> withNoCmd


initDisplay : Viewer -> PollDiscriminator -> ( Model, Cmd Message )
initDisplay viewer pollDiscriminator =
    { viewer = viewer
    , titleInput = ""
    , state = LoadingFromExisting
    }
        |> withCmd
            [ Api.Polls.getPoll (Session.viewerCredentials viewer) pollDiscriminator GotNewPoll
                |> Task.mapError (always <| GotError DisplayError)
                |> Task.Extra.execute
            ]



-- UPDATE


type
    Message
    -- User interface.
    = WriteNewTitle String
    | ClickPollTitleButton
    | ClickSessionStatus Api.Sessions.SessionStatus
      -- Model updates
    | RequestSession PollDiscriminator
    | GotNewPoll Poll
    | GotSessionStatus Api.Sessions.ServerSession
    | GotError PollError
      -- Sub model
    | QuestionMessage Questions.Message
      -- Navigation
    | RequestNavigateToPoll Poll


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        QuestionMessage subMessage ->
            case model.state of
                Loaded poll pollModel session ->
                    let
                        ( updatedModel, cmd ) =
                            Questions.update subMessage pollModel
                    in
                    ( { model | state = Loaded poll updatedModel session }, Cmd.map QuestionMessage cmd )

                _ ->
                    model |> withNoCmd

        RequestSession discriminator ->
            model
                |> withCmd
                    [ Api.Sessions.getSession (Session.viewerCredentials model.viewer) identity discriminator
                        |> Task.mapError (always <| GotError UpdateError)
                        |> Task.map GotSessionStatus
                        |> Task.Extra.execute
                    ]

        GotSessionStatus status ->
            case model.state of
                Loaded poll pollModel _ ->
                    { model | state = Loaded poll pollModel <| Just status } |> withNoCmd

                _ ->
                    model |> withNoCmd

        ClickSessionStatus status ->
            case model.state of
                Loaded poll _ _ ->
                    model
                        |> withCmd
                            [ Api.Sessions.putSession (Session.viewerCredentials model.viewer) { status = status } poll identity
                                |> Task.mapError (always <| GotError UpdateError)
                                |> Task.map GotSessionStatus
                                |> Task.Extra.execute
                            ]

                _ ->
                    model |> withNoCmd

        WriteNewTitle title ->
            { model | titleInput = title }
                |> withNoCmd

        ClickPollTitleButton ->
            case model.state of
                CreatingNew ->
                    model
                        |> withCmd
                            [ Api.Polls.create (Session.viewerCredentials model.viewer) model.titleInput RequestNavigateToPoll
                                |> Task.mapError (always <| GotError CreateError)
                                |> Task.Extra.execute
                            ]

                LoadingFromExisting ->
                    model |> withNoCmd

                Loaded poll _ _ ->
                    model
                        |> withCmd
                            [ Api.Polls.update (Session.viewerCredentials model.viewer) poll model.titleInput GotNewPoll
                                |> Task.mapError (always <| GotError UpdateError)
                                |> Task.Extra.execute
                            ]

                Error _ ->
                    model |> withNoCmd

        RequestNavigateToPoll poll ->
            model
                |> withCmd
                    [ Route.replaceUrl
                        (Session.viewerNavKey model.viewer)
                        (Route.DisplayPoll (PollDiscriminator poll.idPoll))
                    ]

        GotError error ->
            case model.state of
                Loaded _ _ _ ->
                    model |> withNoCmd

                _ ->
                    { model | state = Error error }
                        |> withNoCmd

        GotNewPoll poll ->
            let
                ( pollModel, cmd ) =
                    Questions.init poll

                updated =
                    { model | state = Loaded poll pollModel Nothing }
            in
            case model.state of
                CreatingNew ->
                    updated
                        |> withCmd
                            [ Cmd.succeed <| RequestNavigateToPoll poll
                            , Cmd.map QuestionMessage cmd
                            ]

                _ ->
                    updated |> withCmd [ Cmd.map QuestionMessage cmd ]



-- VIEW


view : Model -> List (Html Message)
view model =
    let
        appended =
            case model.state of
                Loaded _ pollModel _ ->
                    List.map (Html.map QuestionMessage) (Questions.view pollModel)

                _ ->
                    []
    in
    [ div
        [ class "flex flex-col"
        , class "m-auto my-4 md:my-16"

        -- Card appearance
        , class "bg-white"
        , class "shadow"
        , class "p-8"
        , class "md:rounded-lg"
        , class "md:w-1/2"
        , class "md:max-w-lg"
        ]
        [ styledH2 "Create a new poll"
        , inputTitle <| model
        , buttonPollTitle model.state
        ]
    , div []
        [ switchMode Api.Sessions.Open "Open"
        , switchMode Api.Sessions.Closed "Close"
        , switchMode Api.Sessions.Quarantined "Close to newcomers"
        ]
    ]
        ++ appended


switchMode : Api.Sessions.SessionStatus -> String -> Html Message
switchMode status contents =
    Picasso.button
        (Picasso.filled ++ [ class "block m-4", onClick (ClickSessionStatus status) ])
        [ text contents ]


inputTitle : Model -> Html Message
inputTitle model =
    div []
        [ Input.inputWithTitle ("Poll title: " ++ extractTitle model)
            [ onInput WriteNewTitle
            , placeholder "Et tu, Brute?"
            , value model.titleInput
            ]
            []
            |> withMargin
        , extractEmojiCode model
        ]


extractEmojiCode : Model -> Html msg
extractEmojiCode model =
    case model.state of
        Loaded _ _ (Just status) ->
            let
                mapper emoji =
                    case emoji of
                        Api.Sessions.Emoji0 ->
                            "0"

                        Api.Sessions.Emoji1 ->
                            "1"

                        Api.Sessions.Emoji2 ->
                            "2"

                        Api.Sessions.Emoji3 ->
                            "3"

                        Api.Sessions.Emoji4 ->
                            "4"

                        Api.Sessions.Emoji5 ->
                            "5"

                        Api.Sessions.Emoji6 ->
                            "6"

                        Api.Sessions.Emoji7 ->
                            "7"

                        Api.Sessions.Emoji8 ->
                            "8"

                        Api.Sessions.Emoji9 ->
                            "9"

                        Api.Sessions.EmojiA ->
                            "a"

                        Api.Sessions.EmojiB ->
                            "b"

                        Api.Sessions.EmojiC ->
                            "c"

                        Api.Sessions.EmojiD ->
                            "d"

                        Api.Sessions.EmojiE ->
                            "e"

                        Api.Sessions.EmojiF ->
                            "f"
            in
            List.map mapper status.code
                |> List.map (\letter -> "/emoji/emoji_" ++ letter ++ ".png")
                |> List.map (\path -> Html.img [ Html.Attributes.src path, class "w-8 h-8 inline-block" ] [])
                |> div []

        _ ->
            text "NO CODE"


extractTitle : Model -> String
extractTitle model =
    case model.state of
        Loaded poll _ status ->
            poll.title
                ++ (case Maybe.map .status status of
                        Just Api.Sessions.Closed ->
                            " Closed"

                        Just Api.Sessions.Quarantined ->
                            " Quarantined"

                        Just Api.Sessions.Open ->
                            " Open"

                        Nothing ->
                            " Not set"
                   )

        _ ->
            ""


buttonPollTitle : State -> Html Message
buttonPollTitle state =
    let
        message =
            case state of
                Loaded poll _ _ ->
                    "Update"

                LoadingFromExisting ->
                    "Loading"

                CreatingNew ->
                    "Create"

                Error error ->
                    "Update error"
    in
    button
        (filled ++ elevated ++ [ onClick ClickPollTitleButton, class "mt-8" ])
        [ text message ]


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]
