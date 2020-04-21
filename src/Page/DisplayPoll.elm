module Page.DisplayPoll exposing
    ( Message
    , Model
    , initCreate
    , initDisplay
    , update
    , view
    )

import Api.Polls exposing (Poll, PollDiscriminator)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Picasso.Button exposing (button, elevated, filled, filledDisabled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Route
import Session exposing (Session, Viewer)
import Task
import Task.Extra


type Message
    = WriteNewTitle String
    | ClickPollTitleButton
    | GotCreateError
    | RequestNavigateToPoll Poll
    | GotNewPoll Poll
    | GotPollDisplayError
    | GotUpdateError


type CreationState
    = NotCreated
    | Pending
    | Success
    | BadNetwork
    | UpdateError


type State
    = CreatingNew
    | LoadingFromExisting
    | Loaded Poll


type alias Model =
    { viewer : Viewer
    , state : CreationState
    , titleInput : String
    , state_ : State
    }


initCreate : Viewer -> ( Model, Cmd Message )
initCreate viewer =
    { viewer = viewer
    , state = NotCreated
    , titleInput = ""
    , state_ = CreatingNew
    }
        |> withNoCmd


initDisplay : Viewer -> PollDiscriminator -> ( Model, Cmd Message )
initDisplay viewer pollDiscriminator =
    { viewer = viewer
    , state = Success
    , titleInput = ""
    , state_ = LoadingFromExisting
    }
        |> withCmd
            [ Api.Polls.getPoll (Session.viewerCredentials viewer) pollDiscriminator GotNewPoll
                |> Task.mapError (always GotPollDisplayError)
                |> Task.Extra.execute
            ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewTitle title ->
            { model | titleInput = title }
                |> withNoCmd

        ClickPollTitleButton ->
            case model.state_ of
                CreatingNew ->
                    { model | state = Pending }
                        |> withCmd
                            [ Api.Polls.create (Session.viewerCredentials model.viewer) model.titleInput RequestNavigateToPoll
                                |> Task.mapError (always GotCreateError)
                                |> Task.Extra.execute
                            ]

                LoadingFromExisting ->
                    model |> withNoCmd

                Loaded poll ->
                    { model | state = Pending }
                        |> withCmd
                            [ Api.Polls.update (Session.viewerCredentials model.viewer) poll model.titleInput GotNewPoll
                                |> Task.mapError (always GotUpdateError)
                                |> Task.Extra.execute
                            ]

        RequestNavigateToPoll poll ->
            model
                |> withCmd
                    [ Route.replaceUrl
                        (Session.viewerNavKey model.viewer)
                        (Route.DisplayPoll (PollDiscriminator poll.idPoll))
                    ]

        GotCreateError ->
            { model | state = BadNetwork }
                |> withNoCmd

        GotNewPoll poll ->
            let
                updated =
                    { model | state_ = Loaded poll, state = Success }
            in
            case model.state_ of
                CreatingNew ->
                    updated |> withCmd [ Cmd.succeed <| RequestNavigateToPoll poll ]

                LoadingFromExisting ->
                    updated |> withNoCmd

                Loaded _ ->
                    updated |> withNoCmd

        GotUpdateError ->
            { model | state = BadNetwork }
                |> withNoCmd

        GotPollDisplayError ->
            { model | state = BadNetwork }
                |> withNoCmd


view : Model -> List (Html Message)
view model =
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
    ]


inputTitle : Model -> Html Message
inputTitle model =
    Input.inputWithTitle ("Poll title: " ++ extractTitle model)
        [ onInput WriteNewTitle
        , placeholder "Et tu, Brute?"
        , value model.titleInput
        ]
        []
        |> withMargin


extractTitle : Model -> String
extractTitle model =
    case model.state_ of
        Loaded poll ->
            poll.title

        _ ->
            ""


buttonPollTitle : CreationState -> Html Message
buttonPollTitle state =
    let
        fillIn =
            if state == Pending then
                filledDisabled

            else
                filled ++ elevated ++ [ onClick ClickPollTitleButton ]

        message =
            case state of
                NotCreated ->
                    "Create poll"

                Pending ->
                    "Loading..."

                Success ->
                    "Poll updated !"

                BadNetwork ->
                    "Network error"

                UpdateError ->
                    "Update error"
    in
    button
        (fillIn ++ [ class "mt-8" ])
        [ text message ]


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]
