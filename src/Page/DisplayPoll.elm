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



-- MODEL


type PollError
    = DisplayError
    | UpdateError
    | CreateError


type State
    = CreatingNew
    | LoadingFromExisting
    | Loaded Poll
    | Error PollError


type alias Model =
    { viewer : Viewer
    , titleInput : String
    , state : State
    }


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


type Message
    = WriteNewTitle String
    | ClickPollTitleButton
    | RequestNavigateToPoll Poll
    | GotNewPoll Poll
    | GotError PollError


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
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

                Loaded poll ->
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
            { model | state = Error error }
                |> withNoCmd

        GotNewPoll poll ->
            let
                updated =
                    { model | state = Loaded poll }
            in
            case model.state of
                CreatingNew ->
                    updated |> withCmd [ Cmd.succeed <| RequestNavigateToPoll poll ]

                _ ->
                    updated |> withNoCmd



-- VIEW


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
    case model.state of
        Loaded poll ->
            poll.title

        _ ->
            ""


buttonPollTitle : State -> Html Message
buttonPollTitle state =
    let
        message =
            case state of
                Loaded poll ->
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
