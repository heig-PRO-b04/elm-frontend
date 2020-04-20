module Page.DisplayPoll exposing
    ( Message
    , Model
    , initCreate
    , initDisplay
    , update
    , view
    )

import Api
import Api.Polls exposing (Poll, PollIdentifier)
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
    | GotCreateSuccess Poll
    | GotPollDisplaySuccess Poll
    | GotPollDisplayError
    | GotUpdateError
    | GotUpdateSuccess Poll


type CreationState
    = NotCreated
    | Pending
    | Success
    | BadNetwork
    | UpdateError


type PollMode
    = Create
    | Update


type alias Model =
    { viewer : Viewer
    , state : CreationState
    , mode : PollMode
    , titleInput : String
    , poll : Maybe Poll
    }


initCreate : Viewer -> ( Model, Cmd Message )
initCreate viewer =
    { viewer = viewer
    , state = NotCreated
    , mode = Create
    , titleInput = ""
    , poll = Nothing
    }
        |> withNoCmd


initDisplay : Viewer -> PollIdentifier -> ( Model, Cmd Message )
initDisplay viewer pollIdentifier =
    { viewer = viewer
    , state = Success
    , mode = Update
    , titleInput = ""
    , poll = Nothing
    }
        |> withCmd
            [ Api.Polls.getPoll (Session.viewerCredentials viewer) pollIdentifier GotPollDisplaySuccess
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
            case model.mode of
                Create ->
                    { model | state = Pending }
                        |> withCmd
                            [ Api.Polls.create (Session.viewerCredentials model.viewer) model.titleInput GotCreateSuccess
                                |> Task.mapError (always GotCreateError)
                                |> Task.Extra.execute
                            ]

                Update ->
                    case model.poll of
                        Just poll ->
                            { model | state = Pending }
                                |> withCmd
                                    [ Api.Polls.update (Session.viewerCredentials model.viewer) poll model.titleInput GotUpdateSuccess
                                        |> Task.mapError (always GotUpdateError)
                                        |> Task.Extra.execute
                                    ]

                        Nothing ->
                            { model | state = UpdateError }
                                |> withNoCmd

        GotCreateError ->
            { model
                | poll = Nothing
                , state = BadNetwork
            }
                |> withNoCmd

        GotCreateSuccess poll ->
            { model
                | poll = Just poll
                , state = Success
                , mode = Update
            }
                |> withCmd
                    [ Route.replaceUrl
                        (Session.viewerNavKey model.viewer)
                        (Route.DisplayPoll (PollIdentifier poll.idPoll))
                    ]

        GotUpdateError ->
            { model | state = BadNetwork }
                |> withNoCmd

        GotUpdateSuccess poll ->
            { model
                | poll = Just poll
                , state = Success
            }
                |> withNoCmd

        GotPollDisplaySuccess poll ->
            { model
                | poll = Just poll
                , state = Success
            }
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
        , buttonPollTitle model.mode model.state
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
    case model.poll of
        Just poll ->
            poll.title

        Nothing ->
            ""


buttonPollTitle : PollMode -> CreationState -> Html Message
buttonPollTitle mode state =
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
                    case mode of
                        Create ->
                            "Poll created !"

                        Update ->
                            "Title changed !"

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
