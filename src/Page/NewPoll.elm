module Page.NewPoll exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api
import Api.Polls exposing (Poll)
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


init : Viewer -> ( Model, Cmd Message )
init viewer =
    { viewer = viewer
    , state = NotCreated
    , mode = Create
    , titleInput = ""
    , poll = Nothing
    }
        |> withNoCmd


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
                |> withNoCmd

        --[ Route.replaceUrl
        --    (Session.viewerNavKey model.viewer)
        --    Route.Polls
        --]
        GotUpdateError ->
            { model | state = BadNetwork }
                |> withNoCmd

        GotUpdateSuccess poll ->
            { model
                | poll = Just poll
                , state = Success
            }
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
        , inputTitle <| model.titleInput
        , buttonPollTitle model.mode model.state
        ]
    ]


inputTitle : String -> Html Message
inputTitle content =
    Input.inputWithTitle "Poll title"
        [ onInput WriteNewTitle
        , placeholder "Et tu, Brute?"
        , value content
        ]
        []
        |> withMargin


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
