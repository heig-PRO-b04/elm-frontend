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
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Page.DisplayPoll.Questions as Questions
import Page.DisplayPoll.Session as Sessions
import Picasso.Button exposing (button, elevated, filled)
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
    | Loaded Poll Questions.Model Sessions.Model
    | Error PollError


type alias Model =
    { viewer : Viewer
    , titleInput : String
    , state : State
    }


subscriptions : Model -> Sub Message
subscriptions model =
    case model.state of
        Loaded _ _ sessionModel ->
            Sub.map SessionMessage (Sessions.subscriptions sessionModel)

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


type Message
    = WriteNewTitle String
    | ClickPollTitleButton
    | GotNewPoll Poll
    | GotError PollError
    | RequestNavigateToPoll Poll
      -- Sub model
    | QuestionMessage Questions.Message
    | SessionMessage Sessions.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        SessionMessage subMessage ->
            case model.state of
                Loaded poll pModel sModel ->
                    let
                        ( updatedModel, cmd ) =
                            Sessions.update subMessage sModel
                    in
                    ( { model | state = Loaded poll pModel updatedModel }, Cmd.map SessionMessage cmd )

                _ ->
                    model |> withNoCmd

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
                ( questionModel, questionCmd ) =
                    Questions.init model.viewer poll

                ( sessionModel, sessionCmd ) =
                    Sessions.init model.viewer poll

                updated =
                    { model | state = Loaded poll questionModel sessionModel }
            in
            case model.state of
                CreatingNew ->
                    updated
                        |> withCmd
                            [ Cmd.succeed <| RequestNavigateToPoll poll
                            , Cmd.map QuestionMessage questionCmd
                            , Cmd.map SessionMessage sessionCmd
                            ]

                _ ->
                    updated
                        |> withCmd
                            [ Cmd.map QuestionMessage questionCmd
                            , Cmd.map SessionMessage sessionCmd
                            ]



-- VIEW


view : Model -> List (Html Message)
view model =
    let
        prepended =
            case model.state of
                Loaded _ _ sModel ->
                    List.map (Html.map SessionMessage) (Sessions.moderatorView sModel)

                _ ->
                    []

        appended =
            case model.state of
                Loaded _ qModel _ ->
                    List.map (Html.map QuestionMessage) (Questions.view qModel)

                _ ->
                    []
    in
    prepended
        ++ [ div
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
                [ styledH2 <|
                    case model.state of
                        Loaded poll _ _ ->
                            poll.title

                        _ ->
                            "Create a poll"
                , inputTitle <| model
                , buttonPollTitle model.state
                ]
           ]
        ++ appended


inputTitle : Model -> Html Message
inputTitle model =
    div []
        [ Input.inputWithTitle "Poll title: "
            [ onInput WriteNewTitle
            , placeholder "Diu vivere Caesar"
            , value model.titleInput
            ]
            []
            |> withMargin
        ]


buttonPollTitle : State -> Html Message
buttonPollTitle state =
    let
        message =
            case state of
                Loaded poll _ _ ->
                    "Update title"

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
