module Page.Poll exposing
    ( Message
    , Model
    , initCreate
    , initDisplay
    , subscriptions
    , update
    , view
    )

import Api.Polls exposing (ClientPoll, PollDiscriminator, ServerPoll)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnterDown)
import Page.Poll.Questions as Questions
import Page.Poll.Session as Sessions
import Picasso.Button exposing (button, elevated, filled)
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
    = Creating
    | LoadingExisting
    | Editing ServerPoll Questions.Model Sessions.Model
    | Ready ServerPoll Questions.Model Sessions.Model


type alias Model =
    { viewer : Viewer
    , titleInput : String
    , state : State
    , error : Maybe PollError
    }


subscriptions : Model -> Sub Message
subscriptions model =
    case model.state of
        Ready poll _ sessionModel ->
            Sub.batch
                [ Sub.map SessionMessage (Sessions.subscriptions sessionModel)
                , Time.every (1000 * 20) (always (LoadPoll { idPoll = poll.idPoll }))
                ]

        Editing poll _ sessionModel ->
            Sub.batch
                [ Sub.map SessionMessage (Sessions.subscriptions sessionModel)
                , Time.every (1000 * 20) (always (LoadPoll { idPoll = poll.idPoll }))
                ]

        _ ->
            Sub.none


initCreate : Viewer -> ( Model, Cmd Message )
initCreate viewer =
    { viewer = viewer
    , titleInput = ""
    , state = Creating
    , error = Nothing
    }
        |> withNoCmd


initDisplay : Viewer -> PollDiscriminator -> ( Model, Cmd Message )
initDisplay viewer pollDiscriminator =
    { viewer = viewer
    , titleInput = ""
    , state = LoadingExisting
    , error = Nothing
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
    | GotNewPoll ServerPoll
    | GotError PollError
    | RequestNavigateToPoll ServerPoll
    | LoadPoll PollDiscriminator
      -- Sub model
    | QuestionMessage Questions.Message
    | SessionMessage Sessions.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadPoll discriminator ->
            model
                |> withCmd
                    [ Api.Polls.getPoll
                        (Session.viewerCredentials model.viewer)
                        discriminator
                        GotNewPoll
                        |> Task.mapError (always <| GotError UpdateError)
                        |> Task.Extra.execute
                    ]

        SessionMessage subMessage ->
            case model.state of
                Ready poll pModel sModel ->
                    let
                        ( updatedModel, cmd ) =
                            Sessions.update subMessage sModel
                    in
                    ( { model | state = Ready poll pModel updatedModel }, Cmd.map SessionMessage cmd )

                _ ->
                    model |> withNoCmd

        QuestionMessage subMessage ->
            case model.state of
                Ready poll pollModel session ->
                    let
                        ( updatedModel, cmd ) =
                            Questions.update subMessage pollModel
                    in
                    ( { model | state = Ready poll updatedModel session }, Cmd.map QuestionMessage cmd )

                _ ->
                    model |> withNoCmd

        WriteNewTitle title ->
            { model | titleInput = title }
                |> withNoCmd

        ClickPollTitleButton ->
            case model.state of
                Creating ->
                    model
                        |> withCmd
                            [ Api.Polls.create
                                (Session.viewerCredentials model.viewer)
                                (ClientPoll model.titleInput)
                                RequestNavigateToPoll
                                |> Task.mapError (always <| GotError CreateError)
                                |> Task.Extra.execute
                            ]

                LoadingExisting ->
                    model |> withNoCmd

                Ready poll q s ->
                    { model | state = Editing poll q s }
                        |> withNoCmd

                Editing poll _ _ ->
                    { model | state = LoadingExisting }
                        |> withCmd
                            [ Api.Polls.update
                                (Session.viewerCredentials model.viewer)
                                (PollDiscriminator poll.idPoll)
                                (ClientPoll model.titleInput)
                                GotNewPoll
                                |> Task.mapError (always <| GotError UpdateError)
                                |> Task.Extra.execute
                            ]

        RequestNavigateToPoll poll ->
            model
                |> withCmd
                    [ Route.replaceUrl
                        (Session.viewerNavKey model.viewer)
                        (Route.DisplayPoll (PollDiscriminator poll.idPoll))
                    ]

        GotError error ->
            { model | error = Just error } |> withNoCmd

        GotNewPoll poll ->
            let
                ( questionModel, questionCmd ) =
                    Questions.init model.viewer poll

                ( sessionModel, sessionCmd ) =
                    Sessions.init model.viewer poll

                updated =
                    case model.state of
                        Editing _ _ _ ->
                            { model | state = Editing poll questionModel sessionModel }

                        _ ->
                            { model | state = Ready poll questionModel sessionModel }
            in
            case model.state of
                Creating ->
                    updated
                        |> withCmd
                            [ Cmd.succeed <| RequestNavigateToPoll poll
                            ]

                LoadingExisting ->
                    { updated | titleInput = poll.title }
                        |> withCmd
                            [ Cmd.map QuestionMessage questionCmd
                            , Cmd.map SessionMessage sessionCmd
                            ]

                Ready _ _ _ ->
                    { model | titleInput = poll.title }
                        |> withCmd
                            [ Cmd.map QuestionMessage questionCmd
                            , Cmd.map SessionMessage sessionCmd
                            ]

                Editing _ _ _ ->
                    model
                        |> withCmd
                            [ Cmd.map QuestionMessage questionCmd
                            , Cmd.map SessionMessage sessionCmd
                            ]



-- VIEW


view : Model -> List (Html Message)
view model =
    let
        editing =
            case model.state of
                Creating ->
                    True

                Editing _ _ _ ->
                    True

                _ ->
                    False

        textColor =
            if editing then
                "text-black font-semibold"

            else
                "text-gray-500 font-semibold cursor-not-allowed"
    in
    prepended model
        ++ [ div
                [ class "align-middle mx-2 md:mx-8 mt-8"
                , class "bg-white shadow md:rounded-lg p-4"
                ]
                [ Html.span
                    [ class "block font-archivo capitalize text-gray-500" ]
                    [ Html.text "Poll Title :" ]
                , div [ class "flex flex-row items-center mt-2" ]
                    [ Input.input
                        [ onInput WriteNewTitle
                        , onEnterDown ClickPollTitleButton
                        , placeholder "What do unicorns dream of ? \u{1F984}"
                        , value model.titleInput
                        , class "flex-grow"
                        , class "mr-4"
                        , class textColor
                        , Html.Attributes.readonly (not editing)
                        ]
                        []
                    , buttonPollTitle model.state
                    ]
                ]
           ]
        ++ appended model


prepended model =
    case model.state of
        Ready _ _ sModel ->
            List.map (Html.map SessionMessage) (Sessions.moderatorView sModel)

        Editing _ _ sModel ->
            List.map (Html.map SessionMessage) (Sessions.moderatorView sModel)

        _ ->
            []


appended model =
    case model.state of
        Ready _ qModel _ ->
            List.map (Html.map QuestionMessage) (Questions.view qModel)

        Editing _ qModel _ ->
            List.map (Html.map QuestionMessage) (Questions.view qModel)

        _ ->
            []


buttonPollTitle : State -> Html Message
buttonPollTitle state =
    let
        message =
            case state of
                Ready poll _ _ ->
                    "Edit"

                LoadingExisting ->
                    "Loading..."

                Creating ->
                    "Create"

                Editing poll _ _ ->
                    "Save changes"

        style =
            case state of
                Creating ->
                    Picasso.Button.filled ++ elevated

                LoadingExisting ->
                    Picasso.Button.filledDisabled ++ Picasso.Button.outlinedLight

                _ ->
                    Picasso.Button.filledLight ++ Picasso.Button.outlinedLight
    in
    button
        (style ++ [ onClick ClickPollTitleButton ])
        [ text message ]
