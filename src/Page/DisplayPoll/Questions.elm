module Page.DisplayPoll.Questions exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll, PollDiscriminator)
import Api.Questions exposing (ClientQuestion, QuestionDiscriminator, ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html)
import Route
import Session exposing (Viewer)
import Task
import Task.Extra


type alias Model =
    { viewer : Viewer
    , poll : Poll
    , questions : List ServerQuestion
    }


type Message
    = GotAllQuestions (List ServerQuestion)
    | GotInvalidCredentials
    | NowRequestQuestions PollDiscriminator
    | NowCreateQuestion ClientQuestion
    | NowDeleteQuestion QuestionDiscriminator


init : Viewer -> Api.Polls.Poll -> ( Model, Cmd Message )
init viewer poll =
    { viewer = viewer
    , poll = poll
    , questions = []
    }
        |> withCmd [ Cmd.succeed <| NowRequestQuestions <| PollDiscriminator poll.idPoll ]


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        GotAllQuestions serverQuestionList ->
            { model | questions = serverQuestionList }
                |> withNoCmd

        NowCreateQuestion clientQuestion ->
            model
                |> withNoCmd

        NowRequestQuestions pollDiscriminator ->
            model
                |> withCmd
                    [ Api.Questions.getQuestionList (Session.viewerCredentials model.viewer) pollDiscriminator identity
                        |> Task.map GotAllQuestions
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Questions.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotAllQuestions []
                            )
                        |> Task.Extra.execute
                    ]

        NowDeleteQuestion questionDiscriminator ->
            model |> withNoCmd

        GotInvalidCredentials ->
            model
                |> withCmd [ Route.badCredentials (Session.viewerNavKey model.viewer) ]


view : Model -> List (Html Message)
view model =
    []
