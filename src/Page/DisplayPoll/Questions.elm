module Page.DisplayPoll.Questions exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll, PollDiscriminator)
import Api.Questions exposing (ClientQuestion, QuestionDiscriminator, QuestionVisibility(..), ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Picasso.Button exposing (button, elevated, filled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Route
import Session exposing (Viewer)
import Task
import Task.Extra


type alias Model =
    { viewer : Viewer
    , poll : Poll
    , questions : List ServerQuestion
    , newQuestion : ClientQuestion
    }


type Message
    = WriteNewTitle String
    | GotAllQuestions (List ServerQuestion)
    | GotInvalidCredentials
    | NowCreateQuestion ClientQuestion
    | GotQuestion ServerQuestion
    | NowRequestQuestions PollDiscriminator
    | NowDeleteQuestion QuestionDiscriminator


init : Viewer -> Api.Polls.Poll -> ( Model, Cmd Message )
init viewer poll =
    { viewer = viewer
    , poll = poll
    , questions = []
    , newQuestion =
        ClientQuestion "" "" Visible 1 1
    }
        |> withCmd [ Cmd.succeed <| NowRequestQuestions <| PollDiscriminator poll.idPoll ]


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        WriteNewTitle string ->
            let
                details =
                    model.newQuestion.details

                visibility =
                    model.newQuestion.visibility

                ansMin =
                    model.newQuestion.answersMin

                ansMax =
                    model.newQuestion.answersMax
            in
            { model | newQuestion = ClientQuestion string details visibility ansMin ansMax }
                |> withNoCmd

        NowCreateQuestion clientQuestion ->
            -- TODO: Should this reload all questions?
            model
                |> withCmd
                    [ Api.Questions.create (Session.viewerCredentials model.viewer) clientQuestion identity
                        |> Task.map GotQuestion
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

        GotQuestion serverQuestion ->
            { model | questions = serverQuestion :: model.questions }
                |> withNoCmd

        GotAllQuestions serverQuestionList ->
            { model | questions = serverQuestionList }
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
        [ styledH2 <| "Create a new question"
        , inputTitle <| model
        , buttonNewQuestionTitle model.newQuestion
        ]
    ]


inputTitle : Model -> Html Message
inputTitle model =
    div []
        [ Input.inputWithTitle "Question title: "
            [ onInput WriteNewTitle
            , placeholder "Et tu, Brute?"
            , value model.newQuestion.title
            ]
            []
            |> withMargin
        ]


buttonNewQuestionTitle : ClientQuestion -> Html Message
buttonNewQuestionTitle newQuestion =
    let
        message =
            "Create"
    in
    button
        (filled ++ elevated ++ [ onClick <| NowCreateQuestion newQuestion, class "mt-8" ])
        [ text message ]


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]
