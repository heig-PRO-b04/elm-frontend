module Page.Poll.Questions exposing
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
    | NowCreateQuestion ClientQuestion
    | NowRequestQuestions
    | GotAllQuestions (List ServerQuestion)
    | GotInvalidCredentials


init : Viewer -> Api.Polls.Poll -> ( Model, Cmd Message )
init viewer poll =
    { viewer = viewer
    , poll = poll
    , questions = []
    , newQuestion = ClientQuestion "" "" Visible 1 1
    }
        |> withCmd [ Cmd.succeed <| NowRequestQuestions ]


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        WriteNewTitle string ->
            let
                question =
                    model.newQuestion
            in
            { model | newQuestion = { question | title = string } }
                |> withNoCmd

        NowCreateQuestion clientQuestion ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Questions.create viewer { idPoll = model.poll.idPoll } clientQuestion identity
                        |> Task.map (always NowRequestQuestions)
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

        GotAllQuestions serverQuestionList ->
            { model | questions = serverQuestionList |> List.sortBy .title }
                |> withNoCmd

        NowRequestQuestions ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Questions.getQuestionList viewer { idPoll = model.poll.idPoll } identity
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

        GotInvalidCredentials ->
            let
                viewer =
                    Session.viewerNavKey model.viewer
            in
            model
                |> withCmd [ Route.badCredentials viewer ]


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
        , inputTitle <| model.newQuestion.title
        , buttonNewQuestionTitle model.newQuestion
        , buttonRequestQuestions
        ]
    ]
        ++ showQuestionList model.questions


showQuestionList : List ServerQuestion -> List (Html Message)
showQuestionList questions =
    List.map (\serverQuestion -> showQuestion serverQuestion) questions


showQuestion : ServerQuestion -> Html Message
showQuestion serverQuestion =
    div
        [ class "flex flex-col"
        , class "m-auto my-4 md:my-16"
        , class "bg-white shadow p-8 md:rounded-lg md:w-1/2 md:max-w-lg"
        ]
        [ styledH2 <| serverQuestion.title
        ]


inputTitle : String -> Html Message
inputTitle title =
    div []
        [ Input.inputWithTitle "Question title: "
            [ onInput WriteNewTitle
            , placeholder "Et tu, Brute?"
            , value title
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


buttonRequestQuestions : Html Message
buttonRequestQuestions =
    let
        message =
            "Refresh questions"
    in
    button
        (filled ++ elevated ++ [ onClick <| NowRequestQuestions, class "mt-8" ])
        [ text message ]


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]
