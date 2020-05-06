module Page.Answers exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Answers exposing (ClientAnswer, ServerAnswer)
import Api.Questions exposing (QuestionDiscriminator, QuestionVisibility(..), ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Picasso.Input as Input
import Session exposing (Viewer)
import Task exposing (Task)
import Task.Extra



-- MODEL


type AnswersState
    = Loading
    | Loaded (List ServerAnswer)
    | Error Viewer


type alias Model =
    { viewer : Viewer
    , state : AnswersState
    , question : QuestionDiscriminator
    , titleInput : String
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    ( { viewer = viewer
      , state = Loading
      , question = QuestionDiscriminator discriminator.idPoll discriminator.idQuestion
      , titleInput = "placeholder text"
      }
    , Cmd.succeed NowRequestAnswers
    )



-- UPDATE


type Message
    = NowRequestAnswers
    | GotAnswerList (List ServerAnswer)
    | NowCreateAnswer ClientAnswer
    | GotInvalidCredentials
    | GotError
    | WriteNewTitle String


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NowRequestAnswers ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Answers.getAnswerList viewer model.question identity
                        |> Task.map GotAnswerList
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Answers.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotError
                            )
                        |> Task.Extra.execute
                    ]

        GotAnswerList serverAnswerList ->
            { model | state = Loaded serverAnswerList }
                |> withNoCmd

        GotInvalidCredentials ->
            { model | state = Error model.viewer }
                |> withNoCmd

        GotError ->
            { model | state = Error model.viewer }
                |> withNoCmd

        WriteNewTitle string ->
            { model | titleInput = string }
                |> withNoCmd

        NowCreateAnswer clientAnswer ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Answers.create viewer model.question clientAnswer identity
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Answers.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotError
                            )
                        |> Task.andThen (always <| Task.succeed NowRequestAnswers)
                        |> Task.Extra.execute
                    ]



-- VIEW


view : Model -> Html Message
view model =
    div
        []
        [ Html.input [ value model.titleInput, onInput WriteNewTitle ] []
        , Html.button [ onClick <| NowCreateAnswer <| ClientAnswer model.titleInput "" ] [ Html.text "**save**" ]
        ]


showAnswerList : Model -> Html msg
showAnswerList model =
    case model.state of
        Loading ->
            div [] [ Html.text "Loading" ]

        Loaded serverAnswers ->
            div [] (List.map (\answer -> showAnswer answer) serverAnswers)

        Error viewer ->
            div [] [ Html.text "Error" ]


showAnswer : ServerAnswer -> Html msg
showAnswer answer =
    Html.text answer.title
