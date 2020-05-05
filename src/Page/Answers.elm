module Page.Answers exposing
    ( Message
    , Model
    , init
    , show
    , update
    , view
    )

import Api.Answers exposing (ServerAnswer)
import Api.Questions exposing (QuestionDiscriminator, QuestionVisibility(..), ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events as Html
import Session exposing (Viewer)
import Task
import Task.Extra



-- MODEL


type AnswersState
    = Loading
    | Loaded (List ServerAnswer)
    | Error Viewer


type alias Model =
    { viewer : Viewer
    , state : AnswersState
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    { viewer = viewer
    , state = Loading
    }
        |> withCmd
            [ QuestionDiscriminator discriminator.idPoll discriminator.idQuestion
                |> NowRequestAnswers
                |> Cmd.succeed
            ]



-- UPDATE


type Message
    = NowRequestAnswers QuestionDiscriminator
    | GotAnswerList (List ServerAnswer)
    | GotInvalidCredentials
    | GotError


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NowRequestAnswers questionDiscriminator ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Answers.getAnswerList viewer questionDiscriminator identity
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



-- VIEW


show : Html msg
show =
    div [] [ Html.text "show" ]


view : Model -> Html msg
view model =
    case model.state of
        Loading ->
            div [] [ Html.text "Loading" ]

        Loaded serverAnswers ->
            div [] [ Html.text "Loaded" ]

        Error viewer ->
            div [] [ Html.text "Error" ]
