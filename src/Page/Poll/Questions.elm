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
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Page.Question as Question
import Picasso.Button exposing (button, elevated, filled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Route
import Session exposing (Viewer)
import Set exposing (Set)
import Task
import Task.Extra


type alias QuestionIdentifier =
    ( Int, Int )


type alias Model =
    { viewer : Viewer
    , poll : Poll
    , questions : Dict QuestionIdentifier Question.Model
    , newQuestion : ClientQuestion
    }


type Message
    = WriteNewTitle String
    | NowCreateQuestion ClientQuestion
    | NowRequestQuestions
    | GotAllQuestions (List ServerQuestion)
    | GotQuestionMessage QuestionIdentifier Question.Message
    | GotInvalidCredentials


init : Viewer -> Api.Polls.Poll -> ( Model, Cmd Message )
init viewer poll =
    { viewer = viewer
    , poll = poll
    , questions = Dict.empty
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
            let
                keys : Set QuestionIdentifier
                keys =
                    serverQuestionList
                        |> List.map (\question -> ( question.idPoll, question.idQuestion ))
                        |> Set.fromList

                -- STEP 1 : Remove the extra keys.
                withRemovals : Dict QuestionIdentifier Question.Model
                withRemovals =
                    let
                        toRemove : Set QuestionIdentifier
                        toRemove =
                            Dict.keys model.questions
                                |> Set.fromList
                                |> Set.diff keys
                    in
                    Set.foldr
                        (\id dict -> Dict.remove id dict)
                        model.questions
                        toRemove

                -- STEP 2 : Add the missing keys.
                ( withInsertions, commands ) =
                    let
                        toInsert : Set QuestionIdentifier
                        toInsert =
                            Set.diff keys (Set.fromList (Dict.keys withRemovals))
                    in
                    Set.foldr
                        (\( idPoll, idQuestion ) ( dict, cmd ) ->
                            let
                                id =
                                    ( idPoll, idQuestion )

                                ( m, c ) =
                                    Question.init model.viewer
                                        { idPoll = idPoll
                                        , idQuestion = idQuestion
                                        }

                                newCmd =
                                    Cmd.batch
                                        [ c |> Cmd.map (GotQuestionMessage id)
                                        , cmd
                                        ]
                            in
                            ( Dict.insert id m dict, newCmd )
                        )
                        ( withRemovals, Cmd.none )
                        toInsert
            in
            ( { model | questions = withInsertions }, commands )

        GotQuestionMessage identifier message ->
            let
                updated : Maybe ( Question.Model, Cmd Question.Message )
                updated =
                    model.questions
                        |> Dict.get identifier
                        |> Maybe.map (\m -> Question.update message m)
            in
            case updated of
                Just ( mod, cmd ) ->
                    ( { model | questions = model.questions |> Dict.insert identifier mod }
                    , Cmd.map (GotQuestionMessage identifier) cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

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


showQuestionList : Dict QuestionIdentifier Question.Model -> List (Html Message)
showQuestionList questions =
    questions
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map (\serverQuestion -> showQuestion serverQuestion)


showQuestion : ( QuestionIdentifier, Question.Model ) -> Html Message
showQuestion ( identifier, model ) =
    Question.view model
        |> Html.map (\msg -> GotQuestionMessage identifier msg)


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
