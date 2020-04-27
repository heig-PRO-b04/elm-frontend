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
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Page.Question as Question
import Picasso.Button exposing (button, elevated, filled)
import Picasso.FloatingButton
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
    , newQuestion : Maybe ClientQuestion
    }


type Message
    = WriteNewTitle String
    | NowStartCreateQuestion
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
    , newQuestion = Nothing
    }
        |> withCmd [ Cmd.succeed <| NowRequestQuestions ]


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        WriteNewTitle string ->
            let
                question =
                    model.newQuestion
                        |> Maybe.map (\x -> { x | title = string })
            in
            { model | newQuestion = question }
                |> withNoCmd

        NowStartCreateQuestion ->
            { model
                | newQuestion =
                    Just <|
                        { title = ""
                        , details = ""
                        , visibility = Visible
                        , answersMin = 1
                        , answersMax = 1
                        }
            }
                |> withNoCmd

        NowCreateQuestion clientQuestion ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            { model | newQuestion = Nothing }
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
    let
        questionView =
            case model.newQuestion of
                Just question ->
                    div
                        [ class "flex flex-col"
                        , class "m-auto my-4 md:my-16"
                        , class "bg-white shadow p-8 md:rounded-lg md:w-1/2 md:max-w-lg"
                        ]
                        [ styledH2 <| "Create a new question"
                        , inputTitle <| question.title
                        , buttonNewQuestionTitle question
                        , buttonRequestQuestions
                        ]

                Nothing ->
                    div [] []

        fabView =
            case model.newQuestion of
                Just _ ->
                    div [] []

                Nothing ->
                    Picasso.FloatingButton.button
                        [ class "fixed right-0 bottom-0 m-8"
                        , onClick NowStartCreateQuestion
                        ]
                        [ img [ src "/icon/action-button-plus.svg" ] []
                        , div [ class "ml-4" ] [ text "New question" ]
                        ]
    in
    [ viewQuestionsTable model.questions
    , questionView
    , fabView
    ]


viewQuestionsTable : Dict QuestionIdentifier Question.Model -> Html Message
viewQuestionsTable model =
    let
        headerBase =
            class "font-bold font-archivo text-gray-500 text-left tracking-wider border-gray-200 select-none py-3"
    in
    div [ class "align-middle mx-2 md:mx-8 mt-8 mb-32" ]
        [ Html.table [ class "min-w-full center border rounded-lg overflow-hidden shadow" ]
            [ Html.thead [ class "bg-gray-100 border-b" ]
                [ Html.td [ class "px-6", headerBase ] [ Html.text "Title" ]
                , Html.td [ class "px-2", headerBase ] [ Html.text "Visibility" ]
                , Html.td [] []
                ]
            , Html.tbody [ class "bg-white" ] (showQuestionList model)
            ]
        ]


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
