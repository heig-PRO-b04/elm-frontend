module Page.Poll.Questions exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll, PollDiscriminator)
import Api.Questions
    exposing
        ( ClientQuestion
        , QuestionDiscriminator
        , QuestionVisibility(..)
        , ServerQuestion
        )
import Cmd exposing (withCmd, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, colspan, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Page.Question as Question
import Picasso.FloatingButton
import Picasso.Input as Input
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
    [ viewQuestionsTable model
    , fabView
    ]


viewQuestionsTable : Model -> Html Message
viewQuestionsTable model =
    let
        questionView =
            case model.newQuestion of
                Just question ->
                    Html.tr [ class "border-b active:shadow-inner hover:bg-gray-100" ]
                        [ Html.td
                            [ class "py-3 pl-4", colspan 2, class "w-full" ]
                            [ inputTitle <| question.title ]
                        , Html.td
                            [ class "text-right px-8 text-seaside-600" ]
                            [ Html.button
                                [ onClick <| NowCreateQuestion question
                                , class "font-bold"
                                ]
                                [ text "Create" ]
                            ]
                        ]

                Nothing ->
                    div [] []

        headerBase =
            class "font-bold font-archivo text-gray-500 text-left tracking-wider border-gray-200 select-none py-3"
    in
    div [ class "align-middle mx-2 md:mx-8 mt-8 mb-32" ]
        [ Html.table [ class "min-w-full center border rounded-lg overflow-hidden shadow" ]
            [ Html.thead [ class "bg-gray-100 border-b" ]
                [ Html.td [ class "px-6 w-full", headerBase ] [ Html.text "Title" ]
                , Html.td [ class "px-2", headerBase ] [ Html.text "Visibility" ]
                , Html.td [] []
                ]
            , Html.tbody [ class "bg-white" ] (questionView :: viewQuestionList model.questions)
            ]
        ]


viewQuestionList : Dict QuestionIdentifier Question.Model -> List (Html Message)
viewQuestionList questions =
    questions
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map (\serverQuestion -> viewQuestion serverQuestion)


viewQuestion : ( QuestionIdentifier, Question.Model ) -> Html Message
viewQuestion ( identifier, model ) =
    Question.view model
        |> Html.map (\msg -> GotQuestionMessage identifier msg)


inputTitle : String -> Html Message
inputTitle title =
    Input.input [ onInput WriteNewTitle, placeholder "🚀 New question...", class "w-full", value title ] []
