module Page.Answers exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Answers exposing (AnswerDiscriminator, ClientAnswer, ServerAnswer)
import Api.Questions exposing (QuestionDiscriminator, QuestionVisibility(..), ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, span)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Event
import Page.Answers.Indices as Indices
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
    , creating : Bool
    , modifying : Maybe Int
    , titleCreate : String
    , descriptionCreate : String
    , titleModify : String
    , descriptionModify : String
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    ( { viewer = viewer
      , state = Loading
      , question = QuestionDiscriminator discriminator.idPoll discriminator.idQuestion
      , creating = False
      , modifying = Nothing
      , titleCreate = ""
      , descriptionCreate = ""
      , titleModify = ""
      , descriptionModify = ""
      }
    , Cmd.succeed PerformReload
    )



-- UPDATE


type Message
    = WriteCreateTitle String
    | WriteCreateDescription String
    | WriteModifyTitle String
    | WriteModifyDescription String
    | PerformReload
    | PerformCreateMode Bool
    | PerformModifyMode (Maybe Int)
    | PerformCreate ClientAnswer
    | PerformUpdate ServerAnswer ClientAnswer
    | PerformDelete ServerAnswer
    | GotAnswerList (List ServerAnswer)
    | GotInvalidCredentials
    | GotError


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        WriteCreateTitle string ->
            { model | titleCreate = string }
                |> withNoCmd

        WriteCreateDescription string ->
            { model | descriptionCreate = string }
                |> withNoCmd

        WriteModifyTitle string ->
            { model | titleModify = string }
                |> withNoCmd

        WriteModifyDescription string ->
            { model | descriptionModify = string }
                |> withNoCmd

        PerformReload ->
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

        PerformCreateMode bool ->
            { model | creating = bool }
                |> withNoCmd

        PerformModifyMode maybeId ->
            case maybeId of
                Just id ->
                    case model.state of
                        Loaded serverAnswers ->
                            let
                                answerSingleton =
                                    List.filter (\ans -> ans.idAnswer == id) serverAnswers

                                probablyAnswer =
                                    List.head answerSingleton

                                title =
                                    case probablyAnswer of
                                        Just answer ->
                                            answer.title

                                        Nothing ->
                                            ""

                                desc =
                                    case probablyAnswer of
                                        Just answer ->
                                            answer.description

                                        Nothing ->
                                            ""
                            in
                            { model | modifying = Just id, titleModify = title, descriptionModify = desc }
                                |> withNoCmd

                        _ ->
                            { model | modifying = Nothing, titleModify = "", descriptionModify = "" }
                                |> withNoCmd

                _ ->
                    { model | modifying = Nothing, titleModify = "", descriptionModify = "" }
                        |> withNoCmd

        PerformCreate clientAnswer ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            { model | creating = False, titleCreate = "", descriptionCreate = "" }
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
                        |> Task.andThen (always <| Task.succeed PerformReload)
                        |> Task.Extra.execute
                    ]

        PerformUpdate serverAnswer clientAnswer ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            { model | modifying = Nothing, titleModify = "", descriptionModify = "" }
                |> withCmd
                    [ Api.Answers.update viewer (AnswerDiscriminator serverAnswer.idPoll serverAnswer.idQuestion serverAnswer.idAnswer) clientAnswer identity
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Answers.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotError
                            )
                        |> Task.andThen (always <| Task.succeed PerformReload)
                        |> Task.Extra.execute
                    ]

        PerformDelete serverAnswer ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Answers.delete viewer (AnswerDiscriminator serverAnswer.idPoll serverAnswer.idQuestion serverAnswer.idAnswer) identity
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Answers.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotError
                            )
                        |> Task.andThen (always <| Task.succeed PerformReload)
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


view : Model -> Html Message
view model =
    div
        [ class "border-b bg-gray-100" ]
        [ answerHeader model
        , showAnswerList model
        ]


answerHeader : Model -> Html Message
answerHeader model =
    let
        header =
            if model.creating then
                newAnswerInput model

            else
                div
                    [ class "flex flex-row"
                    ]
                    [ span
                        [ class "font-archivo font-semibold text-gray-600 p-4" ]
                        [ Html.text (headerText model.state) ]
                    , div [ class "flex-grow" ] []
                    , newAnswerButton
                    ]
    in
    header


headerText : AnswersState -> String
headerText state =
    let
        empty =
            "Your answers for this question will appear here! Press the \"New Answer\" button to get started!"

        notEmpty =
            "Here are the answers for this question! You can modify them by clicking on the ✏️ icon next to them!"
    in
    case state of
        Loading ->
            "Loading answers..."

        Loaded serverAnswers ->
            if List.length serverAnswers == 0 then
                empty

            else
                notEmpty

        Error viewer ->
            "An error has occured. Please try again later"


showAnswerList : Model -> Html Message
showAnswerList model =
    case model.state of
        Loaded serverAnswers ->
            div [ class "flex-col" ]
                (List.sortBy .idAnswer serverAnswers
                    |> List.indexedMap
                        (\index answer ->
                            answerOrEdit
                                index
                                model.titleModify
                                model.descriptionModify
                                answer
                                model.modifying
                        )
                )

        _ ->
            div [] []


answerOrEdit : Int -> String -> String -> ServerAnswer -> Maybe Int -> Html Message
answerOrEdit index title desc answer maybeModify =
    case maybeModify of
        Just id ->
            if answer.idAnswer == id then
                modifyAnswerInput title desc answer

            else
                showAnswer index answer

        Nothing ->
            showAnswer index answer


showAnswer : Int -> ServerAnswer -> Html Message
showAnswer index answer =
    div
        [ class "flex flex-row items-center py-3 pl-4"
        , class "font-archivo font-semibold text-gray-700"
        ]
        [ span [ class "ml-10 text-gray-500" ] [ Html.text <| Indices.forIndex index ++ ".\u{00A0}" ]
        , span [] [ Html.text answer.title ]
        , span [ class "text-gray-400" ] [ Html.text "\u{00A0}/\u{00A0}" ]
        , span [ class "text-gray-500" ]
            [ Html.text <|
                if String.isEmpty answer.description then
                    "-"

                else
                    answer.description
            ]
        , div [ class "flex-grow" ] []
        , div [] [ modifyAnswerButton answer ]
        , div [] [ deleteAnswerButton answer ]
        ]


newAnswerInput : Model -> Html Message
newAnswerInput model =
    let
        created =
            ClientAnswer model.titleCreate model.descriptionCreate
    in
    div
        [ class "w-full flex flex-row items-center"
        , class "border-b active:shadow-inner bg-gray-100"
        , class "py-3 pl-4"
        ]
        [ Input.input
            [ Event.onInput WriteCreateTitle
            , Attribute.placeholder "✍️  New answer title..."
            , class "flex-grow"
            , class "mr-3"
            , Attribute.value model.titleCreate
            ]
            []
        , Input.input
            [ Event.onInput WriteCreateDescription
            , Attribute.placeholder "📄️  New answer description..."
            , class "flex-grow"
            , Attribute.value model.descriptionCreate
            ]
            []
        , Html.button
            [ Event.onClick <| PerformCreateMode False
            , class "flex-end"
            , class "font-bold"
            , class "text-right pl-8 text-gray-500 hover:text-gray-600"
            ]
            [ Html.text "Cancel" ]
        , Html.button
            [ Event.onClick <| PerformCreate created
            , class "flex-end"
            , class "font-bold"
            , class "text-right px-8 text-seaside-600 hover:text-seaside-700"
            ]
            [ Html.text "Create" ]
        ]


modifyAnswerInput : String -> String -> ServerAnswer -> Html Message
modifyAnswerInput title desc answer =
    let
        modified =
            ClientAnswer title desc
    in
    div
        [ class "w-full flex flex-row items-center"
        , class "border-b active:shadow-inner bg-gray-100"
        , class "py-3 pl-4"
        ]
        [ Input.input
            [ Event.onInput WriteModifyTitle
            , Attribute.placeholder "✍️  Modify answer title..."
            , class "flex-grow"
            , class "mr-3"
            , Attribute.value title
            ]
            []
        , Input.input
            [ Event.onInput WriteModifyDescription
            , Attribute.placeholder "📄️  Modify answer description..."
            , class "flex-grow"
            , Attribute.value desc
            ]
            []
        , Html.button
            [ Event.onClick <| PerformModifyMode Nothing
            , class "flex-end"
            , class "font-bold"
            , class "text-right pl-8 text-gray-500 hover:text-gray-600"
            ]
            [ Html.text "Cancel" ]
        , Html.button
            [ Event.onClick <| PerformUpdate answer modified
            , class "flex-end"
            , class "font-bold"
            , class "text-right px-8 text-seaside-600 hover:text-seaside-700"
            ]
            [ Html.text "Modify" ]
        ]


newAnswerButton : Html Message
newAnswerButton =
    Html.button
        [ Event.onClick <| PerformCreateMode True
        , class "flex-end"
        , class "font-bold"
        , class "text-right px-8 text-seaside-600"
        ]
        [ Html.text "New answer" ]


modifyAnswerButton : ServerAnswer -> Html Message
modifyAnswerButton answer =
    Html.img
        [ Attribute.src "/icon/pencil.svg"
        , class "h-6 w-6 cursor-pointer"
        , Event.onClick <| PerformModifyMode (Just answer.idAnswer)
        ]
        []


deleteAnswerButton : ServerAnswer -> Html Message
deleteAnswerButton answer =
    Html.button
        [ Attribute.class "text-gray-500 hover:text-red-500 capitalize font-archivo"
        , Attribute.class "text-right px-8"
        , Event.onClick <| PerformDelete answer
        ]
        [ Html.text "Delete" ]
