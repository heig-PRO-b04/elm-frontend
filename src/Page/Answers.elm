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
import Html exposing (Html, div, text)
import Html.Attributes as Attribute exposing (class, placeholder, value)
import Html.Events as Event exposing (onClick, onInput)
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
    , descriptionInput : String
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    ( { viewer = viewer
      , state = Loading
      , question = QuestionDiscriminator discriminator.idPoll discriminator.idQuestion
      , titleInput = ""
      , descriptionInput = ""
      }
    , Cmd.succeed PerformReload
    )



-- UPDATE


type Message
    = WriteNewTitle String
    | WriteNewDescription String
    | PerformReload
    | PerformCreate ClientAnswer
    | PerformUpdate ServerAnswer ClientAnswer
    | PerformDelete ServerAnswer
    | GotAnswerList (List ServerAnswer)
    | GotInvalidCredentials
    | GotError


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        WriteNewTitle string ->
            { model | titleInput = string }
                |> withNoCmd

        WriteNewDescription string ->
            { model | descriptionInput = string }
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

        PerformCreate clientAnswer ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            { model | titleInput = "", descriptionInput = "" }
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
            model
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
        []
        [ createNewAnswer model
        , showAnswerList model
        ]


createNewAnswer : Model -> Html Message
createNewAnswer model =
    let
        created =
            ClientAnswer model.titleInput model.descriptionInput
    in
    div
        [ class "w-full flex flex-row items-center"
        , class "border-b active:shadow-inner hover:bg-gray-100"
        , class "py-3 pl-4"
        ]
        [ Input.input
            [ Event.onInput WriteNewTitle
            , Attribute.placeholder "✍️  New answer title..."
            , class "flex-grow"
            , class "mr-3"
            , Attribute.value model.titleInput
            ]
            []
        , Input.input
            [ Event.onInput WriteNewDescription
            , Attribute.placeholder "📄️  New answer description..."
            , class "flex-grow"
            , Attribute.value model.descriptionInput
            ]
            []
        , Html.button
            [ Event.onClick <| PerformCreate created
            , class "flex-end"
            , class "font-bold"
            , class "text-right px-8 text-seaside-600"
            ]
            [ Html.text "Create" ]
        ]


showAnswerList : Model -> Html Message
showAnswerList model =
    case model.state of
        Loading ->
            div [] [ Html.text "Loading" ]

        Loaded serverAnswers ->
            div [ class "flex-col" ] (List.map (\answer -> showAnswer answer) serverAnswers)

        Error viewer ->
            div [] [ Html.text "Error" ]


showAnswer : ServerAnswer -> Html Message
showAnswer answer =
    div
        [ class "flex flex-row"
        , class "border-b active:shadow-inner hover:bg-gray-100"
        , class "py-3 pl-4"
        ]
        [ div [ class "mx-3 ml-10" ] [ Html.text "▪️" ]
        , div [] [ Html.text answer.title ]
        , div [ class "px-1" ] [ Html.text ":" ]
        , div [] [ Html.text answer.description ]
        , div [ class "flex-grow" ] []
        , div [] [ editButton answer ]
        , div [] [ deleteButton answer ]
        ]


editButton : ServerAnswer -> Html Message
editButton answer =
    Html.button
        [ Attribute.class "text-gray-500 hover:text-red-500 capitalize font-archivo"
        , Attribute.class "text-right px-8"
        , Event.onClick <| PerformUpdate answer (ClientAnswer "hardcoded title" "hardcoded description")
        ]
        [ Html.text "✏️" ]


deleteButton : ServerAnswer -> Html Message
deleteButton answer =
    Html.button
        [ Attribute.class "text-gray-500 hover:text-red-500 capitalize font-archivo"
        , Attribute.class "text-right px-8"
        , Event.onClick <| PerformDelete answer
        ]
        [ Html.text "Delete" ]
