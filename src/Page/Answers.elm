module Page.Answers exposing
    ( Model, Message
    , update, view
    , init, subscriptions
    )

{-| A page sub-module that displays a question's answers


# TEA

@docs Model, Message
@docs update, view


# functions

@docs init, subscriptions

-}

import Api.Answers exposing (AnswerDiscriminator, ClientAnswer, ServerAnswer)
import Api.Questions exposing (QuestionDiscriminator, QuestionVisibility(..))
import Cmd.Extra exposing (withCmds, withNoCmd)
import Html exposing (Html, div, span)
import Html.Attributes as Attribute
import Html.Events as Event
import Html.Events.Extra exposing (onEnterDown)
import Page.Answers.Indices as Indices
import Picasso.Input as Input
import SelectableItemList exposing (SelectableItemList)
import Session exposing (Viewer)
import Task
import Task.Extra
import Time
import Tuple exposing (mapSecond)



-- MODEL


type alias AnswerData =
    { description : String
    , title : String
    }


withDescription : String -> AnswerData -> AnswerData
withDescription contents data =
    { data | description = contents }


withTitle : String -> AnswerData -> AnswerData
withTitle contents data =
    { data | title = contents }


type alias Model =
    { viewer : Viewer
    , question : QuestionDiscriminator
    , creation : Maybe AnswerData
    , answers : SelectableItemList ServerAnswer ( ServerAnswer, AnswerData )
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    ( { viewer = viewer
      , question = QuestionDiscriminator discriminator.idPoll discriminator.idQuestion
      , creation = Nothing
      , answers = SelectableItemList.empty
      }
    , Cmd.Extra.succeed PerformReload
    )



-- UPDATE


type Message
    = WriteCreateTitle String
    | WriteCreateDescription String
    | WriteModifyTitle String
    | WriteModifyDescription String
    | PerformReload
    | PerformStartCreate
    | PerformStartModify Int
    | PerformStopCreate
    | PerformStopModify
    | PerformCreate ClientAnswer
    | PerformUpdate ServerAnswer ClientAnswer
    | PerformDelete ServerAnswer
    | GotAnswerList (List ServerAnswer)
    | GotInvalidCredentials
    | GotError


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every (10 * 1000) (always PerformReload)


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        WriteCreateTitle text ->
            { model | creation = Maybe.map (withTitle text) model.creation }
                |> withNoCmd

        WriteCreateDescription text ->
            { model | creation = Maybe.map (withDescription text) model.creation }
                |> withNoCmd

        WriteModifyTitle text ->
            { model
                | answers =
                    SelectableItemList.map
                        identity
                        (mapSecond (withTitle text))
                        model.answers
            }
                |> withNoCmd

        WriteModifyDescription text ->
            { model
                | answers =
                    SelectableItemList.map identity
                        (mapSecond (withDescription text))
                        model.answers
            }
                |> withNoCmd

        PerformReload ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmds
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

        PerformStartCreate ->
            ( { model | creation = Just { title = "", description = "" } }, Cmd.none )

        PerformStopCreate ->
            ( { model | creation = Nothing }, Cmd.none )

        PerformStartModify index ->
            ( { model
                | answers =
                    SelectableItemList.select Tuple.first
                        (\answer ->
                            ( answer
                            , { title = answer.title
                              , description = answer.description
                              }
                            )
                        )
                        index
                        model.answers
              }
            , Cmd.none
            )

        PerformStopModify ->
            ( { model | answers = SelectableItemList.unselect Tuple.first model.answers }, Cmd.none )

        PerformCreate clientAnswer ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            { model | creation = Nothing }
                |> withCmds
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
            { model | answers = SelectableItemList.unselect Tuple.first model.answers }
                |> withCmds
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
                |> withCmds
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
            let
                -- We do not want to update the answers unless they have changed.
                newAnswers =
                    if SelectableItemList.toList identity Tuple.first model.answers == serverAnswerList then
                        model.answers

                    else
                        List.sortBy .idAnswer serverAnswerList |> SelectableItemList.fromList
            in
            { model | answers = newAnswers }
                |> withNoCmd

        GotInvalidCredentials ->
            { model | answers = SelectableItemList.empty }
                |> withNoCmd

        GotError ->
            { model | answers = SelectableItemList.empty }
                |> withNoCmd



-- VIEW


view : Model -> Html Message
view model =
    div
        [ Attribute.class "border-b bg-gray-100" ]
        [ answerHeader model
        , showAnswerList model
        ]


answerHeader : Model -> Html Message
answerHeader model =
    let
        text =
            case SelectableItemList.length model.answers of
                0 ->
                    "Press the \"Add Answer\" button to get started!"

                _ ->
                    "Here are the answers for this question!"
    in
    Maybe.map (\data -> newAnswerInput data) model.creation
        |> Maybe.withDefault
            (div
                [ Attribute.class "flex flex-row"
                ]
                [ span
                    [ Attribute.class "font-archivo font-semibold text-gray-600 p-4 pb-0" ]
                    [ Html.text text ]
                , div [ Attribute.class "flex-grow" ] []
                , newAnswerButton
                ]
            )


showAnswerList : Model -> Html Message
showAnswerList model =
    div [ Attribute.class "flex-col" ]
        (SelectableItemList.indexedMap
            (\index answer -> showAnswer index answer)
            (\_ ( answer, data ) -> modifyAnswerInput data.title data.description answer)
            model.answers
            |> SelectableItemList.flatten
        )


showAnswer : Int -> ServerAnswer -> Html Message
showAnswer index answer =
    div
        [ Attribute.class "flex flex-row items-center py-3 pl-4"
        , Attribute.class "font-archivo font-semibold text-gray-700"
        ]
        [ span [ Attribute.class "ml-10 text-gray-500" ] [ Html.text <| Indices.forIndex index ++ ".\u{00A0}" ]
        , span [] [ Html.text answer.title ]
        , span [ Attribute.class "text-gray-400" ] [ Html.text "\u{00A0}/\u{00A0}" ]
        , span [ Attribute.class "text-gray-500" ]
            [ Html.text <|
                if String.isEmpty answer.description then
                    "-"

                else
                    answer.description
            ]
        , div [ Attribute.class "flex-grow" ] []
        , div [] [ modifyAnswerButton index ]
        , div [] [ deleteAnswerButton answer ]
        ]


newAnswerInput : AnswerData -> Html Message
newAnswerInput data =
    let
        created =
            ClientAnswer data.title data.description
    in
    div
        [ Attribute.class "flex flex-row flex-wrap items-center"
        , Attribute.class "border-b active:shadow-inner bg-gray-100"
        , Attribute.class "py-3 px-4"
        ]
        [ Input.input
            [ Event.onInput WriteCreateTitle
            , onEnterDown <| PerformCreate created
            , Attribute.placeholder "✍️  New answer title..."
            , Attribute.autofocus True
            , Attribute.class "flex-grow"
            , Attribute.class "mr-3"
            , Attribute.value data.title
            ]
            []
        , Input.input
            [ Event.onInput WriteCreateDescription
            , onEnterDown <| PerformCreate created
            , Attribute.placeholder "📄️  New answer description..."
            , Attribute.class "flex flex-grow"
            , Attribute.class "mr-3"
            , Attribute.value data.description
            ]
            []
        , Html.div [ Attribute.class "flex flex-row items-center" ]
            [ Html.button
                [ Event.onClick PerformStopCreate
                , Attribute.class "flex flex-end"
                , Attribute.class "pr-3 font-bold text-gray-500 hover:text-gray-600"
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Event.onClick <| PerformCreate created
                , Attribute.class "flex flex-end"
                , Attribute.class "font-bold text-seaside-600 hover:text-seaside-700"
                ]
                [ Html.text "Create" ]
            ]
        ]


modifyAnswerInput : String -> String -> ServerAnswer -> Html Message
modifyAnswerInput title desc answer =
    let
        modified =
            ClientAnswer title desc
    in
    div
        [ Attribute.class "flex flex-row flex-wrap items-center"
        , Attribute.class "border-b active:shadow-inner bg-gray-100"
        , Attribute.class "py-3 px-4"
        ]
        [ Input.input
            [ Event.onInput WriteModifyTitle
            , onEnterDown <| PerformUpdate answer modified
            , Attribute.placeholder "✍️  Modify answer title..."
            , Attribute.autofocus True
            , Attribute.class "flex-grow"
            , Attribute.class "mr-3"
            , Attribute.value title
            ]
            []
        , Input.input
            [ Event.onInput WriteModifyDescription
            , onEnterDown <| PerformUpdate answer modified
            , Attribute.placeholder "📄️  Modify answer description..."
            , Attribute.class "flex-grow"
            , Attribute.class "mr-3"
            , Attribute.value desc
            ]
            []
        , Html.div [ Attribute.class "flex flex-row items-center" ]
            [ Html.button
                [ Event.onClick PerformStopModify
                , Attribute.class "flex-end"
                , Attribute.class "mr-3 font-bold text-gray-500 hover:text-gray-600"
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Event.onClick <| PerformUpdate answer modified
                , Attribute.class "flex-end"
                , Attribute.class "font-bold text-seaside-600 hover:text-seaside-700"
                ]
                [ Html.text "Apply" ]
            ]
        ]


newAnswerButton : Html Message
newAnswerButton =
    Html.button
        [ Event.onClick PerformStartCreate
        , Attribute.class "flex-end"
        , Attribute.class "font-bold"
        , Attribute.class "text-right px-8 text-seaside-600"
        ]
        [ Html.text "Add Answer" ]


modifyAnswerButton : Int -> Html Message
modifyAnswerButton index =
    Html.img
        [ Attribute.src "/icon/pencil.svg"
        , Attribute.class "h-6 w-6 cursor-pointer"
        , Event.onClick <| PerformStartModify index
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
