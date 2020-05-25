module Page.QuestionList exposing
    ( Model, Message
    , update, view
    , init, subscriptions
    )

{-| A page sub-module that displays a poll's questions,
and calls another submodule to display's a question's answers when necessary


# TEA

@docs Model, Message
@docs update, view


# functions

@docs init, subscriptions

-}

import Api.Polls exposing (ServerPoll)
import Api.Questions exposing (ClientQuestion, QuestionVisibility(..), ServerQuestion)
import Array exposing (Array)
import Cmd.Extra
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Html.Events.Extra exposing (onEnterDown)
import Html5.DragDrop
import Page.Answers as Answers
import Page.QuestionList.Visibility as Visibility exposing (Visibility)
import Page.QuestionStatistics as Statistics
import Picasso.FloatingButton
import Picasso.Input as Input
import Random
import Route
import SelectableItemList
import Session exposing (Viewer)
import Task exposing (Task)
import Task.Extra
import Time



-- MODEL


{-| An alias for the index at which a question will be dropped. If there are currently n questions
being displayed, the index will be contained between 0 (putting the question on the first
question) and n-1 (putting the question on the last question).
-}
type alias DropIndex =
    Int


{-| A type representing the current expansion status of a certain question. Expanded questions
display some details about answers, or some detailed statistics about the said question.
-}
type Expansion
    = Collapsed
    | ExpandedAnswers Answers.Model
    | ExpandedStatistics Statistics.Model


type alias Model =
    { viewer : Viewer
    , poll : ServerPoll
    , questions : Array ( ServerQuestion, Expansion )
    , visibility : Visibility
    , visibilityTrayOpen : Bool
    , input : Maybe ( String, QuestionVisibility )
    , modifying : Maybe ( ServerQuestion, ClientQuestion )
    , dragDrop : Html5.DragDrop.Model ServerQuestion DropIndex
    , seed : Random.Seed
    }


init : Viewer -> ServerPoll -> ( Model, Cmd Message )
init viewer poll =
    ( { viewer = viewer
      , poll = poll
      , questions = Array.empty
      , visibility = Visibility.Active
      , visibilityTrayOpen = False
      , input = Nothing
      , modifying = Nothing
      , dragDrop = Html5.DragDrop.init
      , seed = Random.initialSeed 42
      }
    , Cmd.batch
        [ Cmd.Extra.succeed PerformReload
        , cmdNewSeed
        ]
    )



-- UPDATE


type Message
    = WriteNewTitle String
    | WriteNewVisibility QuestionVisibility
    | WriteModify ServerQuestion ClientQuestion
    | SelectVisibility Visibility
    | SelectVisibilityTray
    | PerformCreateMode Bool
    | PerformModifyMode (Maybe ServerQuestion)
    | PerformCreate ClientQuestion
    | PerformDelete ServerQuestion
    | PerformExpandToAnswers ServerQuestion
    | PerformExpandToStatistics ServerQuestion
    | PerformUpdate ServerQuestion ClientQuestion
    | PerformMoveToIndex Int ServerQuestion
    | PerformReload
    | GotAllQuestions (List ServerQuestion)
    | GotNewSeed Random.Seed
    | GotBadCredentials
    | MsgAnswers ServerQuestion Answers.Message
    | MsgStatistics ServerQuestion Statistics.Message
    | MsgDragDrop (Html5.DragDrop.Msg ServerQuestion DropIndex)


subscriptions : Model -> Sub Message
subscriptions model =
    Array.toList model.questions
        |> List.filterMap
            (\( question, expansion ) ->
                case expansion of
                    Collapsed ->
                        Nothing

                    ExpandedAnswers m ->
                        Just <| Sub.map (MsgAnswers question) (Answers.subscriptions m)

                    ExpandedStatistics m ->
                        Just <| Sub.map (MsgStatistics question) (Statistics.subscriptions m)
            )
        |> Sub.batch


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewTitle title ->
            ( { model | input = Maybe.map (Tuple.mapFirst (always title)) model.input }, Cmd.none )

        WriteNewVisibility visibility ->
            ( { model | input = Maybe.map (Tuple.mapSecond (always visibility)) model.input }, Cmd.none )

        WriteModify serverQuestion clientQuestion ->
            ( { model | modifying = Just ( serverQuestion, clientQuestion ) }, Cmd.none )

        SelectVisibility visibility ->
            ( { model | visibility = visibility, visibilityTrayOpen = False }, Cmd.none )

        SelectVisibilityTray ->
            ( { model | visibilityTrayOpen = not model.visibilityTrayOpen }, Cmd.none )

        PerformCreateMode visible ->
            let
                state =
                    if visible then
                        Just ( "", Visible )

                    else
                        Nothing
            in
            ( { model | input = state }, Cmd.none )

        PerformModifyMode maybeQuestion ->
            case maybeQuestion of
                Just question ->
                    let
                        modified =
                            clientFromServer question
                    in
                    ( { model | modifying = Just ( question, modified ) }, Cmd.none )

                Nothing ->
                    ( { model | modifying = Nothing }, Cmd.none )

        PerformCreate question ->
            let
                lower =
                    0

                upper =
                    Array.get 0 model.questions
                        |> Maybe.map (\( q, _ ) -> q)
                        |> Maybe.map .index
                        |> Maybe.withDefault 1

                generator =
                    Random.float lower upper

                ( floatingIndex, seed ) =
                    Random.step generator model.seed

                updated =
                    { question | index = floatingIndex }
            in
            ( { model | input = Nothing, seed = seed }
            , taskCreate model.viewer model.poll updated
                |> thenHandleErrorAndReload
            )

        PerformDelete question ->
            ( model
            , taskDelete model.viewer question
                |> thenHandleErrorAndReload
            )

        PerformUpdate server client ->
            ( { model | modifying = Nothing }
            , taskUpdate model.viewer server client
                |> thenHandleErrorAndReload
            )

        PerformMoveToIndex index question ->
            let
                lower =
                    Array.get (index - 1) model.questions
                        |> Maybe.map (\( q, _ ) -> q)
                        |> Maybe.map .index
                        |> Maybe.withDefault 0

                upper =
                    Array.get index model.questions
                        |> Maybe.map (\( q, _ ) -> q)
                        |> Maybe.map .index
                        |> Maybe.withDefault 1

                generator =
                    Random.float lower upper

                ( floatingIndex, seed ) =
                    Random.step generator model.seed

                updated =
                    { title = question.title
                    , details = question.details
                    , visibility = question.visibility
                    , index = floatingIndex
                    , answersMin = question.answersMin
                    , answersMax = question.answersMax
                    }
            in
            ( { model | seed = seed }
            , taskUpdate model.viewer question updated |> thenHandleErrorAndReload
            )

        PerformExpandToAnswers question ->
            let
                ( questions, cmd ) =
                    expandToAnswers model.viewer question model.questions
            in
            ( { model | questions = questions }, cmd )

        PerformExpandToStatistics question ->
            let
                ( questions, cmd ) =
                    expandToStatistics model.viewer question model.questions
            in
            ( { model | questions = questions }, cmd )

        PerformReload ->
            ( model
            , taskAll model.viewer model.poll
                |> Task.mapError handleError
                |> Task.map GotAllQuestions
                |> Task.Extra.execute
            )

        GotBadCredentials ->
            ( model, cmdRouteToDisconnect model.viewer )

        GotAllQuestions retrieved ->
            let
                sorted =
                    List.sortBy .index retrieved

                questions =
                    initQuestionList model.questions sorted
            in
            ( { model | questions = questions }, Cmd.none )

        GotNewSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        MsgAnswers identifier subMessage ->
            let
                ( questions, cmd ) =
                    updateQuestionListWithAnswers
                        identifier
                        subMessage
                        model.questions
            in
            ( { model | questions = questions }, cmd )

        MsgStatistics identifier subMessage ->
            let
                ( questions, cmd ) =
                    updateQuestionListWithStatistics identifier
                        subMessage
                        model.questions
            in
            ( { model | questions = questions }, cmd )

        MsgDragDrop subMessage ->
            let
                ( updated, result ) =
                    Html5.DragDrop.updateSticky subMessage model.dragDrop

                cmd =
                    result
                        |> Maybe.map
                            (\( question, index, pos ) ->
                                if pos.y < pos.height // 2 then
                                    Cmd.Extra.succeed (PerformMoveToIndex index question)

                                else
                                    Cmd.Extra.succeed (PerformMoveToIndex (index + 1) question)
                            )
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | dragDrop = updated }, cmd )


thenReload : Task x a -> Task x Message
thenReload task =
    task
        |> Task.andThen (always <| Task.succeed PerformReload)


handleError : Api.Questions.QuestionError -> Message
handleError error =
    case error of
        Api.Questions.GotBadCredentials ->
            GotBadCredentials

        Api.Questions.GotBadNetwork ->
            PerformReload

        Api.Questions.GotNotFound ->
            PerformReload


thenHandleErrorAndReload : Task Api.Questions.QuestionError a -> Cmd Message
thenHandleErrorAndReload task =
    task
        |> Task.mapError handleError
        |> thenReload
        |> Task.Extra.execute


expandToAnswers :
    Viewer
    -> ServerQuestion
    -> Array ( ServerQuestion, Expansion )
    -> ( Array ( ServerQuestion, Expansion ), Cmd Message )
expandToAnswers viewer id list =
    let
        values : Array ( ( ServerQuestion, Expansion ), Cmd Message )
        values =
            Array.map
                (\( question, expansion ) ->
                    -- Do not expand non-matching views.
                    if id /= question then
                        ( ( question, expansion ), Cmd.none )

                    else
                        -- Reuse an existing model if it is found. Persist expanded states too.
                        let
                            ( newModel, newCmd ) =
                                Answers.init viewer question

                            ( expanded, command ) =
                                case expansion of
                                    ExpandedAnswers _ ->
                                        ( Collapsed, Cmd.none )

                                    ExpandedStatistics _ ->
                                        ( Collapsed, Cmd.none )

                                    Collapsed ->
                                        ( ExpandedAnswers newModel, Cmd.map (MsgAnswers question) newCmd )
                        in
                        ( ( question, expanded ), command )
                )
                list

        models : Array ( ServerQuestion, Expansion )
        models =
            Array.map Tuple.first values

        cmd : Cmd Message
        cmd =
            Array.map Tuple.second values
                |> Array.toList
                |> Cmd.batch
    in
    ( models, cmd )


expandToStatistics :
    Viewer
    -> ServerQuestion
    -> Array ( ServerQuestion, Expansion )
    -> ( Array ( ServerQuestion, Expansion ), Cmd Message )
expandToStatistics viewer id list =
    let
        values : Array ( ( ServerQuestion, Expansion ), Cmd Message )
        values =
            Array.map
                (\( question, expansion ) ->
                    -- Do not expand non-matching views.
                    if id /= question then
                        ( ( question, expansion ), Cmd.none )

                    else
                        -- Reuse an existing model if it is found. Persist expanded states too.
                        let
                            ( newModel, newCmd ) =
                                Statistics.init viewer question

                            ( otherMode, otherCmd ) =
                                Answers.init viewer question

                            ( expanded, command ) =
                                case expansion of
                                    ExpandedStatistics _ ->
                                        ( ExpandedAnswers otherMode, Cmd.map (MsgAnswers question) otherCmd )

                                    _ ->
                                        ( ExpandedStatistics newModel, Cmd.map (MsgStatistics question) newCmd )
                        in
                        ( ( question, expanded ), command )
                )
                list

        models : Array ( ServerQuestion, Expansion )
        models =
            Array.map Tuple.first values

        cmd : Cmd Message
        cmd =
            Array.map Tuple.second values
                |> Array.toList
                |> Cmd.batch
    in
    ( models, cmd )


initQuestionList :
    Array ( ServerQuestion, Expansion )
    -> List ServerQuestion
    -> Array ( ServerQuestion, Expansion )
initQuestionList existing list =
    Array.fromList list
        |> Array.map
            (\question ->
                let
                    expansion =
                        Array.filter (\( id, _ ) -> id.idQuestion == question.idQuestion) existing
                            |> Array.get 0
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault Collapsed
                in
                ( question, expansion )
            )


updateQuestionListWithAnswers :
    ServerQuestion
    -> Answers.Message
    -> Array ( ServerQuestion, Expansion )
    -> ( Array ( ServerQuestion, Expansion ), Cmd Message )
updateQuestionListWithAnswers id message list =
    let
        values : Array ( ( ServerQuestion, Expansion ), Cmd Message )
        values =
            Array.map
                (\( question, expansion ) ->
                    if id /= question then
                        ( ( question, expansion ), Cmd.none )

                    else
                        let
                            ( newExpansion, command ) =
                                case expansion of
                                    Collapsed ->
                                        ( Collapsed, Cmd.none )

                                    ExpandedAnswers model ->
                                        let
                                            ( m, c ) =
                                                Answers.update message model
                                        in
                                        ( ExpandedAnswers m, Cmd.map (MsgAnswers question) c )

                                    ExpandedStatistics statistics ->
                                        ( ExpandedStatistics statistics, Cmd.none )
                        in
                        ( ( question, newExpansion ), command )
                )
                list

        models : Array ( ServerQuestion, Expansion )
        models =
            Array.map Tuple.first values

        cmd : Cmd Message
        cmd =
            Array.map Tuple.second values
                |> Array.toList
                |> Cmd.batch
    in
    ( models, cmd )


updateQuestionListWithStatistics :
    ServerQuestion
    -> Statistics.Message
    -> Array ( ServerQuestion, Expansion )
    -> ( Array ( ServerQuestion, Expansion ), Cmd Message )
updateQuestionListWithStatistics id message list =
    let
        values : Array ( ( ServerQuestion, Expansion ), Cmd Message )
        values =
            Array.map
                (\( question, expansion ) ->
                    if id /= question then
                        ( ( question, expansion ), Cmd.none )

                    else
                        let
                            ( newExpansion, command ) =
                                case expansion of
                                    Collapsed ->
                                        ( Collapsed, Cmd.none )

                                    ExpandedAnswers statistics ->
                                        ( ExpandedAnswers statistics, Cmd.none )

                                    ExpandedStatistics model ->
                                        let
                                            ( m, c ) =
                                                Statistics.update message model
                                        in
                                        ( ExpandedStatistics m, Cmd.map (MsgStatistics question) c )
                        in
                        ( ( question, newExpansion ), command )
                )
                list

        models : Array ( ServerQuestion, Expansion )
        models =
            Array.map Tuple.first values

        cmd : Cmd Message
        cmd =
            Array.map Tuple.second values
                |> Array.toList
                |> Cmd.batch
    in
    ( models, cmd )



-- EFFECTS


cmdNewSeed : Cmd Message
cmdNewSeed =
    Time.now
        |> Task.map (Time.toMillis Time.utc)
        |> Task.map Random.initialSeed
        |> Task.map GotNewSeed
        |> Task.Extra.execute


cmdRouteToDisconnect : Viewer -> Cmd msg
cmdRouteToDisconnect viewer =
    Session.viewerNavKey viewer
        |> Route.badCredentials


taskCreate : Viewer -> ServerPoll -> ClientQuestion -> Task Api.Questions.QuestionError ServerQuestion
taskCreate viewer poll question =
    Api.Questions.create
        (Session.viewerCredentials viewer)
        poll.idPoll
        question
        identity


taskDelete : Viewer -> ServerQuestion -> Task Api.Questions.QuestionError ()
taskDelete viewer question =
    Api.Questions.delete
        (Session.viewerCredentials viewer)
        { idPoll = question.idPoll, idQuestion = question.idQuestion }
        ()


taskUpdate : Viewer -> ServerQuestion -> ClientQuestion -> Task Api.Questions.QuestionError ServerQuestion
taskUpdate viewer server client =
    Api.Questions.update
        (Session.viewerCredentials viewer)
        { idPoll = server.idPoll, idQuestion = server.idQuestion }
        client
        identity


taskAll : Viewer -> ServerPoll -> Task Api.Questions.QuestionError (List ServerQuestion)
taskAll viewer poll =
    Api.Questions.getQuestionList (Session.viewerCredentials viewer)
        poll.idPoll
        identity



-- UTILS


clientFromServer : ServerQuestion -> ClientQuestion
clientFromServer server =
    { title = server.title
    , details = server.details
    , index = server.index
    , answersMin = server.answersMin
    , answersMax = server.answersMax
    , visibility = server.visibility
    }



-- VIEW


view : Model -> List (Html Message)
view model =
    let
        button =
            Maybe.map (always []) model.input
                |> (Maybe.withDefault <|
                        List.singleton <|
                            Picasso.FloatingButton.button
                                [ Attribute.class "fixed right-0 bottom-0 m-8 z-50"
                                , Event.onClick <| PerformCreateMode True
                                ]
                                [ Html.img [ Attribute.src "/icon/action-button-plus.svg" ] []
                                , Html.div [ Attribute.class "ml-4" ] [ Html.text "New question" ]
                                ]
                   )
    in
    viewQuestions
        model
        |> List.append button


viewQuestions : Model -> List (Html Message)
viewQuestions model =
    let
        header =
            Maybe.map viewInput model.input
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    List.singleton <|
        Html.div [ Attribute.class "block align-middle mx-2 md:mx-8 mt-8 mb-8" ]
            [ Html.table
                [ Attribute.class "min-w-full bg-gray-100 center rounded-lg shadow"
                ]
                [ Html.thead
                    [ Attribute.class "rounded" ]
                    [ Html.tr []
                        [ Html.td
                            [ Attribute.class "font-bold font-archivo text-gray-500"
                            , Attribute.class "text-left tracking-wider"
                            , Attribute.class "border-gray-200 select-none py-3 px-6"
                            , Attribute.class "flex flex-row items-center border-b-2"
                            , Attribute.colspan 4
                            ]
                            [ Html.span [ Attribute.class "flex-grow" ] [ Html.text "Questions" ]
                            , Html.div
                                [ Attribute.class "relative"
                                ]
                                [ Html.img
                                    [ Attribute.src "/icon/filter-variant.svg"
                                    , Attribute.class "h-6 w-6 cursor-pointer"
                                    , Event.onClick SelectVisibilityTray
                                    ]
                                    []
                                , viewVisibilityTray model.visibilityTrayOpen model.visibility
                                ]
                            ]
                        ]
                    ]
                , Array.toList model.questions
                    |> List.filter (\( q, _ ) -> Visibility.display model.visibility q)
                    |> List.indexedMap (\i ( q, e ) -> ( i, ( q, e ) ))
                    |> List.concatMap (\( i, ( q, e ) ) -> viewQuestion model.dragDrop i model.visibility q model.modifying e)
                    |> viewNoQuestions (not <| List.isEmpty (Array.toList model.questions))
                    |> List.append header
                    |> Html.tbody [ Attribute.class "bg-white rounded-b overflow-hidden" ]
                , Html.div [ Attribute.class "h-4" ] []
                ]
            ]


viewNoQuestions : Bool -> List (Html Message) -> List (Html Message)
viewNoQuestions hasSomeQuestions maybeEmpty =
    case maybeEmpty of
        [] ->
            let
                contents =
                    if hasSomeQuestions then
                        "You've hidden all your questions ! You can change the view visibility in the upper right corner 👻"

                    else
                        "You have not added any question yet ! Press the NEW QUESTION button to do that now 👏"
            in
            [ Html.span [ Attribute.class "font-archivo font-semibold text-gray-600 p-4 block border-b" ] [ Html.text contents ] ]

        _ ->
            maybeEmpty


viewVisibilityTray : Bool -> Visibility -> Html Message
viewVisibilityTray visible selected =
    let
        row text visibility =
            let
                textColor =
                    if selected == visibility then
                        Attribute.class "text-seaside-500"

                    else
                        Attribute.class "text-black"
            in
            Html.button
                [ Attribute.class "px-4 py-2 block font-semibold hover:bg-seaside-100 w-full"
                , textColor
                , Event.onClick <| SelectVisibility visibility
                ]
                [ Html.text text ]

        menuAnimation =
            if not visible then
                Attribute.class "invisible opacity-0 scale-95"

            else
                Attribute.class "block opacity-100 scale-100"
    in
    Html.div
        [ menuAnimation
        , Attribute.class "transform duration-200 origin-top-right"
        , Attribute.class "shadow-xl w-48 z-10 bg-white rounded-lg overflow-hidden"
        , Attribute.class "absolute right-0 mt-2 border-2 border-seaside-050"
        ]
        [ row "Visible + Hidden" Visibility.Active
        , row "Archived" Visibility.Archived
        , row "All" Visibility.All
        ]


viewQuestion :
    Html5.DragDrop.Model ServerQuestion DropIndex
    -> Int
    -> Visibility
    -> ServerQuestion
    -> Maybe ( ServerQuestion, ClientQuestion )
    -> Expansion
    -> List (Html Message)
viewQuestion dragDropModel index visibility question modifying expansion =
    let
        -- Style the upper or the lower border of the cell with the right color.
        dropTargetStyling : Html.Attribute msg
        dropTargetStyling =
            Html5.DragDrop.getDragId dragDropModel
                -- Get the identifier and the position.
                |> Maybe.andThen
                    (\id ->
                        Html5.DragDrop.getDroppablePosition dragDropModel
                            |> Maybe.map (\p -> ( id, p ))
                    )
                -- Filter by index.
                |> Maybe.andThen
                    (\( _, pos ) ->
                        case Html5.DragDrop.getDropId dragDropModel of
                            Just dropId ->
                                if index == dropId || index + 1 == dropId then
                                    Just ( dropId, pos )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> Maybe.map
                    (\( dropId, pos ) ->
                        if pos.y <= 0 || pos.y >= pos.height then
                            Attribute.class "border-b-2 border-gray-200"

                        else if pos.y > pos.height // 2 && index == dropId then
                            Attribute.class "border-b-2 border-seaside-800"

                        else if pos.y < pos.height // 2 && index + 1 == dropId then
                            Attribute.class "border-b-2 border-seaside-800"

                        else
                            Attribute.class "border-b-2 border-gray-200"
                    )
                |> Maybe.withDefault (Attribute.class "border-b-2 border-gray-200")

        content =
            case expansion of
                Collapsed ->
                    []

                ExpandedAnswers model ->
                    [ viewQuestionDetails modifying (DetailsAnswers model) visibility question
                    , viewQuestionAnswers question model
                    ]

                ExpandedStatistics model ->
                    [ viewQuestionDetails modifying DetailsStatistics visibility question
                    , viewQuestionStatistics question model
                    ]

        expansionStyling =
            case expansion of
                Collapsed ->
                    Attribute.class "transform duration-200"

                ExpandedAnswers _ ->
                    Attribute.class "transform duration-200 rotate-90"

                ExpandedStatistics _ ->
                    Attribute.class "transform duration-200 rotate-90"
    in
    Html.tr
        [ dropTargetStyling
        , Attribute.class "hover:bg-gray-100"
        ]
        [ Html.td
            (List.concatMap identity
                [ List.singleton <| Attribute.class "flex flex-row items-center"
                , Html5.DragDrop.droppable MsgDragDrop index
                , Html5.DragDrop.draggable MsgDragDrop question
                ]
            )
            [ Html.img [ Attribute.class "ml-4 h-6 w-6 hidden md:block", Attribute.src "/icon/drag-horizontal-variant.svg" ] []
            , Html.div
                [ Attribute.class "font-bold font-archivo break-words py-3 px-4 flex-grow flex flex-row items-center"
                , Event.onClick <| PerformExpandToAnswers question
                ]
                [ Html.span [ Attribute.class "text-gray-500 mr-2" ] [ Html.text <| String.fromInt (index + 1) ++ "." ]
                , Html.span
                    [ if question.visibility /= Api.Questions.Visible then
                        Attribute.class "text-gray-500"

                      else
                        Attribute.class "text-black"
                    ]
                    [ Html.text question.title ]
                , if question.visibility /= Api.Questions.Visible then
                    Html.img [ Attribute.src "/icon/visibility-hide.svg", Attribute.class "h-4 w-4 mx-2" ] []

                  else
                    Html.div [] []
                ]
            , Html.img
                [ Attribute.src "/icon/chevron-right.svg"
                , Attribute.class "w-6 h-6 flex-none"
                , expansionStyling
                , Event.onClick <| PerformExpandToAnswers question
                ]
                []
            , Html.div []
                [ Html.button
                    [ Attribute.class "text-gray-500 hover:text-red-500 capitalize font-archivo"
                    , Attribute.class "text-right px-8"
                    , Event.onClick <| PerformDelete question
                    ]
                    [ Html.text "Delete" ]
                ]
            ]
        ]
        :: content


type DetailsMode
    = DetailsStatistics
    | DetailsAnswers Answers.Model


viewQuestionDetails : Maybe ( ServerQuestion, ClientQuestion ) -> DetailsMode -> Visibility -> ServerQuestion -> Html Message
viewQuestionDetails maybeModifying mode _ question =
    let
        modifyQuestionInputs =
            case maybeModifying of
                Nothing ->
                    []

                Just ( serverQuestion, clientQuestion ) ->
                    let
                        modifyClientTitle string =
                            WriteModify serverQuestion { clientQuestion | title = string }

                        modifyClientDetails string =
                            WriteModify serverQuestion { clientQuestion | details = string }
                    in
                    if serverQuestion.idQuestion == question.idQuestion then
                        [ Html.div
                            [ Attribute.class "flex flex-row flex-grow flex-wrap items-center"
                            , Attribute.class "border-b active:shadow-inner bg-gray-100"
                            , Attribute.class "py-3 px-4"
                            ]
                            [ Input.input
                                [ Event.onInput modifyClientTitle
                                , onEnterDown <| PerformUpdate serverQuestion clientQuestion
                                , Attribute.placeholder "✍️  Modify question title..."
                                , Attribute.autofocus True
                                , Attribute.class "flex flex-grow mr-3"
                                , Attribute.value clientQuestion.title
                                ]
                                []
                            , Input.input
                                [ Event.onInput modifyClientDetails
                                , onEnterDown <| PerformUpdate serverQuestion clientQuestion
                                , Attribute.placeholder "📄️  Modify question details..."
                                , Attribute.class "flex flex-grow mr-3"
                                , Attribute.value clientQuestion.details
                                ]
                                []
                            , Html.div [ Attribute.class "flex flex-row items-center" ]
                                [ Html.button
                                    [ Event.onClick <| PerformModifyMode Nothing
                                    , Attribute.class "flex flex-end"
                                    , Attribute.class "pr-3 font-bold text-gray-500 hover:text-gray-600"
                                    ]
                                    [ Html.text "Discard" ]
                                , Html.button
                                    [ Event.onClick <| PerformUpdate serverQuestion clientQuestion
                                    , Attribute.class "flex flex-end"
                                    , Attribute.class "font-bold text-seaside-600 hover:text-seaside-700"
                                    ]
                                    [ Html.text "Apply" ]
                                ]
                            ]
                        ]

                    else
                        []

        actionIconsAttributes =
            let
                client =
                    clientFromServer question

                animation =
                    Attribute.class "transform duration-200 hover:scale-110"

                statistics =
                    case mode of
                        DetailsStatistics ->
                            [ Attribute.src "/icon/statistics-hide.svg"
                            , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                            , animation
                            , Event.onClick <| PerformExpandToStatistics question
                            ]

                        DetailsAnswers _ ->
                            [ Attribute.src "/icon/statistics-show.svg"
                            , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                            , animation
                            , Event.onClick <| PerformExpandToStatistics question
                            ]

                modifyButton =
                    case mode of
                        DetailsAnswers _ ->
                            [ Attribute.src "/icon/pencil.svg"
                            , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                            , animation
                            , Event.onClick <| PerformModifyMode (Just question)
                            ]

                        DetailsStatistics ->
                            []
            in
            modifyButton
                :: (case question.visibility of
                        Api.Questions.Visible ->
                            [ statistics
                            , [ Attribute.src "/icon/visibility-hide.svg"
                              , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                              , animation
                              , Event.onClick <|
                                    PerformUpdate question { client | visibility = Api.Questions.Hidden }
                              ]
                            , [ Attribute.src "/icon/visibility-archive.svg"
                              , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                              , animation
                              , Event.onClick <|
                                    PerformUpdate question { client | visibility = Api.Questions.Archived }
                              ]
                            ]

                        Api.Questions.Archived ->
                            [ statistics
                            , [ Attribute.src "/icon/visibility-unarchive.svg"
                              , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                              , animation
                              , Event.onClick <|
                                    PerformUpdate question { client | visibility = Api.Questions.Visible }
                              ]
                            ]

                        Api.Questions.Hidden ->
                            [ statistics
                            , [ Attribute.src "/icon/visibility-show.svg"
                              , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                              , animation
                              , Event.onClick <|
                                    PerformUpdate question { client | visibility = Api.Questions.Visible }
                              ]
                            , [ Attribute.src "/icon/visibility-archive.svg"
                              , Attribute.class "w-6 h-6 m-4 cursor-pointer"
                              , animation
                              , Event.onClick <|
                                    PerformUpdate question { client | visibility = Api.Questions.Archived }
                              ]
                            ]
                   )

        modifyingOrActionInfo =
            let
                dropdown =
                    let
                        questionInfo : Answers.Model -> List (Html Message)
                        questionInfo model =
                            let
                                clientQuestion =
                                    clientFromServer question

                                modifyClientMin : String -> Message
                                modifyClientMin s =
                                    let
                                        n =
                                            Maybe.withDefault 0 <| String.toInt s
                                    in
                                    PerformUpdate question { clientQuestion | answersMin = n }

                                modifyClientMax : String -> Message
                                modifyClientMax s =
                                    let
                                        n =
                                            Maybe.withDefault 0 <| String.toInt s
                                    in
                                    PerformUpdate question { clientQuestion | answersMax = n }

                                questionDetails =
                                    if String.isEmpty question.details then
                                        "Nothing yet, modify the question to specify details!"

                                    else
                                        question.details

                                selectGenerator : Int -> List (Html Message)
                                selectGenerator minMax =
                                    let
                                        intToOption n =
                                            let
                                                ifSelected x =
                                                    if x == minMax then
                                                        [ Attribute.selected True ]

                                                    else
                                                        []

                                                notSelect =
                                                    case n of
                                                        0 ->
                                                            ( [ Attribute.value "0" ], [ Html.text "None" ] )

                                                        any ->
                                                            ( [], [ Html.text <| String.fromInt any ] )
                                            in
                                            (\( attrs, contents ) -> Html.option (attrs ++ ifSelected n) contents) notSelect

                                        numAns =
                                            SelectableItemList.length model.answers

                                        optionRange =
                                            List.range 0 numAns
                                    in
                                    List.map intToOption optionRange
                            in
                            [ Html.div
                                [ Attribute.class "flex flex-col" ]
                                [ Html.div
                                    [ Attribute.class "mt-3 ml-4" ]
                                    [ Html.span [ Attribute.class "font-archivo font-semibold text-gray-600" ] [ Html.text <| "Details : " ]
                                    , Html.text questionDetails
                                    ]
                                , Html.div
                                    [ Attribute.class "mt-3 ml-4" ]
                                    [ Html.span [ Attribute.class "font-archivo font-semibold text-gray-600" ] [ Html.text <| "Min answers : " ]
                                    , Html.select
                                        [ Attribute.class "ml-1 px-1 py-1 appearance-none bg-white border border-gray-400 hover:border-gray-500 rounded shadow leading-tight focus:shadow-outline"
                                        , Event.onInput <| modifyClientMin
                                        ]
                                        (selectGenerator question.answersMin)
                                    ]
                                , Html.div
                                    [ Attribute.class "mt-3 ml-4" ]
                                    [ Html.span [ Attribute.class "font-archivo font-semibold text-gray-600" ] [ Html.text <| "Max answers : " ]
                                    , Html.select
                                        [ Attribute.class "px-1 py-1 appearance-none bg-white border border-gray-400 hover:border-gray-500 rounded shadow leading-tight focus:shadow-outline"
                                        , Event.onInput <| modifyClientMax
                                        ]
                                        (selectGenerator question.answersMax)
                                    ]
                                ]
                            , Html.div [ Attribute.class "flex-grow" ] []
                            ]

                        actionsIcons =
                            [ Html.div [ Attribute.class "flex flex-wrap" ] (List.map (\attrs -> Html.img attrs []) actionIconsAttributes) ]
                    in
                    case mode of
                        DetailsAnswers model ->
                            questionInfo model ++ actionsIcons

                        DetailsStatistics ->
                            actionsIcons
            in
            case maybeModifying of
                Nothing ->
                    dropdown

                Just ( serverAnswer, _ ) ->
                    if serverAnswer.idQuestion == question.idQuestion then
                        modifyQuestionInputs

                    else
                        dropdown
    in
    Html.tr
        [ Attribute.class "flex flex-row bg-gray-100 justify-end pr-2" ]
        modifyingOrActionInfo


viewQuestionAnswers : ServerQuestion -> Answers.Model -> Html Message
viewQuestionAnswers question model =
    Answers.view model
        |> Html.map (\msg -> MsgAnswers question msg)


viewQuestionStatistics : ServerQuestion -> Statistics.Model -> Html Message
viewQuestionStatistics question model =
    Statistics.view model
        |> Html.map (\msg -> MsgStatistics question msg)


viewInput : ( String, QuestionVisibility ) -> Html Message
viewInput ( current, visibility ) =
    let
        created =
            { title = current
            , details = ""
            , visibility = visibility
            , index = 0.5
            , answersMin = 0
            , answersMax = 0
            }

        text =
            case visibility of
                Visible ->
                    "🚀 New visible question..."

                _ ->
                    "🚀 New hidden question..."

        switchAttributes =
            case visibility of
                Visible ->
                    [ Attribute.class "h-8 w-8 hover:scale-110 transform duration-200 ml-8 cursor-pointer"
                    , Attribute.src "/icon/visibility-hide.svg"
                    , Event.onClick <| WriteNewVisibility Hidden
                    ]

                _ ->
                    [ Attribute.class "h-8 w-8 hover:scale-110 transform duration-200 ml-8 cursor-pointer"
                    , Attribute.src "/icon/visibility-show.svg"
                    , Event.onClick <| WriteNewVisibility Visible
                    ]
    in
    Html.tr [ Attribute.class "border-b active:shadow-inner" ]
        [ Html.td
            [ Attribute.class "py-3 pl-4", Attribute.class "w-full flex flex-row items-center" ]
            [ Input.input
                [ Event.onInput WriteNewTitle
                , onEnterDown <| PerformCreate created
                , Attribute.placeholder text
                , Attribute.autofocus True
                , Attribute.class "w-full"
                , Attribute.value current
                ]
                []
            , Html.img switchAttributes []
            , Html.button
                [ Event.onClick <| PerformCreateMode False
                , Attribute.class "font-bold"
                , Attribute.class "text-right pl-8 text-gray-500 hover:text-gray-600"
                ]
                [ Html.text "Discard" ]
            , Html.button
                [ Event.onClick <| PerformCreate created
                , Attribute.class "font-bold"
                , Attribute.class "text-right px-8 text-seaside-600 hover:text-seaside-700"
                ]
                [ Html.text "Create" ]
            ]
        ]
