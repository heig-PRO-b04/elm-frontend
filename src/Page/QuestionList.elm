module Page.QuestionList exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (ServerPoll)
import Api.Questions exposing (ClientQuestion, ServerQuestion)
import Array exposing (Array)
import Cmd
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Html5.DragDrop
import Page.Answers as Answers
import Page.QuestionList.Visibility as Visibility exposing (Visibility)
import Picasso.FloatingButton
import Picasso.Input as Input
import Random
import Route
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


type alias Model =
    { viewer : Viewer
    , poll : ServerPoll
    , questions : Array ( ServerQuestion, Bool, Answers.Model )
    , visibility : Visibility
    , visibilityTrayOpen : Bool
    , input : Maybe String
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
      , dragDrop = Html5.DragDrop.init
      , seed = Random.initialSeed 42
      }
    , Cmd.batch
        [ Cmd.succeed PerformReload
        , cmdNewSeed
        ]
    )



-- UPDATE


type Message
    = WriteNewTitle String
    | SelectVisibility Visibility
    | SelectVisibilityTray
    | PerformCreateMode Bool
    | PerformCreate ClientQuestion
    | PerformDelete ServerQuestion
    | PerformExpand ServerQuestion
    | PerformUpdate ServerQuestion ClientQuestion
    | PerformMoveToIndex Int ServerQuestion
    | PerformReload
    | GotAllQuestions (List ServerQuestion)
    | GotNewSeed Random.Seed
    | GotBadCredentials
    | MsgQuestion ServerQuestion Answers.Message
    | MsgDragDrop (Html5.DragDrop.Msg ServerQuestion DropIndex)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewTitle title ->
            ( { model | input = Maybe.map (always title) model.input }, Cmd.none )

        SelectVisibility visibility ->
            ( { model | visibility = visibility, visibilityTrayOpen = False }, Cmd.none )

        SelectVisibilityTray ->
            ( { model | visibilityTrayOpen = not model.visibilityTrayOpen }, Cmd.none )

        PerformCreateMode visible ->
            let
                state =
                    if visible then
                        Just ""

                    else
                        Nothing
            in
            ( { model | input = state }, Cmd.none )

        PerformCreate question ->
            let
                lower =
                    Array.get (Array.length model.questions - 1) model.questions
                        |> Maybe.map (\( q, _, _ ) -> q)
                        |> Maybe.map .index
                        |> Maybe.withDefault 0

                upper =
                    Array.get (Array.length model.questions) model.questions
                        |> Maybe.map (\( q, _, _ ) -> q)
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
            ( model
            , taskUpdate model.viewer server client
                |> thenHandleErrorAndReload
            )

        PerformMoveToIndex index question ->
            let
                lower =
                    Array.get (index - 1) model.questions
                        |> Maybe.map (\( q, _, _ ) -> q)
                        |> Maybe.map .index
                        |> Maybe.withDefault 0

                upper =
                    Array.get index model.questions
                        |> Maybe.map (\( q, _, _ ) -> q)
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

        PerformExpand question ->
            ( { model | questions = expand question model.questions }, Cmd.none )

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

                ( questions, cmd ) =
                    initQuestionList model.viewer model.questions sorted
            in
            ( { model | questions = questions }, cmd )

        GotNewSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        MsgQuestion identifier subMessage ->
            let
                ( questions, cmd ) =
                    updateQuestionList identifier subMessage model.questions
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
                                    Cmd.succeed (PerformMoveToIndex index question)

                                else
                                    Cmd.succeed (PerformMoveToIndex (index + 1) question)
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

        -- TODO : Display an error message ?
        Api.Questions.GotBadNetwork ->
            PerformReload

        -- TODO : Quit instead ?
        Api.Questions.GotNotFound ->
            PerformReload


thenHandleErrorAndReload : Task Api.Questions.QuestionError a -> Cmd Message
thenHandleErrorAndReload task =
    task
        |> Task.mapError handleError
        |> thenReload
        |> Task.Extra.execute


expand : identifier -> Array ( identifier, Bool, model ) -> Array ( identifier, Bool, model )
expand id list =
    Array.map
        (\( identifier, value, model ) ->
            if identifier == id then
                ( identifier, not value, model )

            else
                ( identifier, value, model )
        )
        list


initQuestionList :
    Viewer
    -> Array ( ServerQuestion, Bool, Answers.Model )
    -> List ServerQuestion
    -> ( Array ( ServerQuestion, Bool, Answers.Model ), Cmd Message )
initQuestionList viewer existing list =
    let
        values : Array ( ( ServerQuestion, Bool, Answers.Model ), Cmd Message )
        values =
            Array.fromList list
                |> Array.map
                    (\question ->
                        -- Reuse an existing model if it is found. Persist expanded states too.
                        let
                            previous : Maybe ( ServerQuestion, Bool, Answers.Model )
                            previous =
                                Array.filter (\( id, _, _ ) -> question == id) existing
                                    |> Array.get 0

                            ( newModel, newCmd ) =
                                Answers.init viewer question

                            ( model, expanded, command ) =
                                case previous of
                                    Just ( _, b, m ) ->
                                        ( m, b, Cmd.none )

                                    Nothing ->
                                        ( newModel, False, Cmd.map (MsgQuestion question) newCmd )
                        in
                        ( ( question, expanded, model ), command )
                    )

        models : Array ( ServerQuestion, Bool, Answers.Model )
        models =
            Array.map Tuple.first values

        cmd : Cmd Message
        cmd =
            Array.map Tuple.second values
                |> Array.toList
                |> Cmd.batch
    in
    ( models, cmd )


updateQuestionList :
    ServerQuestion
    -> Answers.Message
    -> Array ( ServerQuestion, x, Answers.Model )
    -> ( Array ( ServerQuestion, x, Answers.Model ), Cmd Message )
updateQuestionList id message list =
    let
        values : Array ( ( ServerQuestion, x, Answers.Model ), Cmd Message )
        values =
            Array.map
                (\( identifier, value, model ) ->
                    if identifier == id then
                        let
                            ( m, c ) =
                                Answers.update message model
                        in
                        ( ( identifier, value, m ), Cmd.map (MsgQuestion identifier) c )

                    else
                        ( ( identifier, value, model ), Cmd.none )
                )
                list

        models : Array ( ServerQuestion, x, Answers.Model )
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
        { idPoll = poll.idPoll }
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
        { idPoll = poll.idPoll }
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
                                [ Attribute.class "fixed right-0 bottom-0 m-8"
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
                            [ Html.span [ Attribute.class "flex-grow" ] [ Html.text "Title" ]
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
                    |> List.filter (\( q, _, _ ) -> Visibility.display model.visibility q)
                    |> List.indexedMap (\i ( q, v, m ) -> ( i, ( q, v, m ) ))
                    |> List.concatMap (\( i, ( q, v, m ) ) -> viewQuestion model.dragDrop i model.visibility q v m)
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
    in
    if not visible then
        Html.div [] []

    else
        Html.div
            [ Attribute.class "block absolute shadow-xl bg-white right-0 w-48 z-10 mt-2 rounded-lg overflow-hidden border-2 border-seaside-050" ]
            [ row "Visible + Hidden" Visibility.Active
            , row "Archived" Visibility.Archived
            , row "All" Visibility.All
            ]


viewQuestion :
    Html5.DragDrop.Model ServerQuestion DropIndex
    -> Int
    -> Visibility
    -> ServerQuestion
    -> Bool
    -> Answers.Model
    -> List (Html Message)
viewQuestion dragDropModel index visibility question expanded model =
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
                    (\( id, pos ) ->
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

        expansion =
            if expanded then
                [ viewQuestionDetails visibility question
                , viewQuestionExpansion question model
                ]

            else
                []

        expansionStyling =
            if expanded then
                Attribute.class "transform duration-200 rotate-90"

            else
                Attribute.class "transform duration-200"
    in
    [ Html.tr
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
                [ Attribute.class "font-bold font-archivo break-words py-3 px-4 flex-grow"
                , Event.onClick <| PerformExpand question
                ]
                [ Html.span [ Attribute.class "text-gray-500 mr-2" ] [ Html.text <| String.fromInt (index + 1) ++ "." ]
                , Html.text question.title
                ]
            , Html.div []
                [ Html.img
                    [ Attribute.src "/icon/chevron-right.svg"
                    , expansionStyling
                    , Event.onClick <| PerformExpand question
                    ]
                    []
                ]
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
    ]
        ++ expansion


viewQuestionDetails : Visibility -> ServerQuestion -> Html Message
viewQuestionDetails visibility question =
    let
        actionsAndIcons =
            let
                client =
                    clientFromServer question
            in
            case question.visibility of
                Api.Questions.Visible ->
                    [ [ Attribute.src "/icon/visibility-hide.svg"
                      , Attribute.class "w-6 h-6 m-4"
                      , Event.onClick <|
                            PerformUpdate question { client | visibility = Api.Questions.Hidden }
                      ]
                    , [ Attribute.src "/icon/visibility-archive.svg"
                      , Attribute.class "w-6 h-6 m-4"
                      , Event.onClick <|
                            PerformUpdate question { client | visibility = Api.Questions.Archived }
                      ]
                    ]

                Api.Questions.Archived ->
                    [ [ Attribute.src "/icon/visibility-unarchive.svg"
                      , Attribute.class "w-6 h-6 m-4"
                      , Event.onClick <|
                            PerformUpdate question { client | visibility = Api.Questions.Visible }
                      ]
                    ]

                Api.Questions.Hidden ->
                    [ [ Attribute.src "/icon/visibility-show.svg"
                      , Attribute.class "w-6 h-6 m-4"
                      , Event.onClick <|
                            PerformUpdate question { client | visibility = Api.Questions.Visible }
                      ]
                    , [ Attribute.src "/icon/visibility-archive.svg"
                      , Attribute.class "w-6 h-6 m-4"
                      , Event.onClick <|
                            PerformUpdate question { client | visibility = Api.Questions.Archived }
                      ]
                    ]
    in
    Html.tr
        [ Attribute.class "flex flex-row bg-gray-100 justify-end pr-2" ]
        (List.map (\attrs -> Html.img attrs []) actionsAndIcons)


viewQuestionExpansion : ServerQuestion -> Answers.Model -> Html Message
viewQuestionExpansion question model =
    Answers.view model
        |> Html.map (\msg -> MsgQuestion question msg)


viewInput : String -> Html Message
viewInput current =
    let
        created =
            { title = current
            , details = ""
            , visibility = Api.Questions.Visible
            , index = 0.5
            , answersMin = 0
            , answersMax = 0
            }
    in
    Html.tr [ Attribute.class "border-b active:shadow-inner" ]
        [ Html.td
            [ Attribute.class "py-3 pl-4", Attribute.class "w-full flex flex-row items-center" ]
            [ Input.input
                [ Event.onInput WriteNewTitle
                , Attribute.placeholder "🚀 New question..."
                , Attribute.class "w-full"
                , Attribute.value current
                ]
                []
            , Html.button
                [ Event.onClick <| PerformCreateMode False
                , Attribute.class "font-bold"
                , Attribute.class "text-right pl-8 text-gray-500 hover:text-gray-600"
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Event.onClick <| PerformCreate created
                , Attribute.class "font-bold"
                , Attribute.class "text-right px-8 text-seaside-600 hover:text-seaside-700"
                ]
                [ Html.text "Create" ]
            ]
        ]
