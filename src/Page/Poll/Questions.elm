module Page.Poll.Questions exposing
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
import Page.Question as Question
import Random
import Route
import Session exposing (Viewer)
import Task exposing (Task)
import Task.Extra



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
    , questions : Array ( ServerQuestion, Bool, Question.Model )
    , input : Maybe String
    , dragDrop : Html5.DragDrop.Model ServerQuestion DropIndex
    , seed : Random.Seed
    }


init : Viewer -> ServerPoll -> ( Model, Cmd Message )
init viewer poll =
    ( { viewer = viewer
      , poll = poll
      , questions = Array.empty
      , input = Nothing
      , dragDrop = Html5.DragDrop.init
      , seed = Random.initialSeed 42
      }
    , Cmd.succeed PerformReload
    )



-- UPDATE


type Message
    = WriteNewTitle String
    | PerformCreateStart
    | PerformCreate ClientQuestion
    | PerformDelete ServerQuestion
    | PerformExpand ServerQuestion
    | PerformUpdate ServerQuestion ClientQuestion
    | PerformMoveToIndex Int ServerQuestion
    | PerformReload
    | GotAllQuestions (List ServerQuestion)
    | GotBadCredentials
    | MsgQuestion ServerQuestion Question.Message
    | MsgDragDrop (Html5.DragDrop.Msg ServerQuestion DropIndex)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewTitle title ->
            ( { model | input = Maybe.map (always title) model.input }, Cmd.none )

        PerformCreateStart ->
            ( { model | input = Just "" }, Cmd.none )

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

        MsgQuestion identifier subMessage ->
            let
                ( questions, cmd ) =
                    updateQuestionList identifier subMessage model.questions
            in
            ( { model | questions = questions }, cmd )

        MsgDragDrop subMessage ->
            let
                ( updated, result ) =
                    Html5.DragDrop.update subMessage model.dragDrop

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
    -> Array ( ServerQuestion, Bool, Question.Model )
    -> List ServerQuestion
    -> ( Array ( ServerQuestion, Bool, Question.Model ), Cmd Message )
initQuestionList viewer existing list =
    let
        values : Array ( ( ServerQuestion, Bool, Question.Model ), Cmd Message )
        values =
            Array.fromList list
                |> Array.map
                    (\question ->
                        -- Reuse an existing model if it is found. Persist expanded states too.
                        let
                            previous : Maybe ( ServerQuestion, Bool, Question.Model )
                            previous =
                                Array.filter (\( id, _, _ ) -> question == id) existing
                                    |> Array.get 0

                            ( newModel, newCmd ) =
                                Question.init viewer question

                            ( model, expanded, command ) =
                                case previous of
                                    Just ( _, b, m ) ->
                                        ( m, b, Cmd.none )

                                    Nothing ->
                                        ( newModel, False, Cmd.map (MsgQuestion question) newCmd )
                        in
                        ( ( question, expanded, model ), command )
                    )

        models : Array ( ServerQuestion, Bool, Question.Model )
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
    -> Question.Message
    -> Array ( ServerQuestion, x, Question.Model )
    -> ( Array ( ServerQuestion, x, Question.Model ), Cmd Message )
updateQuestionList id message list =
    let
        values : Array ( ( ServerQuestion, x, Question.Model ), Cmd Message )
        values =
            Array.map
                (\( identifier, value, model ) ->
                    if identifier == id then
                        let
                            ( m, c ) =
                                Question.update message model
                        in
                        ( ( identifier, value, m ), Cmd.map (MsgQuestion identifier) c )

                    else
                        ( ( identifier, value, model ), Cmd.none )
                )
                list

        models : Array ( ServerQuestion, x, Question.Model )
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



-- VIEW


view : Model -> List (Html Message)
view model =
    []
        ++ viewInput model.input
        ++ viewQuestions model.dragDrop (List.map (\( a, b, _ ) -> ( a, b )) <| Array.toList model.questions)


viewInput : Maybe String -> List (Html Message)
viewInput current =
    case current of
        Just text ->
            let
                created =
                    { title = text
                    , details = ""
                    , visibility = Api.Questions.Visible
                    , index = 0.5
                    , answersMin = 0
                    , answersMax = 0
                    }
            in
            [ Html.input [ Attribute.value text, Event.onInput WriteNewTitle ] []
            , Html.button [ Event.onClick <| PerformCreate created ] [ Html.text "**save**" ]
            ]

        Nothing ->
            List.singleton <| Html.button [ Event.onClick PerformCreateStart ] [ Html.text "**new question**" ]


viewQuestions :
    Html5.DragDrop.Model ServerQuestion DropIndex
    -> List ( ServerQuestion, Bool )
    -> List (Html Message)
viewQuestions dragDropModel list =
    List.singleton <|
        Html.div [ Attribute.class "block align-middle mx-2 md:mx-8 mt-8 mb-32" ]
            [ Html.table
                [ Attribute.class "min-w-full center border rounded-lg overflow-hidden shadow"
                ]
                [ Html.thead
                    [ Attribute.class "bg-gray-100 border-b" ]
                    [ Html.tr []
                        [ Html.td
                            [ Attribute.class "font-bold font-archivo text-gray-500"
                            , Attribute.class "text-left tracking-wider"
                            , Attribute.class "border-gray-200 select-none py-3 px-6"
                            , Attribute.colspan 3
                            ]
                            [ Html.text "Title" ]
                        ]
                    ]
                , List.indexedMap (\i ( q, v ) -> ( i, q, v )) list
                    |> List.concatMap (\( i, q, v ) -> viewQuestion dragDropModel i q v)
                    |> Html.tbody [ Attribute.class "bg-white" ]
                ]
            ]


viewQuestion :
    Html5.DragDrop.Model ServerQuestion DropIndex
    -> Int
    -> ServerQuestion
    -> Bool
    -> List (Html Message)
viewQuestion dragDropModel index question expanded =
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
                                if id /= question && index == dropId then
                                    Just pos

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> Maybe.map
                    (\pos ->
                        if pos.y <= 0 || pos.y >= pos.height then
                            Attribute.class "border-b border-gray-200"

                        else if pos.y < pos.height // 2 then
                            Attribute.class "bg-seaside-050"

                        else
                            Attribute.class "bg-seaside-100"
                    )
                |> Maybe.withDefault (Attribute.class "border-b border-gray-200")

        expansion =
            if expanded then
                List.singleton <| viewQuestionExpansion question

            else
                []
    in
    [ Html.tr
        [ dropTargetStyling
        ]
        [ Html.td
            (List.concatMap identity
                [ List.singleton <| Attribute.class "flex flex-row items-center hover:bg-gray-100"
                , Html5.DragDrop.droppable MsgDragDrop index
                , Html5.DragDrop.draggable MsgDragDrop question
                ]
            )
            [ Html.div [ Attribute.class "font-bold font-archivo break-words py-3 px-4 flex-grow" ]
                [ Html.span [ Attribute.class "text-gray-500 mr-2" ] [ Html.text <| String.fromInt (index + 1) ++ "." ]
                , Html.text question.title
                ]
            , Html.div [] [ Html.button [ Event.onClick <| PerformExpand question ] [ Html.text "**expand**" ] ]
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


viewQuestionExpansion : ServerQuestion -> Html Message
viewQuestionExpansion question =
    Html.tr [] [ Html.td [ Attribute.colspan 3 ] [ Html.text <| "Expansion for : \"" ++ question.title ++ "\"" ] ]
