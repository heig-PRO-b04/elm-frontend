module Page.Poll.Questions exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (ServerPoll)
import Api.Questions exposing (ClientQuestion, ServerQuestion)
import Cmd
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Page.Question as Question
import Route
import Session exposing (Viewer)
import Task exposing (Task)
import Task.Extra



-- MODEL


type alias Model =
    { viewer : Viewer
    , poll : ServerPoll
    , questions : List ( ServerQuestion, Bool, Question.Model )
    , input : Maybe String
    }


init : Viewer -> ServerPoll -> ( Model, Cmd Message )
init viewer poll =
    ( Model viewer poll [] Nothing, Cmd.succeed PerformReload )



-- UPDATE


type Message
    = WriteNewTitle String
    | PerformCreateStart
    | PerformCreate ClientQuestion
    | PerformDelete ServerQuestion
    | PerformExpand ServerQuestion
    | PerformUpdate ServerQuestion ClientQuestion
    | PerformReload
    | GotAllQuestions (List ServerQuestion)
    | GotBadCredentials
    | MsgQuestion ServerQuestion Question.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewTitle title ->
            ( { model | input = Maybe.map (always title) model.input }, Cmd.none )

        PerformCreateStart ->
            ( { model | input = Just "" }, Cmd.none )

        PerformCreate question ->
            ( { model | input = Nothing }
            , taskCreate model.viewer model.poll question
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
                ( questions, cmd ) =
                    initQuestionList model.viewer model.questions retrieved
            in
            ( { model | questions = questions }, cmd )

        MsgQuestion identifier subMessage ->
            let
                ( questions, cmd ) =
                    updateQuestionList identifier subMessage model.questions
            in
            ( { model | questions = questions }, cmd )


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


expand : identifier -> List ( identifier, Bool, model ) -> List ( identifier, Bool, model )
expand id list =
    List.map
        (\( identifier, value, model ) ->
            if identifier == id then
                ( identifier, not value, model )

            else
                ( identifier, value, model )
        )
        list


initQuestionList :
    Viewer
    -> List ( ServerQuestion, Bool, Question.Model )
    -> List ServerQuestion
    -> ( List ( ServerQuestion, Bool, Question.Model ), Cmd Message )
initQuestionList viewer existing list =
    let
        values : List ( ( ServerQuestion, Bool, Question.Model ), Cmd Message )
        values =
            List.map
                (\question ->
                    -- Reuse an existing model if it is found. Persist expanded states too.
                    let
                        previous : Maybe ( ServerQuestion, Bool, Question.Model )
                        previous =
                            List.filter (\( id, _, _ ) -> question == id) existing
                                |> List.head

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
                list

        models : List ( ServerQuestion, Bool, Question.Model )
        models =
            List.map Tuple.first values

        cmd : Cmd Message
        cmd =
            List.map Tuple.second values
                |> Cmd.batch
    in
    ( models, cmd )


updateQuestionList :
    ServerQuestion
    -> Question.Message
    -> List ( ServerQuestion, x, Question.Model )
    -> ( List ( ServerQuestion, x, Question.Model ), Cmd Message )
updateQuestionList id message list =
    let
        values : List ( ( ServerQuestion, x, Question.Model ), Cmd Message )
        values =
            List.map
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

        models : List ( ServerQuestion, x, Question.Model )
        models =
            List.map Tuple.first values

        cmd : Cmd Message
        cmd =
            List.map Tuple.second values
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
        ++ viewQuestions (List.map (\( a, b, _ ) -> ( a, b )) model.questions)


viewInput : Maybe String -> List (Html Message)
viewInput current =
    case current of
        Just text ->
            let
                created =
                    { title = text
                    , details = ""
                    , visibility = Api.Questions.Visible
                    , index = 0
                    , answersMin = 0
                    , answersMax = 0
                    }
            in
            [ Html.input [ Attribute.value text, Event.onInput WriteNewTitle ] []
            , Html.button [ Event.onClick <| PerformCreate created ] [ Html.text "**save**" ]
            ]

        Nothing ->
            List.singleton <| Html.button [ Event.onClick PerformCreateStart ] [ Html.text "**new question**" ]


viewQuestions : List ( ServerQuestion, Bool ) -> List (Html Message)
viewQuestions list =
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
                    |> List.concatMap (\( i, q, v ) -> viewQuestion i q v)
                    |> Html.tbody [ Attribute.class "bg-white" ]
                ]
            ]


viewQuestion : Int -> ServerQuestion -> Bool -> List (Html Message)
viewQuestion index question expanded =
    let
        expansion =
            if expanded then
                List.singleton <| viewQuestionExpansion question

            else
                []
    in
    [ Html.tr [ Attribute.class "border-b hover:bg-gray-100" ]
        [ Html.td [ Attribute.class "font-bold font-archivo break-words py-3 px-4" ]
            [ Html.span [ Attribute.class "text-gray-500 mr-2" ] [ Html.text <| String.fromInt (index + 1) ++ "." ]
            , Html.text question.title
            ]
        , Html.td [] [ Html.button [ Event.onClick <| PerformExpand question ] [ Html.text "**expand**" ] ]
        , Html.td []
            [ Html.button
                [ Attribute.class "text-gray-500 hover:text-red-500 capitalize font-archivo"
                , Attribute.class "text-right px-8"
                , Event.onClick <| PerformDelete question
                ]
                [ Html.text "Delete" ]
            ]
        ]
    ]
        ++ expansion


viewQuestionExpansion : ServerQuestion -> Html Message
viewQuestionExpansion question =
    Html.tr [] [ Html.td [ Attribute.colspan 3 ] [ Html.text <| "Expansion for : \"" ++ question.title ++ "\"" ] ]
