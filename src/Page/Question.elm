module Page.Question exposing
    ( Message
    , Model
    , init
    , subscriptions
    , update
    , view
    , viewIndex
    )

import Api.Questions exposing (QuestionDiscriminator, QuestionVisibility(..), ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events as Html
import Session exposing (Viewer)
import Task
import Task.Extra



-- MODEL


type QuestionState
    = Loading
    | Loaded ServerQuestion
    | Deleted
    | Error Viewer


type QuestionPill
    = LoadingPill
    | VisiblePill
    | HiddenPill
    | ArchivedPill
    | ErrorPill
    | DeletedPill


type alias Model =
    { viewer : Viewer
    , state : QuestionState
    }


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init viewer discriminator =
    { viewer = viewer
    , state = Loading
    }
        |> withCmd
            [ QuestionDiscriminator discriminator.idPoll discriminator.idQuestion
                |> NowRequestQuestion
                |> Cmd.succeed
            ]



-- UPDATE


type Message
    = NowRequestQuestion QuestionDiscriminator
    | NowDeleteQuestion QuestionDiscriminator
    | GotQuestion ServerQuestion
    | GotDeletedQuestion
    | GotInvalidCredentials
    | GotError


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NowRequestQuestion questionDiscriminator ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Questions.getQuestion viewer questionDiscriminator identity
                        |> Task.map GotQuestion
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Questions.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotError
                            )
                        |> Task.Extra.execute
                    ]

        NowDeleteQuestion questionDiscriminator ->
            let
                viewer =
                    Session.viewerCredentials model.viewer
            in
            model
                |> withCmd
                    [ Api.Questions.delete viewer questionDiscriminator GotDeletedQuestion
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Questions.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotError
                            )
                        |> Task.Extra.execute
                    ]

        GotQuestion serverQuestion ->
            { model | state = Loaded serverQuestion }
                |> withNoCmd

        GotDeletedQuestion ->
            { model | state = Deleted }
                |> withNoCmd

        GotInvalidCredentials ->
            { model | state = Error model.viewer }
                |> withNoCmd

        GotError ->
            { model | state = Error model.viewer }
                |> withNoCmd


subscriptions : Sub Message
subscriptions =
    Sub.none



-- VIEW


view : Model -> Html Message
view model =
    case model.state of
        Loading ->
            Html.tr
                [ class " border-b active:shadow-inner hover:bg-gray-100"
                ]
                [ Html.td
                    [ class "py-3 px-4"
                    , class "font-bold font-archivo break-all"
                    ]
                    [ Html.text "Loading question" ]
                , Html.td
                    [ class "py-2" ]
                    [ visibilityPill LoadingPill ]
                , Html.td
                    [ class "text-right px-8" ]
                    []
                ]

        Loaded serverQuestion ->
            Html.tr
                [ class " border-b active:shadow-inner hover:bg-gray-100"
                ]
                [ Html.td
                    [ class "py-3 px-4"
                    , class "font-bold font-archivo break-all"
                    ]
                    [ Html.text serverQuestion.title ]
                , Html.td
                    [ class "py-2" ]
                    [ case serverQuestion.visibility of
                        Visible ->
                            VisiblePill
                                |> visibilityPill

                        Hidden ->
                            HiddenPill
                                |> visibilityPill

                        Archived ->
                            ArchivedPill
                                |> visibilityPill
                    ]
                , Html.td
                    [ class "text-right px-8" ]
                    [ Html.button
                        [ class "text-gray-500 hover:text-red-500 "
                        , class "capitalize font-archivo"
                        , QuestionDiscriminator serverQuestion.idPoll serverQuestion.idQuestion
                            |> NowDeleteQuestion
                            |> Html.onClick
                        ]
                        [ Html.text "Delete"
                        ]
                    ]
                ]

        Deleted ->
            Html.tr
                [ class " border-b active:shadow-inner hover:bg-gray-100"
                ]
                [ Html.td
                    [ class "py-3 px-4"
                    , class "font-bold font-archivo break-all"
                    ]
                    [ Html.text "The question was deleted" ]
                , Html.td
                    [ class "py-2" ]
                    [ visibilityPill DeletedPill ]
                , Html.td
                    [ class "text-right px-8" ]
                    []
                ]

        Error viewer ->
            Html.tr
                [ class " border-b active:shadow-inner hover:bg-gray-100"
                ]
                [ Html.td
                    [ class "py-3 px-4"
                    , class "font-bold font-archivo break-all"
                    ]
                    [ Html.text "There was an error" ]
                , Html.td
                    [ class "py-2" ]
                    [ visibilityPill ErrorPill ]
                , Html.td
                    [ class "text-right px-8" ]
                    []
                ]


visibilityPill : QuestionPill -> Html msg
visibilityPill status =
    let
        ( contents, color ) =
            case status of
                LoadingPill ->
                    ( "Loading", class "bg-gray-100 text-gray-400 border border-gray-400" )

                VisiblePill ->
                    ( "Visible", class "bg-seaside-500 text-white shadow" )

                HiddenPill ->
                    ( "Hidden", class "bg-seaside-050 text-black" )

                ArchivedPill ->
                    ( "Archived", class "bg-yellow-500 text-white shadow" )

                ErrorPill ->
                    ( "Error", class "bg-black text-white shadow" )

                DeletedPill ->
                    ( "Deleted", class "bg-red-500 text-white shadow" )
    in
    div
        [ class "rounded-full py-0 px-4"
        , class "font-archivo font-semibold capitalize text-sm"
        , class "inline-block select-none cursor-default"
        , color
        ]
        [ Html.text contents ]


viewIndex : Model -> Float
viewIndex _ =
    0
