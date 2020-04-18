module Page.Polls exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Picasso.FloatingButton
import Route
import Session exposing (Session, Viewer)
import Task
import Task.Extra


type Message
    = GotNewPolls (List Poll)
    | RequestPolls
    | DeletePoll Poll


type alias Model =
    { viewer : Viewer
    , polls : List Poll
    }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    { viewer = viewer
    , polls = []
    }
        |> withCmd [ Task.perform identity <| Task.succeed RequestPolls ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GotNewPolls polls ->
            { model | polls = polls } |> withNoCmd

        RequestPolls ->
            model
                |> withCmd
                    [ Api.Polls.getAllPolls (Session.viewerCredentials model.viewer) identity
                        |> Task.mapError (always [])
                        |> Task.Extra.execute
                        |> Cmd.map GotNewPolls
                    ]

        DeletePoll poll ->
            model
                |> withCmd
                    [ Api.Polls.delete (Session.viewerCredentials model.viewer) poll RequestPolls
                        |> Task.mapError (always RequestPolls)
                        |> Task.Extra.execute
                    ]


view : Model -> List (Html Message)
view model =
    [ table model.polls ]
        ++ [ Picasso.FloatingButton.a
                [ class "fixed right-0 bottom-0 m-8"
                , Route.href Route.NewPoll
                ]
                [ img [ src "icon/action-button-plus.svg" ] []
                , div [ class "ml-4" ] [ text "New poll" ]
                ]
           ]


table : List Poll -> Html Message
table polls =
    div [ class "align-middle mx-8 my-8" ]
        [ Html.table [ class "min-w-full center border rounded-lg overflow-hidden shadow" ]
            [ Html.thead [] [ viewHeader ]
            , Html.tbody [ class "bg-white" ]
                (List.map viewPoll polls)
            ]
        ]


viewHeader : Html msg
viewHeader =
    Html.tr [ class "bg-gray-100 border-b" ]
        [ viewHeaderRow [ class "px-6" ] [ text "Title" ]
        , viewHeaderRow [ class "px-2" ] [ text "Status" ]
        , viewHeaderRow [] []
        ]


viewHeaderRow : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewHeaderRow attrs html =
    let
        base =
            [ class "font-archivo text-gray-500 text-left tracking-wider border-gray-200"
            , class "py-3"
            ]
    in
    Html.th (base ++ attrs) html


viewPoll : Poll -> Html Message
viewPoll poll =
    Html.tr
        [ class " border-b hover:shadow-inner hover:bg-gray-100" ]
        [ Html.td
            [ class "py-3 px-4"
            , class "font-bold font-archivo"
            ]
            [ text poll.title ]
        , Html.td
            []
            [ text "Content that is extremely big" ]
        , Html.td
            [ class "text-right px-8" ]
            [ Html.button
                [ class "text-gray-500 hover:text-red-500 "
                , class "capitalize font-archivo"
                , onClick <| DeletePoll poll
                ]
                [ text "Delete" ]
            ]
        ]
