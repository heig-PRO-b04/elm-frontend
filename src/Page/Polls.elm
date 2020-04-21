module Page.Polls exposing
    ( Message
    , Model
    , init
    , subscriptions
    , update
    , view
    )

import Api.Polls exposing (Poll)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Page.Polls.Sorting as Sorting
import Picasso.FloatingButton
import Route
import Session exposing (Session, Viewer)
import Task
import Task.Extra
import Time



-- MODEL


type alias Model =
    { viewer : Viewer
    , polls : List Poll
    , order : Sorting.Order
    }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    { viewer = viewer
    , polls = []
    , order = Sorting.TitleAsc
    }
        |> withCmd [ Cmd.succeed NowRequestPolls ]



-- MESSAGE


type Message
    = GotAllPolls (List Poll)
    | GotInvalidCredentials
    | NowRequestPolls
    | NowDeletePoll Poll
    | NowSetOrder Sorting.Order
    | NowDisplayPoll Poll


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GotAllPolls polls ->
            { model | polls = polls } |> withNoCmd

        GotInvalidCredentials ->
            model
                |> withCmd [ Route.badCredentials (Session.viewerNavKey model.viewer) ]

        NowRequestPolls ->
            model
                |> withCmd
                    [ Api.Polls.getAllPolls (Session.viewerCredentials model.viewer) identity
                        |> Task.map GotAllPolls
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Polls.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        GotAllPolls []
                            )
                        |> Task.Extra.execute
                    ]

        NowDeletePoll poll ->
            model
                |> withCmd
                    [ Api.Polls.delete (Session.viewerCredentials model.viewer) poll NowRequestPolls
                        |> Task.mapError
                            (\error ->
                                case error of
                                    Api.Polls.GotBadCredentials ->
                                        GotInvalidCredentials

                                    _ ->
                                        NowRequestPolls
                            )
                        |> Task.Extra.execute
                    ]

        NowSetOrder order ->
            { model | order = order } |> withNoCmd

        NowDisplayPoll poll ->
            model
                |> withCmd
                    [ Route.replaceUrl
                        (Session.viewerNavKey model.viewer)
                        (Route.DisplayPoll { idPoll = poll.idPoll })
                    ]


{-| Request polls refresh every 10 seconds.
-}
subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every (10 * 1000) (always NowRequestPolls)



-- VIEW


{-| The main view of the page. Displays the list of polls, as well as the floating action button
that navigates to the poll creation screen.
-}
view : Model -> List (Html Message)
view model =
    let
        sorted =
            List.sortWith (Sorting.ordering model.order) model.polls
    in
    [ viewTable model.order sorted ] ++ [ fab ]


{-| The floating action button that enables navigation to the new poll screen.
-}
fab : Html Message
fab =
    Picasso.FloatingButton.a
        [ class "fixed right-0 bottom-0 m-8"
        , Route.href Route.NewPoll
        ]
        [ img [ src "icon/action-button-plus.svg" ] []
        , div [ class "ml-4" ] [ text "New poll" ]
        ]


viewTable : Sorting.Order -> List Poll -> Html Message
viewTable order polls =
    div [ class "align-middle mx-2 md:mx-8 mt-8 mb-32" ]
        [ Html.table [ class "min-w-full center border rounded-lg overflow-hidden shadow" ]
            [ Html.thead [] [ viewHeader order ]
            , Html.tbody [ class "bg-white" ]
                (List.map viewPoll polls)
            ]
        ]


viewHeader : Sorting.Order -> Html Message
viewHeader order =
    let
        titleNextOrder =
            case order of
                Sorting.TitleAsc ->
                    Sorting.TitleDes

                Sorting.TitleDes ->
                    Sorting.TitleAsc

        titleSortIcon =
            let
                base =
                    [ src "/icon/arrow-down.svg"
                    , class "inline-block ml-1"
                    , class "transform duration-200 "
                    , class "scale-75 group-hover:scale-90"
                    ]

                rotation =
                    case order of
                        Sorting.TitleAsc ->
                            class "rotate-0"

                        Sorting.TitleDes ->
                            class "rotate-180"
            in
            img (rotation :: base) []
    in
    Html.tr [ class "bg-gray-100 border-b" ]
        [ viewHeaderRow
            [ class "px-6 group", onClick <| NowSetOrder titleNextOrder ]
            [ text "Title", titleSortIcon ]
        , viewHeaderRow [ class "px-2" ] [ text "Status" ]
        , viewHeaderRow [] []
        ]


viewHeaderRow : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewHeaderRow attrs html =
    let
        base =
            [ class "font-archivo text-gray-500 text-left tracking-wider border-gray-200 select-none"
            , class "py-3"
            ]
    in
    Html.th (base ++ attrs) html


viewPoll : Poll -> Html Message
viewPoll poll =
    Html.tr
        [ class " border-b active:shadow-inner hover:bg-gray-100"
        ]
        [ Html.td
            [ class "py-3 px-4"
            , class "font-bold font-archivo break-all"
            , onClick <| NowDisplayPoll poll
            ]
            [ text poll.title ]
        , Html.td
            [ class "py-2" ]
            [ pill Closed ]
        , Html.td
            [ class "text-right px-8" ]
            [ Html.button
                [ class "text-gray-500 hover:text-red-500 "
                , class "capitalize font-archivo"
                , onClick <| NowDeletePoll poll
                ]
                [ text "Delete" ]
            ]
        ]


type PollStatus
    = Closed
    | ClosedToNewcomers
    | Open
    | Loading


pill : PollStatus -> Html msg
pill status =
    let
        ( contents, color ) =
            case status of
                Open ->
                    ( "Live", class "bg-seaside-500 text-white shadow" )

                Closed ->
                    ( "Closed", class "bg-seaside-050 text-black" )

                ClosedToNewcomers ->
                    ( "Closed to newcomers", class "bg-seaside-400 text-white shadow" )

                Loading ->
                    ( "Loading", class "hidden" )
    in
    div
        [ class "rounded-full py-0 px-4"
        , class "font-archivo font-semibold capitalize text-sm"
        , class "inline-block select-none cursor-default"
        , color
        ]
        [ text contents ]
