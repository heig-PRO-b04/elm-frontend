module Page.PollList exposing
    ( Message
    , Model
    , init
    , subscriptions
    , update
    , view
    )

import Api.Polls exposing (ServerPoll)
import Api.Sessions
import Cmd.Extra exposing (withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Page.PollList.Sorting as Sorting
import Picasso.FloatingButton
import Route
import Session exposing (Viewer)
import Task
import Task.Extra
import Time



-- MODEL


type alias Model =
    { viewer : Viewer
    , polls : List ServerPoll
    , sessionStatuses : Dict ( Int, Int ) PollStatus
    , order : Sorting.Order
    }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    { viewer = viewer
    , polls = []
    , sessionStatuses = Dict.empty
    , order = Sorting.TitleAsc
    }
        |> withCmds [ Cmd.Extra.succeed NowRequestPolls ]



-- MESSAGE


type Message
    = GotAllPolls (List ServerPoll)
    | GotSessionStatus ServerPoll (Maybe Api.Sessions.SessionStatus)
    | GotInvalidCredentials
    | NowRequestPolls
    | NowDeletePoll ServerPoll
    | NowSetOrder Sorting.Order
    | NowDisplayPoll ServerPoll


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GotSessionStatus poll maybeStatus ->
            let
                status : PollStatus
                status =
                    case maybeStatus of
                        Just Api.Sessions.Open ->
                            Open

                        Just Api.Sessions.Closed ->
                            Closed

                        Just Api.Sessions.Quarantined ->
                            ClosedToNewcomers

                        Nothing ->
                            Closed

                updated : Dict ( Int, Int ) PollStatus
                updated =
                    Dict.insert
                        ( poll.idModerator, poll.idPoll )
                        status
                        model.sessionStatuses
            in
            { model | sessionStatuses = updated }
                |> withNoCmd

        GotAllPolls polls ->
            let
                getSessionStatus : ServerPoll -> Cmd Message
                getSessionStatus poll =
                    Api.Sessions.getSession (Session.viewerCredentials model.viewer) identity poll
                        |> Task.mapError (always <| GotSessionStatus poll Nothing)
                        |> Task.map (\status -> status.status)
                        |> Task.map (\status -> GotSessionStatus poll (Just status))
                        |> Task.Extra.execute
            in
            { model | polls = polls }
                |> withCmds
                    (List.map getSessionStatus polls)

        GotInvalidCredentials ->
            model
                |> withCmds [ Route.badCredentials (Session.viewerNavKey model.viewer) ]

        NowRequestPolls ->
            model
                |> withCmds
                    [ Api.Polls.getPollList (Session.viewerCredentials model.viewer) identity
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
                |> withCmds
                    [ Api.Polls.delete (Session.viewerCredentials model.viewer) poll.idPoll NowRequestPolls
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
                |> withCmds
                    [ Route.replaceUrl
                        (Session.viewerNavKey model.viewer)
                        (Route.DisplayPoll poll.idPoll)
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
        pollToPair :
            Dict ( Int, Int ) PollStatus
            -> ServerPoll
            -> ( ServerPoll, PollStatus )
        pollToPair dict poll =
            Dict.get ( poll.idModerator, poll.idPoll ) dict
                |> Maybe.withDefault Loading
                |> (\status -> ( poll, status ))

        polls =
            List.sortWith (Sorting.ordering model.order) model.polls

        mapped =
            polls |> List.map (pollToPair model.sessionStatuses)
    in
    [ viewTable model.order mapped, fab ]


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


viewTable : Sorting.Order -> List ( ServerPoll, PollStatus ) -> Html Message
viewTable order polls =
    div [ class "align-middle mx-2 md:mx-8 mt-8 mb-32" ]
        [ Html.table [ class "min-w-full center border rounded-lg overflow-hidden shadow" ]
            [ Html.thead [] [ viewHeader order ]
            , Html.tbody [ class "bg-white" ]
                (List.map (\( poll, status ) -> viewPoll poll status) polls)
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


viewPoll : ServerPoll -> PollStatus -> Html Message
viewPoll poll status =
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
            [ class "py-2"
            , onClick <| NowDisplayPoll poll
            ]
            [ pill status ]
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
                    ( "Loading", class "bg-gray-100 text-gray-400 border border-gray-400" )
    in
    div
        [ class "rounded-full py-0 px-4"
        , class "font-archivo font-semibold capitalize text-sm"
        , class "inline-block select-none cursor-default"
        , color
        ]
        [ text contents ]
