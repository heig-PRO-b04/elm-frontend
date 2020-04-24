module Picasso.Navigation exposing
    ( Model, init
    , withRoute, withSession
    , Message, update, view
    )

{-| A module in charge of providing some utilities to display a navigation bar.
In particular, the navigation bar will offer some links to different application
sections, and let the user therefore log in, and perform similar actions.


# Populating

To populate a bar, you need to provide it with some information about what
contents it should display. This content can be created through designated
methods from various objects, in particular application sessions.

@docs Model, init
@docs withRoute, withSession


# View

@docs bar

-}

import Api
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Picasso.Button as Button exposing (filled, filledLight, outlined, outlinedLight)
import Picasso.Text exposing (styledH1, styledH2, styledH3)
import Route exposing (Route)
import Session exposing (Session)



-- MODEL


type Model
    = Model Route Session MenuState


type Message
    = Toggle
    | Request Route


type MenuState
    = MenuOpen
    | MenuClosed


type Display
    = NoInfo
    | NoInfoStatic
    | ReadyToLogin
    | LoggedInClosed { username : String }
    | LoggedInOpen { username : String }


init : Route -> Session -> Model
init route session =
    Model route session MenuClosed


withRoute : Route -> Model -> Model
withRoute route (Model _ session state) =
    Model route session state


withSession : Session -> Model -> Model
withSession session (Model route _ state) =
    Model route session state



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message (Model route session state) =
    case message of
        Toggle ->
            case state of
                MenuOpen ->
                    Model route session MenuClosed
                        |> withNoCmd

                MenuClosed ->
                    Model route session MenuOpen
                        |> withNoCmd

        Request toRoute ->
            Model toRoute session MenuClosed
                |> withCmd
                    [ Route.replaceUrl
                        (Session.sessionNavKey session)
                        toRoute
                    ]



-- VIEW


display : Model -> Display
display (Model route session state) =
    case route of
        Route.Login ->
            NoInfo

        Route.Registration ->
            NoInfo

        Route.LivePoll _ ->
            NoInfoStatic

        _ ->
            case Session.sessionCredentials session of
                Just credentials ->
                    case state of
                        MenuClosed ->
                            LoggedInClosed { username = Api.username credentials }

                        MenuOpen ->
                            LoggedInOpen { username = Api.username credentials }

                Nothing ->
                    ReadyToLogin


view : Model -> Html Message
view model =
    let
        info =
            display model

        clickableTitle =
            if info == NoInfoStatic then
                [ class "cursor-default" ]

            else
                [ onClick <| Request Route.Home ]

        navigationClass =
            if info == ReadyToLogin || info == NoInfo then
                class "flex flex-col md:flex-row items-stretch md:items-center"

            else
                class "flex flex-row items-center"
    in
    Html.header
        [ class "bg-white shadow w-full"
        , navigationClass
        , class "relative"
        , class "sticky top-0"
        , class "pr-2 pl-4 md:pr-8 md:pl-8 py-4"
        ]
        ([ title <| [ class "md:text-start" ] ++ clickableTitle
         , filler
         ]
            ++ (case info of
                    LoggedInOpen username ->
                        tailAuthenticated True username

                    LoggedInClosed username ->
                        tailAuthenticated False username

                    ReadyToLogin ->
                        tailUnauthenticated

                    NoInfo ->
                        []

                    NoInfoStatic ->
                        []
               )
        )


title : List (Html.Attribute Message) -> Html Message
title attributes =
    Html.button
        ([ class "py-2" ]
            ++ attributes
        )
        [ styledH1 "✌️ rockin • app" ]


icon : Html Message
icon =
    Html.img [ src "/icon/navigation-account-circle-outline.svg" ] []


filler : Html a
filler =
    div [ class "flex-grow" ] []


tailAuthenticated : Bool -> { username : String } -> List (Html Message)
tailAuthenticated open data =
    let
        button =
            menuButton data.username open []
    in
    [ button ]


menuButton :
    String
    -> Bool
    -> List (Html.Attribute Message)
    -> Html Message
menuButton username open attributes =
    let
        visibility =
            if open then
                class "block"

            else
                class "hidden"

        listItem route message =
            Html.button
                [ class "w-full px-4 py-2"
                , class "hover:bg-seaside-400 hover:text-white"
                , onClick <| Request route
                ]
                [ styledH3 message ]
    in
    div
        ([ class "relative"
         ]
            ++ attributes
        )
        [ Button.button
            (filledLight
                ++ [ class "flex flex-row items-center"
                   , onClick Toggle
                   ]
            )
            [ div [ class "hidden md:block" ] [ text username ]
            , div [ class "hidden md:block px-2" ] []
            , icon
            ]
        , div
            [ class "absolute mt-2 right-0"
            , class "rounded-md bg-white shadow-2xl"
            , class "border-2 border-seaside-050"
            , class "overflow-hidden"
            , visibility
            , class "w-32"
            ]
            [ listItem Route.Home "Home"
            , listItem Route.Polls "My Polls"
            , listItem Route.Logout "Sign out"
            ]
        ]


tailUnauthenticated : List (Html a)
tailUnauthenticated =
    [ Button.a
        (outlined
            |> List.append
                [ Route.href Route.Login
                , class "mt-4 md:mt-0"
                , class "md:self-center text-center"
                ]
        )
        [ text "Sign In" ]
    , Button.a
        (filled
            |> List.append
                [ Route.href Route.Registration
                , class "mt-4 md:mt-0 md:ml-4"
                , class "md:self-center text-center"
                ]
        )
        [ text "Create an Account" ]
    ]
