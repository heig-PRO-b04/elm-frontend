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
import Picasso.Text exposing (styledH1, styledH2)
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
                        (Session.navKey session)
                        toRoute
                    ]



-- VIEW


display : Model -> Display
display (Model route session state) =
    if route == Route.Login || route == Route.Registration then
        NoInfo

    else
        case Session.extractCredentials session of
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
    in
    Html.header
        [ class "flex flex-col md:flex-row bg-white shadow relative"
        , class "fixed top-0"
        , class "px-8 py-4"
        ]
        ([ title [ class "self-center md:text-start" ]
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
               )
        )


title : List (Html.Attribute Message) -> Html Message
title attributes =
    Html.button
        ([ onClick <| Request Route.Home, class "py-2" ]
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
            Button.button
                (filledLight
                    ++ [ class "flex flex-row items-center"
                       , onClick Toggle
                       ]
                )
                [ div
                    [ class "hidden md:block" ]
                    [ text <| data.username ]
                , div [ class "hidden md:block px-2" ] []
                , icon
                ]
    in
    [ div
        [ class "absolute right-0 top-0 bottom-0 h-12"
        , class "mr-4 mb-4 mt-4 md:mr-4 md:mb-0 md:mt-4"
        , class "flex flex-col items-center"
        ]
        ([ button ]
            ++ (if open then
                    [ div
                        [ class "relative mt-4 mr-4 top-0 right-0"
                        , class "rounded-md bg-white shadow px-0 border-4 border-seaside-300"
                        , class "flex flex-col"
                        ]
                        [ Button.button [ onClick <| Request Route.Home ] [ text "Home" ]
                        , Button.button [ onClick <| Request Route.Polls ] [ text "My Polls" ]
                        , Button.button [ onClick <| Request Route.Logout ] [ text "Log out" ]
                        ]
                    ]

                else
                    []
               )
        )
    ]


menuItem : Route -> String -> Html a
menuItem route content =
    Button.a [ Route.href Route.Home ] [ text "Home" ]


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
