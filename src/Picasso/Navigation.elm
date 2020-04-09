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
import Cmd exposing (withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Picasso.Button as Button exposing (filled, filledLight, outlined, outlinedLight)
import Picasso.Text exposing (styledH1)
import Route exposing (Route)
import Session exposing (Session)



-- MODEL


type Model
    = Model Route Session MenuState


type Message
    = Toggle


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
                    Model route session MenuClosed |> withNoCmd

                MenuClosed ->
                    Model route session MenuOpen |> withNoCmd



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
        , class "sticky top-0"
        , class "px-8 py-4"
        ]
        ([ title [ class "self-center md:text-start" ]
         , filler
         ]
            ++ (case info of
                    LoggedInOpen username ->
                        tailAuthenticated username

                    LoggedInClosed username ->
                        tailAuthenticated username

                    ReadyToLogin ->
                        tailUnauthenticated

                    NoInfo ->
                        []
               )
        )


title : List (Html.Attribute a) -> Html a
title attributes =
    Html.a
        ([ Route.href Route.Home, class "py-2" ]
            ++ attributes
        )
        [ styledH1 "✌️ rockin • app" ]


icon : Html Message
icon =
    Html.img [ onClick Toggle, src "/icon/navigation-account-circle-outline.svg" ] []


filler : Html a
filler =
    div [ class "flex-grow" ] []


tailAuthenticated : { username : String } -> List (Html Message)
tailAuthenticated data =
    [ Button.a
        (filledLight
            ++ [ class "flex flex-row items-center"
               , class "absolute right-0 top-0 bottom-0 mt-4 mb-4 mr-4"

               --, class "text-center"
               , Route.href Route.Logout
               ]
        )
        [ div
            [ class "hidden md:block" ]
            [ text <| data.username ]
        , div [ class "hidden md:block px-2" ] []
        , icon
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
