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
import Html.Attributes exposing (class)
import Picasso.Button as Button exposing (filled, outlined, outlinedLight)
import Picasso.Text exposing (styledH1)
import Route exposing (Route)
import Session exposing (Session)



-- MODEL


type Model
    = Model Route Session


type alias Message =
    Never


type Display
    = NoInfo
    | NoAuthentication
    | Authenticated { username : String }


init : Route -> Session -> Model
init route session =
    Model route session


withRoute : Route -> Model -> Model
withRoute route (Model _ session) =
    Model route session


withSession : Session -> Model -> Model
withSession session (Model route _) =
    Model route session



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    model |> withNoCmd



-- VIEW


display : Model -> Display
display (Model route session) =
    if route == Route.Login || route == Route.Registration then
        NoInfo

    else
        Session.extractCredentials session
            |> Maybe.map (\cred -> { username = Api.username cred })
            |> Maybe.map Authenticated
            |> Maybe.withDefault NoAuthentication


view : Model -> Html Message
view model =
    let
        info =
            display model
    in
    Html.header
        [ class "flex flex-col md:flex-row bg-white shadow"
        , class "sticky top-0"
        , class "px-8 py-4"
        ]
        ([ Html.a
            [ Route.href Route.Home, class "self-center md:text-start py-2" ]
            [ styledH1 "✌️ rockin • app" ]
         , filler
         ]
            ++ (case info of
                    Authenticated username ->
                        tailAuthenticated username

                    NoAuthentication ->
                        tailUnauthenticated

                    NoInfo ->
                        []
               )
        )


filler : Html a
filler =
    div [ class "flex-grow" ] []


tailAuthenticated : { username : String } -> List (Html a)
tailAuthenticated data =
    List.singleton <|
        Button.a
            (outlinedLight
                ++ [ class "flex flex-row items-center"
                   , Route.href Route.Logout
                   ]
            )
            [ text <| data.username
            , div [ class "px-2" ] [ text "•" ]
            , text "log out"
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
