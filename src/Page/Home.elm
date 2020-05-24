module Page.Home exposing
    ( Message
    , Model
    , init
    , toSession
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Picasso.Button
import Route
import Session exposing (Session)


type Message
    = GoToSignIn


type alias Model =
    Session


toSession : Model -> Session
toSession =
    identity


init : Session -> ( Model, Cmd Message )
init session =
    ( session, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GoToSignIn ->
            ( model
            , Route.replaceUrl
                (Session.sessionNavKey model)
                Route.Registration
            )


view : Model -> List (Html Message)
view _ =
    [ callToAction
    , mobileInfo
    , footer
    ]


callToAction : Html Message
callToAction =
    Html.div
        [ Attribute.class "shadow bg-white md:rounded-lg max-w-full mt-8 lg:mt-48 lg:mx-48 py-4 px-16 lg:py-8 lg:px-8 mx-auto"
        , Attribute.class "flex flex-row justify-between"
        ]
        [ Html.div [ Attribute.class "pt-12" ]
            [ Html.h1
                [ Attribute.class "text-4xl font-semibold text-gray-800" ]
                [ Html.span [ Attribute.class "block" ] [ Html.text "Real-time polls for" ]
                , Html.span [ Attribute.class "block" ] [ Html.text "conferences and schools" ]
                ]
            , Html.div
                [ Attribute.class "pt-8 pb-12" ]
                [ Picasso.Button.button
                    (Picasso.Button.filled
                        ++ Picasso.Button.elevated
                        ++ [ Attribute.class "rounded-full text-xl"
                           , Event.onClick GoToSignIn
                           ]
                    )
                    [ Html.text "Register now"
                    , Html.img [ Attribute.class "inline-block pl-4", Attribute.src "/icon/arrow-right.svg" ] []
                    ]
                ]
            ]
        , Html.img [ Attribute.class "hidden md:block self-center mr-16 w-48 h-48", Attribute.src "/img/logo.png" ] []
        ]


mobileInfo : Html Message
mobileInfo =
    Html.div
        [ Attribute.class "shadow bg-gray-100 md:rounded-lg max-w-full mt-4 lg:mt-8 lg:mx-48 mb-24 lg:mb-48 py-4 px-16 lg:py-8 lg:px-8 mx-auto text-xl text-gray-500" ]
        [ Html.h2
            [ Attribute.class "text-2xl font-semibold text-gray-700" ]
            [ Html.text "Got a mobile device ?" ]
        , Html.span [] [ Html.text "You can download the participant app from " ]
        , Html.a [ Attribute.href "https://play.google.com/store/apps/details?id=ch.heigvd.pro.b04.android", Attribute.class "text-seaside-400 underline" ] [ Html.text "Google Play" ]
        , Html.span [] [ Html.text "." ]
        ]


footer : Html Message
footer =
    Html.footer
        [ Attribute.class "fixed bottom-0 bg-white shadow pl-2 pr-8 md:pr-16 py-2 w-full"
        ]
        [ Html.div
            [ Attribute.class "flex flex-row items-center" ]
            [ Html.img [ Attribute.src "/img/github.jpg", Attribute.class "h-16" ] []
            , Html.div [ Attribute.class "ml-4" ]
                [ Html.text "This project was done at HEIG-VD as part of the PRO class."
                , Html.br [] []
                , Html.a
                    [ Attribute.href "https://github.com/heig-PRO-b04", Attribute.class "text-seaside-500" ]
                    [ Html.text "Check it out on GitHub." ]
                ]
            ]
        ]
