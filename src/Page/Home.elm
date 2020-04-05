module Page.Home exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser.Navigation as Nav
import Cmd exposing (withNoCmd)
import Html exposing (Html, a, br, div, form, h1, h2, h4, hr, img, input, label, nav, p, tbody, text, ul)
import Html.Attributes exposing (alt, class, for, href, id, src, type_)
import Html.Events exposing (onClick)
import Picasso.Button exposing (button, elevated, filled, outlined)
import Picasso.Text exposing (styledH1, styledH2)
import Route
import Session exposing (Session, guest)


type alias Message =
    Never


type alias Model =
    { session : Session }


init : Session -> Model
init session =
    { session = session }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    model
        |> withNoCmd


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]


view : Model -> Html Message
view model =
    div
        -- Whole page
        [ class "flex flex-col"
        , class "w-screen"
        , class "h-screen"
        , class "items-stretch"
        ]
        [ navBar
        , content
        , footer
        ]


uniformPadding : Html msg -> Html msg
uniformPadding html =
    div [ class "p-3" ]
        [ html ]


navBar : Html Never
navBar =
    div
        -- Navigation bar
        [ class "flex flex-row"
        , class "items-center"
        , class "bg-white"
        ]
        [ div
            -- Title and logo
            []
            [ a
                [ href "#"
                , class "flex flex-row items-center"
                , class "m-3"
                ]
                [ img
                    [ src "https://res-5.cloudinary.com/crunchbase-production/image/upload/c_lpad,h_256,w_256,f_auto,q_auto:eco/kyjjstbylc4layrwzshq"
                    , class "h-12"
                    , class "w-12"
                    , class "mx-2"
                    ]
                    []
                , styledH2 "HEIG-PRO-B04"
                ]
            ]
        , div
            -- Navbar filler
            [ class "flex-grow"
            ]
            []
        , button (outlined ++ elevated ++ [ class "m-3 mr-1", class "p-3" ]) [ text "Sign in" ]
        , button (filled ++ elevated ++ [ class "m-3 ml-2", class "p-3" ]) [ text "Sign up" ]
        ]


content : Html Never
content =
    div
        -- Main content
        [ class "flex flex-row"
        , class "flex-grow"
        , class "items-center"
        , class "bg-gray-200"
        ]
        [ div
            [ class "flex flex-grow flex-col"
            , class "items-center"
            ]
            [ div
                [ class "flex"
                ]
                [ styledH1 "A poll website"
                ]
            , div
                [ class "flex"
                ]
                [ text "Hello, world"
                ]
            ]
        , div
            [ class "w-1/3"
            , class "content-center"
            ]
            [ div
                [ class "flex flex-col"
                , class "bg-white"
                , class "rounded-lg"
                , class "p-8"
                , class "m-8"
                ]
                [ styledH2 "Get the app"
                , text "To participate to polls created by your peers, you need to download the android app!"
                , br [] []
                , text "Sorry, no iOS app in sight."
                , button (filled ++ elevated ++ [ class "mt-3" ] ++ [ Route.Login |> Route.href ]) [ text "Get the app" ]
                ]
            ]
        ]


footer : Html Never
footer =
    div
        -- Footer
        [ class "flex flex-row"
        , class "bg-gray-600"
        , class "items-center justify-center"
        ]
        [ a
            [ href "https://github.com/heig-PRO-b04/elm-frontend"
            ]
            [ img
                [ src "https://danielzelfo.com/img/github.png"
                , class "h-10"
                , class "w-10"
                , class "m-2"
                ]
                []
            ]
        ]
