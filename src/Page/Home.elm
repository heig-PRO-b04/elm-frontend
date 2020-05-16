module Page.Home exposing
    ( Message
    , Model
    , init
    , toSession
    , update
    , view
    )

import Cmd exposing (withNoCmd)
import Html exposing (Html, a, br, div, img, text)
import Html.Attributes exposing (class, href, src)
import Session exposing (Session)


type alias Message =
    Never


type alias Model =
    Session


toSession : Model -> Session
toSession =
    identity


init : Session -> ( Model, Cmd Message )
init session =
    ( session, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    model
        |> withNoCmd


view : Model -> List (Html Message)
view _ =
    [ footer
    ]


footer : Html Never
footer =
    Html.footer
        [ class "fixed bottom-0 bg-white shadow pl-2 pr-8 md:pr-16 py-2 w-full"
        ]
        [ div
            [ class "flex flex-row items-center" ]
            [ img [ src "/img/github.jpg", class "h-16" ] []
            , div [ class "ml-4" ]
                [ text "This project was done at HEIG-VD as part of the PRO class."
                , br [] []
                , a [ href "https://github.com/heig-PRO-b04", class "text-seaside-500" ] [ text "Check it out on GitHub." ]
                ]
            ]
        ]
