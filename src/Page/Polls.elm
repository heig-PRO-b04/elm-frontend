module Page.Polls exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Cmd exposing (withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Picasso.Button exposing (button, filled, outlined)
import Picasso.Text exposing (styledH2)
import Session exposing (Session)


type alias Message =
    Never


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Message )
init session =
    { session = session }
        |> withNoCmd


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    model
        |> withNoCmd


view : Model -> List (Html Message)
view model =
    [ div
        [ class "flex flex-col" ]
        [ div
            [ class "flex flex-row justify-center" ]
            [ actionButton "Create a new poll"
            , actionButton "See your archived polls"
            ]
        , div [ class "px-3 pb-3" ] [ styledH2 "Your polls:" ]
        , pollButton "This is a poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        , pollButton "This is another poll title"
        ]
    ]


actionButton content =
    button
        (filled
            ++ [ class "m-4"
               ]
        )
        [ text content ]


pollButton content =
    div
        [ class "flex flex-row" ]
        [ button
            (outlined
                ++ [ class "flex-grow"
                   , class "mb-2 mx-2"
                   ]
            )
            [ text content ]
        , button
            (filled
                ++ [ class "flex-shrink"
                   , class "mb-2 mr-2"
                   ]
            )
            [ text "arch" ]
        , button
            (filled
                ++ [ class "flex-shrink"
                   , class "mb-2 mr-2"
                   ]
            )
            [ text "del" ]
        ]
