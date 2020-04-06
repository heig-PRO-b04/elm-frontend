module Picasso.Text exposing (..)

import Html exposing (Html, h1, h2, text)
import Html.Attributes exposing (class)


styledH1 : String -> Html msg
styledH1 contents =
    h1 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]


styledH2 : String -> Html msg
styledH2 contents =
    h2 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]
