module Picasso.Text exposing (..)

import Html exposing (Html, h2, text)
import Html.Attributes exposing (class)


styledH2 : String -> Html Never
styledH2 contents =
    h2 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]
