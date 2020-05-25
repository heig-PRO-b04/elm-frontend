module Picasso.Text exposing (styledH1, styledH2, styledH3)

{-| A module that provides basic text elements


# Constructors

If you want to use Route-based hrefs for navigation, it is highly recommended
that you use an `a` based button.

@docs styledH1, styledH2, styledH3

-}

import Html exposing (Html, h1, h2, h3, text)
import Html.Attributes exposing (class)


styledH1 : String -> Html msg
styledH1 contents =
    h1 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]


styledH2 : String -> Html msg
styledH2 contents =
    h2 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]


styledH3 : String -> Html msg
styledH3 contents =
    h3 [ class "font-archivo text-md font-extrabold" ] [ text contents ]
