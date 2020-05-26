module Picasso.Text exposing (styledH1, styledH2, styledH3)

{-| A module that provides basic text elements

@docs styledH1, styledH2, styledH3

-}

import Html exposing (Html, h1, h2, h3, text)
import Html.Attributes exposing (class)


{-| Builds a text title with the h1 tag
-}
styledH1 : String -> Html msg
styledH1 contents =
    h1 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]


{-| Builds a text title with the h2 tag
-}
styledH2 : String -> Html msg
styledH2 contents =
    h2 [ class "font-archivo text-2xl font-extrabold" ] [ text contents ]


{-| Builds a text title with the h3 tag
-}
styledH3 : String -> Html msg
styledH3 contents =
    h3 [ class "font-archivo text-md font-extrabold" ] [ text contents ]
