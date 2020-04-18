module Picasso.FloatingButton exposing (a, button)

import Html exposing (Html, div)
import Html.Attributes exposing (class)



-- BASE STYLING


base : List (Html.Attribute msg)
base =
    [ class "bg-seaside-500 hover:bg-seaside-400"
    , class "border-2 border-seaside-500"
    , class "text-white"
    , class "pl-4 pr-6 py-4"
    , class "font-archivo font-semibold uppercase"
    , class "shadow-xl hover:shadow-2xl"
    , class "rounded-full"
    ]



-- HTML TAGS


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs html =
    Html.button (base ++ attrs) [ div [ class "flex flew-row items-center" ] html ]


a : List (Html.Attribute msg) -> List (Html msg) -> Html msg
a attrs html =
    Html.a (base ++ attrs) [ div [ class "flex flew-row items-center" ] html ]
