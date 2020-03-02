module Picasso.Button exposing
    ( button
    , elevated
    , filled
    , outlined
    )

import Html exposing (Html)
import Html.Attributes exposing (class)


base : List (Html.Attribute msg)
base =
    [ class "px-6 py-2"
    , class "font-manrope font-semibold"
    , class "rounded-md"
    ]


outlined : List (Html.Attribute msg)
outlined =
    [ class "bg-white hover:bg-cactus-100"
    , class "text-cactus-500 hover:text-cactus-600"
    , class "border-2 border-cactus-500"
    ]


elevated : List (Html.Attribute msg)
elevated =
    [ class "rounded-md"
    , class "shadow"
    ]


filled : List (Html.Attribute msg)
filled =
    [ class "bg-cactus-500 hover:bg-cactus-400"
    , class "text-cactus-050 hover:text-white"
    , class "border-2 border-cactus-500"
    ]


button :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
button attrs html =
    Html.button (base ++ attrs) html
