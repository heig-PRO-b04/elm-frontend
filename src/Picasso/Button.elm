module Picasso.Button exposing
    ( a
    , button
    , elevated
    , filled
    , filledDisabled
    , outlined
    )

import Html exposing (Html)
import Html.Attributes exposing (class)


base : List (Html.Attribute msg)
base =
    [ class "px-6 py-2"
    , class "font-archivo font-semibold"
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


filledDisabled : List (Html.Attribute msg)
filledDisabled =
    [ class "bg-gray-100"
    , class "text-gray-400"
    , class "border-2 border-gray-300"
    , class "cursor-not-allowed"
    ]


button :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
button attrs html =
    Html.button (base ++ attrs) html


a :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
a attrs html =
    Html.a (base ++ attrs) html
