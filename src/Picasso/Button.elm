module Picasso.Button exposing
    ( a, button
    , filled, filledDisabled, outlined, outlinedLight, elevated
    )

{-| A module that provides basic button elements, with varying styling options.


# Constructors

If you want to use Route-based hrefs for navigation, it is highly recommended
that you use an `a` based button.

@docs a, button


# Styling

@docs filled, filledDisabled, outlined, outlinedLight, elevated

-}

import Html exposing (Html)
import Html.Attributes exposing (class)



-- BASE STYLING


{-| The base style for the buttons, that will be applied for all the types of
buttons of the application.
-}
base : List (Html.Attribute msg)
base =
    [ class "px-6 py-2"
    , class "font-archivo font-semibold"
    , class "rounded-md"
    ]



-- CUSTOM STYLING


{-| Some attributes that will outline a button.
-}
outlined : List (Html.Attribute msg)
outlined =
    [ class "bg-white hover:bg-seaside-100"
    , class "text-seaside-500 hover:text-seaside-600"
    , class "border-2 border-seaside-500"
    ]


outlinedLight : List (Html.Attribute msg)
outlinedLight =
    [ class "bg-white hover:bg-seaside-100"
    , class "text-seaside-500 hover:text-seaside-600"
    , class "border-2 border-seaside-100"
    ]


{-| Some attributes that will elevate a button.
-}
elevated : List (Html.Attribute msg)
elevated =
    [ class "rounded-md"
    , class "shadow"
    ]


{-| Some attributes that will fill a button.
-}
filled : List (Html.Attribute msg)
filled =
    [ class "bg-seaside-500 hover:bg-seaside-400"
    , class "text-seaside-050 hover:text-white"
    , class "border-2 border-seaside-500"
    ]


{-| Some attributes that will fill a button and make it look disabled.
-}
filledDisabled : List (Html.Attribute msg)
filledDisabled =
    [ class "bg-gray-100"
    , class "text-gray-400"
    , class "border-2 border-gray-300"
    , class "cursor-not-allowed"
    ]



-- HTML TAGS


{-| Builds a new button element with base styling.
-}
button :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
button attrs html =
    Html.button (base ++ attrs) html


{-| Builds a new link-based button element with base styling.
-}
a :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
a attrs html =
    Html.a (base ++ attrs) html
