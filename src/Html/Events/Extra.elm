module Html.Events.Extra exposing (onEnterDown)

{-| A module that provides additional Html events


# Events

@docs onEnterDown

-}

import Html
import Html.Events as Events
import Json.Decode


{-| Function that will trigger an event when the down key is pressed
-}
onEnterDown : msg -> Html.Attribute msg
onEnterDown action =
    Events.on "keydown" (ifIsEnter action)



-- MATCHERS


ifIsEnter : msg -> Json.Decode.Decoder msg
ifIsEnter msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                if key == "Enter" then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "Not the enter key."
            )
