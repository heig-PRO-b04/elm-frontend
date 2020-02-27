module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Counter =
    Int


type Message
    = Decrement
    | Increment


init : () -> ( Counter, Cmd Message )
init _ =
    ( 42, Cmd.none )


subscriptions : Counter -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Counter -> ( Counter, Cmd Message )
update msg counter =
    let
        value =
            case msg of
                Decrement ->
                    counter - 1

                Increment ->
                    counter + 1
    in
    ( value, Cmd.none )


view : Counter -> Html Message
view counter =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , text <| String.fromInt counter
        , button [ onClick Decrement ] [ text "-" ]
        ]


main : Program () Counter Message
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
