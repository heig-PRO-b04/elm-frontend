module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Picasso.Button exposing (button, elevated, outlined)


type alias Counter =
    Int


type Message
    = Decrement
    | Increment


init : () -> ( Counter, Cmd Message )
init _ =
    ( 0, Cmd.none )


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
    div
        [ class "h-screen w-screen"
        , class "flex flex-col items-center justify-center"
        ]
        [ button
            (elevated ++ outlined ++ [ onClick Increment ])
            [ text <| String.fromInt counter ]
        ]


main : Program () Counter Message
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
