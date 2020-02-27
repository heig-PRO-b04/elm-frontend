module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, disabled, src)
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


placeholder : Counter -> Html Message
placeholder counter =
    let
        title =
            "Finding " ++ String.fromInt counter ++ " ideas for our project"
    in
    div
        [ class "md:flex"
        , class "rounded"
        , class "overflow-hidden"
        , class "shadow-lg"
        , class "container"
        ]
        [ div [ class "md:flex-shrink-0" ]
            [ img
                [ class "md:w-56"
                , src "https://heig-vd.ch/images/default-source/img-global-superpicture/default/heig-vd-site-web-lg-00081917--lg.jpg?sfvrsn=521582ea_2"
                ]
                []
            ]
        , div
            [ class "mt-4"
            , class "md:mt-0"
            , class "md:ml-6"
            ]
            [ div
                [ class "uppercase"
                , class "tracking-wide"
                , class "text-sm"
                , class "text-red-600"
                , class "font-bold"
                ]
                [ text "Marketing" ]
            , a
                [ class "block"
                , class "mt-1"
                , class "text-lg"
                , class "leading-tight"
                , class "font-semibold"
                , class "text-gray-900"
                , class "hover:underline"
                , onClick Increment
                ]
                [ text title ]
            , p [ class "mt-2", class "text-gray-600" ]
                [ text """
                This is just a sample card that we might want
                to use in the project. We'll see actually. By
                the way, pressing on the link will increment the
                counter value, and will therefore update the web
                page.
                """ ]
            ]
        ]


view : Counter -> Html Message
view counter =
    placeholder counter


main : Program () Counter Message
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
