module Page.Polls exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll)
import Cmd exposing (withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Picasso.Button exposing (button, filled, outlined)
import Picasso.Text exposing (styledH2)
import Session exposing (Session)


type alias Message =
    Never


type alias Model =
    { session : Session
    , polls : List Poll
    }


init : Session -> ( Model, Cmd Message )
init session =
    { session = session
    , polls =
        case Session.extractCredentials session of
            Just credentials ->
                [ Poll 1 1 "This is a poll"
                , Poll 1 1 "This is another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                , Poll 1 1 "This is yet another poll"
                ]

            Nothing ->
                []
    }
        |> withNoCmd


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    model
        |> withNoCmd


view : Model -> List (Html Message)
view model =
    [ div
        [ class "flex flex-col" ]
        ([ div
            [ class "flex flex-row justify-center" ]
            [ actionButton "Create a new poll"
            , actionButton "See your archived polls"
            ]
         , div
            [ class "flex flex-row justify-center"
            , class "px-3 pb-3"
            ]
            [ styledH2 "Your polls:" ]
         ]
            ++ displayPolls model.polls
        )
    ]


displayPolls : List Poll -> List (Html msg)
displayPolls polls =
    List.map (\poll -> displayPoll poll) polls


displayPoll : Poll -> Html msg
displayPoll poll =
    div
        [ class "flex flex-row" ]
        [ button
            (outlined
                ++ [ class "flex-grow"
                   , class "mb-2 mx-2"
                   ]
            )
            [ text poll.title ]
        , button
            (filled
                ++ [ class "flex-shrink"
                   , class "mb-2 mr-2"
                   ]
            )
            [ text "arch" ]
        , button
            (filled
                ++ [ class "flex-shrink"
                   , class "mb-2 mr-2"
                   ]
            )
            [ text "del" ]
        ]


actionButton content =
    button
        (filled
            ++ [ class "m-4"
               ]
        )
        [ text content ]
