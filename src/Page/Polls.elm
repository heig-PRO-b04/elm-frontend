module Page.Polls exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Picasso.Button exposing (button, filled, outlined)
import Picasso.Text exposing (styledH2)
import Session exposing (Session)
import Task
import Task.Extra


type Message
    = GotNewPolls (List Poll)
    | RequestPolls


type alias Model =
    { session : Session
    , polls : List Poll
    }


init : Session -> ( Model, Cmd Message )
init session =
    { session = session
    , polls = []
    }
        |> withCmd [ Task.perform identity <| Task.succeed RequestPolls ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GotNewPolls polls ->
            { model | polls = polls } |> withNoCmd

        RequestPolls ->
            case Session.sessionCredentials model.session of
                Nothing ->
                    model |> withNoCmd

                Just credentials ->
                    let
                        cmd : Cmd Message
                        cmd =
                            Api.Polls.getAllPolls credentials identity
                                |> Task.mapError (always [])
                                |> Task.Extra.execute
                                |> Cmd.map GotNewPolls
                    in
                    model |> withCmd [ cmd ]


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
