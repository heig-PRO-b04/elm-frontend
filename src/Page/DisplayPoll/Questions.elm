module Page.DisplayPoll.Questions exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Message
    = Inc
    | Dec


init : Api.Polls.Poll -> ( Model, Cmd Message )
init poll =
    ( poll.idPoll, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Inc ->
            ( model + 1, Cmd.none )

        Dec ->
            ( model - 1, Cmd.none )


view : Model -> List (Html Message)
view model =
    [ Html.button [ onClick Inc, class "block" ] [ Html.text "Inc" ]
    , Html.button [ onClick Dec, class "block" ] [ Html.text "Dec" ]
    , Html.text ("Current value is " ++ String.fromInt model)
    ]
