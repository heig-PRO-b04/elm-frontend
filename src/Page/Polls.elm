module Page.Polls exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Cmd exposing (withNoCmd)
import Html exposing (Html, text)
import Session exposing (Session)


type alias Message =
    Never


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Message )
init session =
    { session = session }
        |> withNoCmd


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    model
        |> withNoCmd


view : Model -> List (Html Message)
view model =
    [ text "Hello from the Polls page" ]
