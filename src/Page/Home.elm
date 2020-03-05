module Page.Home exposing
    ( Message
    , Model
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


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    model
        |> withNoCmd


view : Model -> Html Message
view model =
    text "Hello world"
