module Page.Home exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Browser.Navigation as Nav
import Cmd exposing (withNoCmd)
import Html exposing (Html, text)
import Session exposing (Session, guest)


type alias Message =
    Never


type alias Model =
    { session : Session }


init : Session -> Model
init session =
    { session = session }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    model
        |> withNoCmd


view : Model -> Html Message
view model =
    text "Hello world"
