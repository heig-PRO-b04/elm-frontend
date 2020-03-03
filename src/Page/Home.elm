module Page.Home exposing
    ( Message
    , Model
    , update
    )

import Session exposing (Session)


type alias Message =
    Never


type alias Model =
    { session : Session }


update : Message -> Model -> Model
update msg model =
    model
