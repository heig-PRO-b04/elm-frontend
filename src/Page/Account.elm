module Page.Account exposing
    ( Message
    , Model
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

-- MODEL

import Html exposing (Html)
import Session exposing (Session, Viewer)


type alias Model =
    { viewer : Viewer }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    ( { viewer = viewer }, Cmd.none )


toSession : Model -> Session
toSession model =
    Session.toSession model.viewer



-- UPDATE


type alias Message =
    Never


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> List (Html Message)
view _ =
    []
