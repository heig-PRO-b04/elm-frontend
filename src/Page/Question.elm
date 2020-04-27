module Page.Question exposing
    ( Message
    , Model
    , init
    , subscriptions
    , update
    , view
    , viewIndex
    )

import Html exposing (Html)
import Session exposing (Viewer)



-- MODEL


type alias Model =
    ()


init : Viewer -> { d | idPoll : Int, idQuestion : Int } -> ( Model, Cmd Message )
init _ _ =
    ( (), Cmd.none )



-- UPDATE


type alias Message =
    Never


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


subscriptions : Sub Message
subscriptions =
    Sub.none



-- VIEW


view : Model -> Html Message
view model =
    Html.text "Hello world"


viewIndex : Model -> Float
viewIndex _ =
    0
