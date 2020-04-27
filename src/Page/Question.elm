module Page.Question exposing
    ( Message
    , Model
    , update
    )

import Html exposing (Html)


type alias Model =
    ()



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
