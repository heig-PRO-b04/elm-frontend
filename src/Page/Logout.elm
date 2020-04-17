module Page.Logout exposing
    ( Model
    , init
    , update
    , view
    )

import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Never )
init session =
    { session = Session.guest <| Session.sessionNavKey session }
        |> withCmd [ Route.replaceUrl (Session.sessionNavKey session) Route.Home ]


update : msg -> Model -> ( Model, Cmd Never )
update _ model =
    model |> withNoCmd


view : Model -> List (Html Never)
view _ =
    []
