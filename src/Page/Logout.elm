module Page.Logout exposing
    ( Model
    , init
    , toSession
    , update
    , view
    )

import Api
import Cmd.Extra exposing (withCmds, withNoCmd)
import Html exposing (Html)
import Route
import Session exposing (Session)


type alias Model =
    Session


toSession : Model -> Session
toSession =
    identity


init : Session -> ( Model, Cmd Never )
init session =
    (Session.guest <| Session.sessionNavKey session)
        |> withCmds
            [ Api.storeCredentialsClear
            , Route.replaceUrl (Session.sessionNavKey session) Route.Home
            ]


update : msg -> Model -> ( Model, Cmd Never )
update _ model =
    model |> withNoCmd


view : Model -> List (Html Never)
view _ =
    []
