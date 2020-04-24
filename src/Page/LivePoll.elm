module Page.LivePoll exposing
    ( Message
    , Model
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Api.Polls exposing (PollDiscriminator)
import Api.Sessions exposing (ServerSession)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html)
import Session exposing (Session, Viewer)
import Time



-- MODEL


type Model
    = Loading Viewer PollDiscriminator
    | Loaded Viewer ServerSession
    | Error Viewer PollDiscriminator


toSession : Model -> Session
toSession model =
    case model of
        Loading viewer _ ->
            Session.toSession viewer

        Loaded viewer _ ->
            Session.toSession viewer

        Error viewer _ ->
            Session.toSession viewer


init :
    Viewer
    -> PollDiscriminator
    -> ( Model, Cmd Message )
init viewer discriminator =
    Loading viewer discriminator |> withCmd [ Cmd.succeed RequestPoll ]



-- UPDATE


type Message
    = RequestPoll


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        RequestPoll ->
            model |> withNoCmd


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 1000 (always RequestPoll)



-- VIEW


view : Model -> List (Html msg)
view _ =
    []
