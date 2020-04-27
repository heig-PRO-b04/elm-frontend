module Page.PollLive exposing
    ( Message
    , Model
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Api.Polls exposing (PollDiscriminator)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, text)
import Page.Poll.Session
import Session exposing (Session, Viewer)
import Time



-- MODEL


type Model
    = SessionInfo Viewer Page.Poll.Session.Model


toSession : Model -> Session
toSession (SessionInfo viewer _) =
    Session.toSession viewer


init :
    Viewer
    -> PollDiscriminator
    -> ( Model, Cmd Message )
init viewer discriminator =
    let
        ( model, cmd ) =
            Page.Poll.Session.init viewer discriminator
    in
    ( SessionInfo viewer model, Cmd.map SessionMsg cmd )



-- UPDATE


type Message
    = SessionMsg Page.Poll.Session.Message


update : Message -> Model -> ( Model, Cmd Message )
update (SessionMsg message) (SessionInfo viewer session) =
    let
        ( sessionModel, cmd ) =
            Page.Poll.Session.update message session
    in
    ( SessionInfo viewer sessionModel, Cmd.map SessionMsg cmd )


subscriptions : Model -> Sub Message
subscriptions (SessionInfo _ session) =
    Page.Poll.Session.subscriptions session
        |> Sub.map SessionMsg



-- VIEW


view : Model -> List (Html Message)
view (SessionInfo _ session) =
    List.map (Html.map SessionMsg) <| Page.Poll.Session.participantView session
