module Page.PollLive exposing
    ( Model, Message
    , update, view
    , init, toSession
    , subscriptions
    )

{-| A page module that displays the participant view for a poll (Emoji code and QR code)


# TEA

@docs Model, Message
@docs update, view


# functions

@docs init, toSession

-}

import Api.Polls exposing (PollDiscriminator)
import Html exposing (Html)
import Page.Poll.Session
import Session exposing (Session, Viewer)



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
