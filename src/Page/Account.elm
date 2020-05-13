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
import Html.Attributes as Attribute
import Html.Events as Event
import Session exposing (Session, Viewer)


type alias Model =
    { viewer : Viewer
    , nextUsername : String
    , nextPassword : String
    }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    ( { viewer = viewer, nextUsername = "", nextPassword = "" }, Cmd.none )


toSession : Model -> Session
toSession model =
    Session.toSession model.viewer



-- UPDATE


type Message
    = WriteNewUsername String
    | WriteNewPassword String


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewUsername username ->
            ( { model | nextUsername = username }, Cmd.none )

        WriteNewPassword password ->
            ( { model | nextPassword = password }, Cmd.none )



-- VIEW


view : Model -> List (Html Message)
view model =
    [ Html.input
        [ Attribute.placeholder "New username"
        , Attribute.value model.nextUsername
        , Event.onInput WriteNewUsername
        ]
        []
    , Html.button [] [ Html.text "Update username" ]
    , Html.input
        [ Attribute.placeholder "New password"
        , Attribute.value model.nextPassword
        , Event.onInput WriteNewPassword
        ]
        []
    , Html.button [] [ Html.text "Update password" ]
    , Html.button [] [ Html.text "Delete account" ]
    ]
