module Page.Login exposing
    ( Model, Message
    , init, update, view
    )

{-|


# TEA

@docs Model, Message
@docs init, update, view

-}

import Browser.Navigation as Nav
import Cmd exposing (withNoCmd)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Picasso.Button exposing (button, elevated, filled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Session exposing (Session, guest)


type Message
    = WriteNewUsername String
    | WriteNewPassword String
    | ClickLogin


type alias Model =
    { session : Session
    , username : String
    , password : String
    }


init : Nav.Key -> Model
init key =
    { session = guest key
    , username = ""
    , password = ""
    }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewUsername username ->
            { model | username = username }
                |> withNoCmd

        WriteNewPassword password ->
            { model | password = password }
                |> withNoCmd

        -- TODO : Side-effects and navigation.
        ClickLogin ->
            model
                |> withNoCmd


view : Model -> Html Message
view model =
    div
        [ class "flex flex-col md:items-center md:justify-center"
        , class "h-screen w-screen"
        , class "bg-white md:bg-cactus-050"
        ]
        [ div
            [ class "flex flex-col"

            -- Card appearance
            , class "bg-white"
            , class "md:shadow"
            , class "p-8"
            , class "md:rounded-lg"
            , class "md:max-w-lg"
            ]
            [ Html.map never <| styledH2 "Sign-in to Polls"
            , desc
            , inputEmail
            , inputPassword
            , buttonSignIn
            ]
        ]


desc : Html Message
desc =
    p
        [ class "font-archivo text-gray-700"
        ]
        [ text """Polls is an application that lets you submit
        multiple-choice questions to some participants and get answers in
        real-time.""" ]
        |> withMargin


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]


withHalfMargin : Html msg -> Html msg
withHalfMargin html =
    div [ class "mt-4" ]
        [ html ]


inputEmail : Html Message
inputEmail =
    Input.inputWithTitle "Email address:"
        [ onInput WriteNewUsername
        , type_ "email"
        , placeholder "john.appleseed@example.org"
        ]
        []
        |> withMargin


inputPassword : Html Message
inputPassword =
    Input.inputWithTitle "Password:"
        [ onInput WriteNewPassword
        , type_ "password"
        , placeholder "g3n3r4l_k4n0b1"
        ]
        []
        |> withMargin


buttonSignIn : Html Message
buttonSignIn =
    button
        (filled ++ elevated ++ [ class "mt-8", onClick ClickLogin ])
        [ text "Sign-in" ]
