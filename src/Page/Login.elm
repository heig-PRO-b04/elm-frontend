module Page.Login exposing
    ( Model, Message
    , init, update, view
    )

{-|


# TEA

@docs Model, Message
@docs init, update, view

-}

import Api
import Browser.Navigation as Nav
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Picasso.Button exposing (button, elevated, filled, filledDisabled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Session exposing (Session, guest)
import Task


type Message
    = WriteNewUsername String
    | WriteNewPassword String
    | ClickLogin
    | GotGoodLogin
    | GotBadNetwork
    | GotBadCredentials


{-| A union type representing the different kind of error states that the login
subsystem screen might currently be in. If there is no error, no associated
message will be displayed.
-}
type LoginState
    = NoError
    | Pending
    | Success
    | BadCredentials
    | BadNetwork


isError : LoginState -> Bool
isError state =
    List.member state [ BadCredentials, BadNetwork ]


loginMessage : LoginState -> Maybe String
loginMessage state =
    case state of
        BadCredentials ->
            Just "These credentials are not valid, sorry !"

        BadNetwork ->
            Just "There was a network issue. Try again later ?"

        _ ->
            Nothing


type alias Model =
    { session : Session
    , username : String
    , password : String
    , state : LoginState
    }


init : Nav.Key -> Model
init key =
    { session = guest key
    , username = ""
    , password = ""
    , state = NoError
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

        -- TODO : Navigation.
        -- TODO : Retrieve credentials.
        GotGoodLogin ->
            { model | state = Success }
                |> withNoCmd

        ClickLogin ->
            let
                apiResult : Task.Task Api.LoginError Api.Credentials
                apiResult =
                    Api.login model.username model.password identity

                apiMapper result =
                    case result of
                        Ok _ ->
                            GotGoodLogin

                        Err error ->
                            case error of
                                Api.BadCredentials ->
                                    GotBadCredentials

                                Api.NetworkError ->
                                    GotBadNetwork

                loginResult : Cmd Message
                loginResult =
                    Task.attempt apiMapper apiResult
            in
            { model | state = Pending }
                |> withCmd [ loginResult ]

        GotBadCredentials ->
            { model | state = BadCredentials }
                |> withNoCmd

        GotBadNetwork ->
            { model | state = BadNetwork }
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
            , buttonSignIn <| model.state
            , Html.map never <| errorView model.state
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


buttonSignIn : LoginState -> Html Message
buttonSignIn state =
    let
        fillIn =
            if state == Pending || state == Success then
                filledDisabled

            else
                filled ++ elevated ++ [ onClick ClickLogin ]

        message =
            case state of
                Pending ->
                    "Signing in..."

                Success ->
                    "Success !"

                _ ->
                    "Sign-in"
    in
    button
        (fillIn ++ [ class "mt-8" ])
        [ text message ]


errorView : LoginState -> Html Never
errorView state =
    let
        attrs =
            if isError state then
                [ class "mt-2"
                , class "text-red-500"
                ]

            else
                [ class "hidden" ]

        message =
            loginMessage state
                |> Maybe.withDefault ""
    in
    div attrs [ text message ]
