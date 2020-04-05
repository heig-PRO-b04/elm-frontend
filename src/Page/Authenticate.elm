module Page.Authenticate exposing
    ( Model, Message
    , update, view
    , initLogin, initRegistration
    )

{-|


# TEA

@docs Model, Message
@docs update, view


# Modes

@docs initLogin, initRegistration

-}

import Api
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html, br, div, p, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Picasso.Button exposing (button, elevated, filled, filledDisabled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Route
import Session exposing (Session)
import Task


type Message
    = WriteNewUsername String
    | WriteNewPassword String
    | ClickPerform
    | ClickSwitchModes
    | GotGoodCredentials Api.Credentials
    | GotBadNetwork
    | GotBadCredentials


{-| A union type representing the different kind of error states that the login
subsystem screen might currently be in. If there is no error, no associated
message will be displayed.
-}
type AuthState
    = NoError
    | Pending
    | Success
    | BadCredentials
    | BadNetwork


{-| The two modes that this component might be in. Depending on the mode,
the user interface components will be slightly different.
-}
type AuthMode
    = Login
    | Register


isError : AuthState -> Bool
isError state =
    List.member state [ BadCredentials, BadNetwork ]


loginMessage : AuthState -> Maybe String
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
    , state : AuthState
    , mode : AuthMode
    }


initLogin : Session -> Model
initLogin session =
    { session = session
    , username = ""
    , password = ""
    , state = NoError
    , mode = Login
    }


initRegistration : Session -> Model
initRegistration session =
    { session = session
    , username = ""
    , password = ""
    , state = NoError
    , mode = Register
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

        GotGoodCredentials credentials ->
            { model
                | state = Success
                , session =
                    model.session
                        |> Session.withCredentials credentials
            }
                |> withCmd
                    [ Route.replaceUrl
                        (Session.navKey model.session)
                        Route.Home
                    ]

        ClickPerform ->
            let
                api =
                    case model.mode of
                        Login ->
                            Api.login

                        Register ->
                            Api.register

                apiResult =
                    api model.username model.password identity

                apiMapper result =
                    case result of
                        Ok credentials ->
                            GotGoodCredentials credentials

                        Err error ->
                            case error of
                                Api.BadCredentials ->
                                    GotBadCredentials

                                Api.NetworkError ->
                                    GotBadNetwork
            in
            { model | state = Pending }
                |> withCmd [ Task.attempt apiMapper apiResult ]

        ClickSwitchModes ->
            let
                destination =
                    case model.mode of
                        Login ->
                            Route.Registration

                        Register ->
                            Route.Login
            in
            model
                |> withCmd
                    [ Route.replaceUrl
                        (Session.navKey model.session)
                        destination
                    ]

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
            , class "md:w-1/2"
            , class "md:max-w-lg"
            ]
            [ title model.mode
            , desc model.mode
            , inputEmail
            , inputPassword
            , buttonSignIn model.mode model.state
            , buttonSwitchModes model.mode
            , Html.map never <| errorView model.state
            ]
        ]


title : AuthMode -> Html msg
title mode =
    let
        message =
            case mode of
                Login ->
                    "Sign-in to Polls"

                Register ->
                    "New account"
    in
    Html.map never <| styledH2 message


desc : AuthMode -> Html Message
desc mode =
    let
        message =
            case mode of
                Login ->
                    [ text "Welcome back to rockin.app 👊"
                    , br [] []
                    , text "Our 👀 can't wait to see what you'll be polling about today \u{1F984}"
                    ]

                Register ->
                    [ text "⚡️ Registration in 8 seconds"
                    , br [] []
                    , text "🏗 Craft the poll of your dreams"
                    , br [] []
                    , text "🌈 Share it with your audience"
                    ]

        --""" Polls is an application that lets you submit
        --    multiple-choice questions to some participants and get
        --    answers in real-time."""
    in
    p
        [ class "font-archivo text-gray-700"
        ]
        message
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
    Input.inputWithTitle "Username:"
        [ onInput WriteNewUsername
        , placeholder "i-am-ironman"
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


buttonSignIn : AuthMode -> AuthState -> Html Message
buttonSignIn mode state =
    let
        fillIn =
            if state == Pending || state == Success then
                filledDisabled

            else
                filled ++ elevated ++ [ onClick ClickPerform ]

        message =
            case state of
                Pending ->
                    "Loading..."

                Success ->
                    "Success !"

                _ ->
                    case
                        mode
                    of
                        Login ->
                            "Sign-in"

                        Register ->
                            "Create account"
    in
    button
        (fillIn ++ [ class "mt-8" ])
        [ text message ]


buttonSwitchModes : AuthMode -> Html Message
buttonSwitchModes mode =
    let
        buttonText =
            case mode of
                Login ->
                    "Don't have an account ?"

                Register ->
                    "Already got an account ?"
    in
    button
        (elevated
            ++ [ class "mt-4"
               , onClick ClickSwitchModes
               ]
        )
        [ text buttonText ]


errorView : AuthState -> Html Never
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
