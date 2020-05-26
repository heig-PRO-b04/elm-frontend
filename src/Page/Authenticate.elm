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
import Cmd.Extra exposing (withCmds, withNoCmd)
import Html exposing (Html, br, div, p, text)
import Html.Attributes as Attribute exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnterDown)
import Picasso.Button exposing (button, elevated, filled, filledDisabled, outlinedLight)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Route
import Session exposing (Session)
import Task
import Task.Extra


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
            Just "There was a network issue. Try again with a different username or check your connection ?"

        _ ->
            Nothing


type alias Model =
    { session : Session
    , username : String
    , password : String
    , state : AuthState
    , mode : AuthMode
    }


{-| An init function to use when a user wishes to use the page to log in
-}
initLogin : Session -> ( Model, Cmd Message )
initLogin session =
    { session = session
    , username = ""
    , password = ""
    , state = NoError
    , mode = Login
    }
        |> withNoCmd


{-| An init function to use when a user wishes to use the page to register
-}
initRegistration : Session -> ( Model, Cmd Message )
initRegistration session =
    { session = session
    , username = ""
    , password = ""
    , state = NoError
    , mode = Register
    }
        |> withNoCmd


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
                |> withCmds
                    [ Api.storeCredentials credentials
                    , Route.replaceUrl
                        (Session.sessionNavKey model.session)
                        Route.Polls
                    ]

        ClickPerform ->
            let
                api =
                    case model.mode of
                        Login ->
                            Api.login

                        Register ->
                            Api.register

                errorMapper error =
                    case error of
                        Api.BadCredentials ->
                            GotBadCredentials

                        Api.NetworkError ->
                            GotBadNetwork

                apiResult =
                    api model.username model.password GotGoodCredentials
                        |> Task.mapError errorMapper
            in
            { model | state = Pending }
                |> withCmds [ Task.Extra.execute apiResult ]

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
                |> withCmds
                    [ Route.replaceUrl
                        (Session.sessionNavKey model.session)
                        destination
                    ]

        GotBadCredentials ->
            { model | state = BadCredentials }
                |> withNoCmd

        GotBadNetwork ->
            { model | state = BadNetwork }
                |> withNoCmd


view : Model -> List (Html Message)
view model =
    [ div
        [ class "flex flex-col"
        , class "m-auto mt-4 md:mt-16 mb-4 md:mb-16"

        -- Card appearance
        , class "bg-white"
        , class "shadow"
        , class "p-8"
        , class "md:rounded-lg"
        , class "md:w-1/2"
        , class "md:max-w-lg"
        ]
        [ title model.mode
        , desc model.mode
        , inputEmail <| model.username
        , inputPassword <| model.password
        , Html.map never <| errorView model.state
        , buttonSignIn model.mode model.state
        , buttonSwitchModes model.mode
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
                    [ text "⚡️ Register in 8 seconds"
                    , br [] []
                    , text "🏗 Craft the poll of your dreams"
                    , br [] []
                    , text "🌈 Share it with your audience"
                    ]
    in
    p
        [ class "font-archivo text-gray-700"
        , class "mt-4"
        ]
        message


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]


inputEmail : String -> Html Message
inputEmail content =
    Input.inputWithTitle "Username:"
        [ onInput WriteNewUsername
        , placeholder "john.appleseed (min. 4 characters)"
        , Attribute.autofocus True
        , value content
        ]
        []
        |> withMargin


inputPassword : String -> Html Message
inputPassword content =
    Input.inputWithTitle "Password:"
        [ onInput WriteNewPassword
        , onEnterDown ClickPerform
        , type_ "password"
        , placeholder "superpassword (min. 4 characters)"
        , value content
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
            ++ outlinedLight
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
