module Page.Account exposing
    ( Model, Message
    , update, view
    , init, subscriptions, toSession
    )

{-| A page module that displays a user's account info and ways to modify these infos


# TEA

@docs Model, Message
@docs update, view


# functions

@docs init, subscriptions, toSession

-}

import Api as BaseApi
import Api.Account as Api
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Route
import Session exposing (Session, Viewer)
import Task exposing (Task)
import Task.Extra



-- MODEL


type alias Model =
    { viewer : Viewer
    , nextUsername : String
    , nextPassword : String
    , nextPasswordConfirmation : String
    , error : Maybe Error
    , confirmation : Maybe ( String, Info, String -> Cmd Message )
    }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    ( { viewer = viewer
      , nextUsername = Session.viewerCredentials viewer |> BaseApi.username
      , nextPassword = ""
      , nextPasswordConfirmation = ""
      , error = Nothing
      , confirmation = Nothing
      }
    , Cmd.none
    )


{-| Returns the session of the logged in user.
As this is a logged in only page, it is guaranteed to exist.
-}
toSession : Model -> Session
toSession model =
    Session.toSession model.viewer



-- UPDATE


type Error
    = Cancellation
    | BadCredentials
    | BadCommunication


type alias Info =
    List (Html Never)


type Message
    = WriteNewUsername String
    | WriteNewPassword String
    | WriteNewPasswordConfirmation String
    | WriteConfirmation (String -> Cmd Message) Info String
    | ClickUpdateUsername
    | ClickUpdatePassword
    | ClickDeleteAccount
    | GotError Error
    | NowConfirm (Cmd Message)
    | NowDisconnect


{-| Doesn't refresh the informations on the page
-}
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

        WriteNewPasswordConfirmation password ->
            ( { model | nextPasswordConfirmation = password }, Cmd.none )

        WriteConfirmation command info password ->
            ( { model | confirmation = Just ( password, info, command ) }, Cmd.none )

        ClickUpdateUsername ->
            ( { model | confirmation = Just ( "", updateUsernameInfo model, updateUsername model ) }, Cmd.none )

        ClickUpdatePassword ->
            ( { model | confirmation = Just ( "", updatePasswordInfo model, updatePassword model ) }, Cmd.none )

        ClickDeleteAccount ->
            ( { model | confirmation = Just ( "", deleteInfo model, delete model ) }, Cmd.none )

        GotError error ->
            ( { model | error = Just error, confirmation = Nothing }, Cmd.none )

        NowConfirm command ->
            ( { model | error = Nothing }, command )

        NowDisconnect ->
            ( model
            , Route.replaceUrl
                (Session.viewerNavKey model.viewer)
                Route.Logout
            )



-- EFFECTS


handleError : Api.Error -> Message
handleError error =
    GotError <|
        case error of
            Api.GotNotFound ->
                BadCommunication

            Api.GotInvalidCredentials ->
                BadCredentials

            Api.GotBadNetwork ->
                BadCommunication


handleErrorAndThenDisconnect : Task Api.Error a -> Cmd Message
handleErrorAndThenDisconnect task =
    Task.mapError handleError task
        |> Task.andThen (always <| Task.succeed NowDisconnect)
        |> Task.Extra.execute


updatePassword : Model -> (String -> Cmd Message)
updatePassword model =
    \pwd ->
        handleErrorAndThenDisconnect <|
            Api.updatePassword
                (Session.viewerCredentials model.viewer)
                model.nextPassword
                pwd
                identity


updateUsername : Model -> (String -> Cmd Message)
updateUsername model =
    \pwd ->
        handleErrorAndThenDisconnect <|
            Api.updateUsername
                (Session.viewerCredentials model.viewer)
                model.nextUsername
                pwd
                identity


delete : Model -> String -> Cmd Message
delete model =
    \pwd ->
        handleErrorAndThenDisconnect <|
            Api.deleteAccount
                (Session.viewerCredentials model.viewer)
                pwd
                identity



-- INFO


updateUsernameInfo : Model -> Info
updateUsernameInfo model =
    [ Html.text "You're about to "
    , Html.span [ Attribute.class "font-bold" ] [ Html.text "update your username" ]
    , Html.text " and set it to "
    , Html.span [ Attribute.class "font-semibold" ] [ Html.text model.nextUsername ]
    , Html.text ". Please enter your current password below to confirm this update :"
    ]


updatePasswordInfo : Model -> Info
updatePasswordInfo model =
    let
        hidden =
            String.left 1 model.nextPassword
                ++ String.repeat (String.length model.nextPassword - 2) "•"
                ++ String.right 1 model.nextPassword
    in
    [ Html.text "You're about to "
    , Html.span [ Attribute.class "font-bold" ] [ Html.text "update your password" ]
    , Html.text " and set it to "
    , Html.span [ Attribute.class "font-semibold text-seaside-600" ] [ Html.text hidden ]
    , Html.text ". "
    , Html.text "Please enter your current password below to confirm this update :"
    ]


deleteInfo : Model -> Info
deleteInfo _ =
    [ Html.text "You're about to "
    , Html.span [ Attribute.class "font-bold" ] [ Html.text "delete your account" ]
    , Html.text " and remove all of your polls. "
    , Html.text "Please enter your current password below to confirm this deletion :"
    ]



-- VIEW


view : Model -> List (Html Message)
view model =
    case model.confirmation of
        Just ( password, info, command ) ->
            confirmation password info command

        Nothing ->
            inputs model


confirmation : String -> Info -> (String -> Cmd Message) -> List (Html Message)
confirmation password info command =
    List.singleton <| confirmationDialog password info command


confirmationDialog : String -> Info -> (String -> Cmd Message) -> Html Message
confirmationDialog password info command =
    Html.div
        [ Attribute.class "mt-8 mb-8 bg-white shadow w-full md:max-w-2xl m-auto p-8 md:rounded-lg"
        , Attribute.class "flex flex-col items-justify"
        ]
        [ Html.h1
            [ Attribute.class "text-2xl font-archivo font-bold" ]
            [ Html.text "Just to be sure" ]
        , Html.p
            [ Attribute.class "text-lg font-archivo font-light mt-4" ]
            (List.map (Html.map never) info)
        , Html.input
            [ Attribute.placeholder "Current password..."
            , Attribute.type_ "password"
            , Attribute.class "font-archivo font-semibold"
            , Attribute.class "mt-8 border-2 border-seaside-300 rounded-lg px-4 py-2"
            , Attribute.class "focus:outline-none focus:shadow-outline"
            , Attribute.value password
            , Event.onInput (WriteConfirmation command info)
            ]
            []
        , Html.p
            [ Attribute.class "text-lg font-archivo font-light mt-8" ]
            [ Html.text "All your (active) sessions will be disconnected upon confirmation." ]
        , Html.div
            [ Attribute.class "mt-8"
            , Attribute.class "flex flex-row justify-around"
            ]
            [ Html.button
                [ Attribute.class "border-2 border-gray-200 text-gray-600  hover:bg-gray-100"
                , Attribute.class "rounded-lg px-6 py-2 transform duration-200"
                , Attribute.class "font-archivo font-semibold"
                , Attribute.class "mr-4"
                , Attribute.class "flex-grow"
                , Event.onClick <| GotError Cancellation
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ Attribute.class "border-2 border-red-200 text-red-500 hover:bg-red-100"
                , Attribute.class "rounded-lg px-6 py-2 transform duration-200"
                , Attribute.class "font-archivo font-semibold"
                , Attribute.class "ml-4"
                , Attribute.class "flex-grow"
                , Event.onClick <| NowConfirm (command password)
                ]
                [ Html.text "Confirm" ]
            ]
        ]


errorCard : Maybe Error -> Html Message
errorCard error =
    case error of
        Just Cancellation ->
            Html.div
                [ Attribute.class "bg-yellow-200 rounded p-4 mb-4 border-2 border-yellow-300" ]
                [ Html.h2
                    [ Attribute.class "font-archivo text-xl font-semibold text-yellow-700" ]
                    [ Html.text "Nothing was done" ]
                , Html.span
                    [ Attribute.class " font-archivo text-yellow-600" ]
                    [ Html.text "The operation was cancelled and not performed." ]
                ]

        Just BadCommunication ->
            Html.div
                [ Attribute.class "bg-red-200 rounded p-4 mb-4 border-2 border-red-300" ]
                [ Html.h2
                    [ Attribute.class "font-archivo text-xl font-semibold text-red-700" ]
                    [ Html.text "Bad connection" ]
                , Html.span
                    [ Attribute.class "font-archivo text-red-600" ]
                    [ Html.text "Something is off with your connection to the server." ]
                ]

        Just BadCredentials ->
            Html.div
                [ Attribute.class "bg-red-200 rounded p-4 mb-4 border-2 border-red-300" ]
                [ Html.h2
                    [ Attribute.class "font-archivo text-xl font-semibold text-red-700" ]
                    [ Html.text "Bad credentials" ]
                , Html.span
                    [ Attribute.class "font-archivo text-red-600" ]
                    [ Html.text "Something went wrong with authentication. Are you still connected, and was your password correct ?" ]
                ]

        Nothing ->
            Html.text ""


inputs : Model -> List (Html Message)
inputs model =
    [ Html.div
        [ Attribute.class "mt-8 mb-8 w-full md:max-w-2xl m-auto px-4 py-8 md:px-8"
        , Attribute.class "flex flex-col font-archivo"
        , Attribute.class "bg-white md:rounded-lg shadow"
        ]
        [ Html.h1
            [ Attribute.class "pb-8 text-3xl font-semibold"
            ]
            [ Html.text "Update your profile"
            ]

        -- ERROR UI
        , errorCard model.error

        -- USERNAME UI
        , Html.h2
            [ Attribute.class "border-t-2 pt-4 border-gray-300"
            , Attribute.class "text-xl text-gray-800 mb-2"
            ]
            [ Html.text "Username" ]
        , Html.span
            [ Attribute.class "text-gray-600 pb-4" ]
            [ Html.text """Your username is your unique identifier in the app, and is used for
            login. It's also unique amongst all the users of rockin.app. It must be at least 4 characters long.""" ]
        , input
            [ Attribute.placeholder "New username"
            , Attribute.value model.nextUsername
            , Event.onInput WriteNewUsername
            ]
            []
        , Html.button
            [ Attribute.class "self-start"
            , Attribute.class "bg-white px-4 py-2 rounded-lg my-8"
            , Attribute.class "border-2 border-gray-300 text-gray-700 font-archivo font-semibold"
            , Attribute.class "hover:bg-seaside-600 hover:border-seaside-700 hover:shadow hover:text-white"
            , Attribute.class "transform duration-200"
            , Event.onClick ClickUpdateUsername
            ]
            [ Html.text "Update username" ]

        -- PASSWORD UI
        , Html.h2
            [ Attribute.class "border-t-2 pt-4 border-gray-300"
            , Attribute.class "text-xl text-gray-800 mb-2"
            ]
            [ Html.text "Password" ]
        , Html.span
            [ Attribute.class "text-gray-600 pb-4" ]
            [ Html.text """A strong and unique password guarantees that only you can access your
            polls. Don't share it with anyone 🔐 It must be at least 4 characters long.""" ]
        , input
            [ Attribute.placeholder "New password"
            , Attribute.value model.nextPassword
            , Event.onInput WriteNewPassword
            ]
            []
        , input
            [ Attribute.placeholder "Confirm your password"
            , Attribute.value model.nextPasswordConfirmation
            , Attribute.class "mt-2"
            , Event.onInput WriteNewPasswordConfirmation
            ]
            []
        , let
            style =
                if model.nextPassword == model.nextPasswordConfirmation then
                    [ Attribute.class "self-start"
                    , Attribute.class "bg-white px-4 py-2 rounded-lg my-8"
                    , Attribute.class "border-2 border-gray-300 text-gray-700 font-archivo font-semibold"
                    , Attribute.class "hover:bg-seaside-600 hover:border-seaside-700 hover:shadow hover:text-white"
                    , Attribute.class "transform duration-200"
                    , Event.onClick ClickUpdatePassword
                    ]

                else
                    [ Attribute.class "self-start"
                    , Attribute.class "bg-white px-4 py-2 rounded-lg my-8"
                    , Attribute.class "border-2 border-red-200 text-red-500 font-archivo font-semibold"
                    , Attribute.class "cursor-not-allowed"
                    , Attribute.class "transform duration-200"
                    ]

            contents =
                if model.nextPassword == model.nextPasswordConfirmation then
                    "Update password"

                else
                    "Non-matching passwords"
          in
          Html.button
            style
            [ Html.text contents ]
        , Html.div [ Attribute.class "border-t-2 pt-4 border-gray-300 border-dashed" ] []
        , Html.button
            [ Attribute.class "self-start"
            , Attribute.class "bg-red-600 px-4 py-2 rounded-lg mt-4"
            , Attribute.class "border-2 border-red-700 text-white font-archivo font-semibold"
            , Attribute.class "hover:bg-red-700 hover:border-red-800 hover:shadow hover:text-white"
            , Attribute.class "transform duration-200"
            , Event.onClick ClickDeleteAccount
            ]
            [ Html.text "Delete account" ]
        ]
    ]


input : List (Html.Attribute msg) -> List (Html msg) -> Html msg
input attrs contents =
    let
        base =
            [ Attribute.class "border-2 rounded-lg py-2 px-4" ]
    in
    Html.input (base ++ attrs) contents
