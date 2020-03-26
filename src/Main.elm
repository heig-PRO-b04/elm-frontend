module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cmd exposing (updateWith, withCmd, withNoCmd)
import Html
import Page.Home as Home
import Page.Login as Login
import Route exposing (Route)
import Session exposing (Session)
import Url



-- MODEL


type Model
    = LoginModel Login.Model
    | HomeModel Home.Model



-- TODO : Start from a blank page.


{-| Returns the Session associated with the current model. This information
will be passed around the different sub-models and acts as the shared
information for the lifetime of the application.
-}
toSession : Model -> Session
toSession model =
    case model of
        LoginModel m ->
            m.session

        HomeModel m ->
            m.session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    ( LoginModel <| Login.init <| Session.guest key, Cmd.none )



-- VIEW


view : Model -> Browser.Document Message
view model =
    { title = "heig-PRO-b04 | Live polls"
    , body =
        [ case model of
            LoginModel loginModel ->
                Login.view loginModel
                    |> Html.map LoginMessage

            HomeModel homeModel ->
                Home.view homeModel
                    |> Html.map HomeMessage
        ]
    }



-- UPDATE


type Message
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | HomeMessage Home.Message
    | LoginMessage Login.Message


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink request, _ ) ->
            case request of
                Browser.Internal url ->
                    model
                        |> withCmd
                            [ Nav.pushUrl
                                (Session.navKey (toSession model))
                                (Url.toString url)
                            ]

                Browser.External href ->
                    model |> withCmd [ Nav.load href ]

        ( LoginMessage loginMsg, LoginModel loginModel ) ->
            updateWith
                LoginMessage
                LoginModel
                Login.update
                loginMsg
                loginModel

        ( HomeMessage homeMsg, HomeModel homeModel ) ->
            updateWith
                HomeMessage
                HomeModel
                Home.update
                homeMsg
                homeModel

        ( _, _ ) ->
            model
                |> withNoCmd


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Nothing ->
            HomeModel (Home.init session) |> withNoCmd

        Just Route.Home ->
            HomeModel (Home.init session) |> withNoCmd

        Just Route.Login ->
            LoginModel (Login.init session) |> withNoCmd



-- APPLICATION


main : Program () Model Message
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
