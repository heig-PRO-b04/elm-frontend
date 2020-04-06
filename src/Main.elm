module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cmd exposing (initWith, updateWith, withCmd, withNoCmd)
import Html
import Page.Authenticate as Auth
import Page.Home as Home
import Route exposing (Route)
import Session exposing (Session)
import Url



-- MODEL


type Model
    = AuthModel Auth.Model
    | HomeModel Home.Model


{-| Returns the Session associated with the current model. This information
will be passed around the different sub-models and acts as the shared
information for the lifetime of the application.
-}
toSession : Model -> Session
toSession model =
    case model of
        AuthModel m ->
            m.session

        HomeModel m ->
            m.session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    initWith
        HomeMessage
        HomeModel
        (Home.init (Session.guest key))



--( LoginModel <| Login.init <| Session.guest key, Cmd.none )
-- VIEW


view : Model -> Browser.Document Message
view model =
    { title = "heig-PRO-b04 | Live polls"
    , body =
        case model of
            AuthModel authModel ->
                Auth.view authModel
                    |> List.map (Html.map AuthMessage)

            HomeModel homeModel ->
                Home.view homeModel
                    |> List.map (Html.map HomeMessage)
    }



-- UPDATE


type Message
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | HomeMessage Home.Message
    | AuthMessage Auth.Message


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

        ( AuthMessage authMsg, AuthModel authModel ) ->
            updateWith
                AuthMessage
                AuthModel
                Auth.update
                authMsg
                authModel

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


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Message )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Nothing ->
            initWith
                HomeMessage
                HomeModel
                (Home.init session)

        Just Route.Home ->
            initWith
                HomeMessage
                HomeModel
                (Home.init session)

        Just Route.Login ->
            initWith
                AuthMessage
                AuthModel
                (Auth.initLogin session)

        Just Route.Registration ->
            initWith
                AuthMessage
                AuthModel
                (Auth.initRegistration session)



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
