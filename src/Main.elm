module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cmd exposing (updateWith, withNoCmd)
import Html
import Page.Home
import Page.Login
import Session exposing (Session)
import Url


type Model
    = LoginModel Page.Login.Model


type Message
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | LoginMessage Page.Login.Message
    | HomeMessage Page.Home.Message


{-| Returns the Session associated with the current model. This information
will be passed around the different sub-models and acts as the shared
information for the lifetime of the application.
-}
toSession : Model -> Session
toSession model =
    case model of
        LoginModel m ->
            m.session



-- TEA


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    ( LoginModel <| Page.Login.init key, Cmd.none )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case ( msg, model ) of
        -- TODO : Factorize sub-updates based on the rest of the app.
        ( LoginMessage loginMsg, LoginModel loginModel ) ->
            updateWith
                LoginMessage
                LoginModel
                Page.Login.update
                loginMsg
                loginModel

        -- TODO : Handle url changes and more.
        ( _, _ ) ->
            model
                |> withNoCmd


view : Model -> Browser.Document Message
view (LoginModel model) =
    { title = "heig-PRO-b04"
    , body =
        [ Page.Login.view model
            |> Html.map LoginMessage
        ]
    }



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
