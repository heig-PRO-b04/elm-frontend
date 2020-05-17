module Main exposing (main)

import Api
import Browser
import Browser.Navigation as Nav
import Cmd.Extra exposing (initWith, updateWith, withCmds, withNoCmd)
import Html
import Json.Decode
import Page.Account as Prof
import Page.Authenticate as Auth
import Page.BadCredentials as Disc
import Page.Help as Help
import Page.Home as Home
import Page.Logout as Quit
import Page.Poll as DisplayPoll
import Page.PollList as Poll
import Page.PollLive as Live
import Picasso.Navigation as NavUI
import Route exposing (Route)
import Session exposing (Session)
import Url



-- MODEL


type alias Model =
    { page : PageModel
    , navi : NavUI.Model
    }


type PageModel
    = AuthModel Auth.Model
    | HomeModel Home.Model
    | DiscModel Disc.Model
    | QuitModel Quit.Model
    | ProfModel Prof.Model
    | HelpModel Help.Model
    | PollModel Poll.Model
    | DisplayPollModel DisplayPoll.Model
    | LiveModel Live.Model



-- MODEL EMBEDDING


{-| Given a certain model that displays a page, and a mapping from a model to
a page, produces a function that makes it possible to update the page of the
tuple as needed.

This is in essence like a function to partially map a record and keep its
untouched values.

-}
embed : Model -> (a -> PageModel) -> a -> Model
embed model toModel =
    \m ->
        let
            pageModel =
                toModel m
        in
        { model
            | page = pageModel
            , navi = model.navi |> NavUI.withSession (toSession pageModel)
        }


{-| Embeds navigation model changes into an existing model instance.
-}
embedNav : Model -> (a -> NavUI.Model) -> a -> Model
embedNav model toModel =
    \m ->
        let
            navModel =
                toModel m
        in
        { model | navi = navModel }


{-| Returns the Session associated with the current model. This information
will be passed around the different sub-models and acts as the shared
information for the lifetime of the application.
-}
toSession : PageModel -> Session
toSession model =
    case model of
        AuthModel m ->
            m.session

        HomeModel m ->
            Home.toSession m

        DiscModel m ->
            Disc.toSession m

        QuitModel m ->
            Quit.toSession m

        ProfModel m ->
            Prof.toSession m

        HelpModel m ->
            Help.toSession m

        PollModel m ->
            Session.toSession m.viewer

        DisplayPollModel m ->
            Session.toSession m.viewer

        LiveModel m ->
            Live.toSession m


init : Maybe Api.Credentials -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init credentials url key =
    let
        session =
            case credentials of
                Just unwrapped ->
                    Session.guest key |> Session.withCredentials unwrapped

                Nothing ->
                    Session.guest key

        start =
            \model ->
                { page = HomeModel model
                , navi = NavUI.init Route.Home session
                }
    in
    initWith
        HomeMessage
        start
        (Home.init session)
        |> Cmd.Extra.addCmd (Cmd.Extra.succeed <| ClickedLink (Browser.Internal url))



-- VIEW


view : Model -> Browser.Document Message
view model =
    let
        header =
            NavUI.view model.navi
                |> Html.map NavUIMessage

        contents =
            case model.page of
                AuthModel authModel ->
                    Auth.view authModel
                        |> List.map (Html.map AuthMessage)

                HomeModel homeModel ->
                    Home.view homeModel
                        |> List.map (Html.map HomeMessage)

                DiscModel discModel ->
                    Disc.view discModel
                        |> List.map (Html.map DiscMessage)

                QuitModel quitModel ->
                    Quit.view quitModel
                        |> List.map (Html.map never)

                ProfModel profModel ->
                    Prof.view profModel
                        |> List.map (Html.map ProfMessage)

                HelpModel helpModel ->
                    Help.view helpModel
                        |> List.map (Html.map HelpMessage)

                PollModel pollModel ->
                    Poll.view pollModel
                        |> List.map (Html.map PollMessage)

                DisplayPollModel displayPollModel ->
                    DisplayPoll.view displayPollModel
                        |> List.map (Html.map DisplayPollMessage)

                LiveModel liveModel ->
                    Live.view liveModel
                        |> List.map (Html.map LiveMessage)
    in
    { title = "heig-PRO-b04 | Live polls"
    , body = header :: contents
    }



-- UPDATE


type Message
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | NavUIMessage NavUI.Message
    | HomeMessage Home.Message
    | DiscMessage Disc.Message
    | AuthMessage Auth.Message
    | ProfMessage Prof.Message
    | HelpMessage Help.Message
    | PollMessage Poll.Message
    | DisplayPollMessage DisplayPoll.Message
    | LiveMessage Live.Message


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    let
        session =
            toSession model.page
    in
    case ( msg, model.page ) of
        ( NavUIMessage navMsg, _ ) ->
            updateWith
                NavUIMessage
                (embedNav model identity)
                NavUI.update
                navMsg
                model.navi

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink request, _ ) ->
            case request of
                Browser.Internal url ->
                    model
                        |> withCmds
                            [ Nav.pushUrl
                                (Session.sessionNavKey session)
                                (Url.toString url)
                            ]

                Browser.External href ->
                    model |> withCmds [ Nav.load href ]

        ( AuthMessage authMsg, AuthModel authModel ) ->
            updateWith
                AuthMessage
                (embed model AuthModel)
                Auth.update
                authMsg
                authModel

        ( HomeMessage homeMsg, HomeModel homeModel ) ->
            updateWith
                HomeMessage
                (embed model HomeModel)
                Home.update
                homeMsg
                homeModel

        ( DiscMessage discMsg, DiscModel discModel ) ->
            updateWith
                DiscMessage
                (embed model DiscModel)
                Disc.update
                discMsg
                discModel

        ( ProfMessage profMsg, ProfModel profModel ) ->
            updateWith
                ProfMessage
                (embed model ProfModel)
                Prof.update
                profMsg
                profModel

        ( HelpMessage helpMsg, HelpModel helpModel ) ->
            updateWith
                HelpMessage
                (embed model HelpModel)
                Help.update
                helpMsg
                helpModel

        ( PollMessage pollMsg, PollModel pollModel ) ->
            updateWith
                PollMessage
                (embed model PollModel)
                Poll.update
                pollMsg
                pollModel

        ( DisplayPollMessage displayPollMsg, DisplayPollModel displayPollModel ) ->
            updateWith
                DisplayPollMessage
                (embed model DisplayPollModel)
                DisplayPoll.update
                displayPollMsg
                displayPollModel

        ( LiveMessage liveMsg, LiveModel liveModel ) ->
            updateWith
                LiveMessage
                (embed model LiveModel)
                Live.update
                liveMsg
                liveModel

        ( _, _ ) ->
            model
                |> withNoCmd


subscriptions : Model -> Sub Message
subscriptions model =
    case model.page of
        ProfModel profModel ->
            Sub.map
                ProfMessage
                (Prof.subscriptions profModel)

        PollModel pollModel ->
            Sub.map
                PollMessage
                (Poll.subscriptions pollModel)

        DisplayPollModel displayPollModel ->
            Sub.map
                DisplayPollMessage
                (DisplayPoll.subscriptions displayPollModel)

        LiveModel liveModel ->
            Sub.map
                LiveMessage
                (Live.subscriptions liveModel)

        _ ->
            Sub.none


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Message )
changeRouteTo route model =
    let
        newRoute =
            route |> Maybe.withDefault Route.Home

        newModel =
            { model | navi = model.navi |> NavUI.withRoute newRoute }

        session =
            toSession model.page
    in
    case route of
        Nothing ->
            initWith
                HomeMessage
                (embed newModel HomeModel)
                (Home.init session)

        Just Route.Home ->
            initWith
                HomeMessage
                (embed newModel HomeModel)
                (Home.init session)

        Just Route.Login ->
            initWith
                AuthMessage
                (embed newModel AuthModel)
                (Auth.initLogin session)

        Just Route.Registration ->
            initWith
                AuthMessage
                (embed newModel AuthModel)
                (Auth.initRegistration session)

        Just Route.Logout ->
            initWith
                never
                (embed newModel QuitModel)
                (Quit.init session)

        Just Route.BadCredentials ->
            initWith
                DiscMessage
                (embed newModel DiscModel)
                (Disc.init session)

        Just Route.Account ->
            case Session.toViewer session of
                Just viewer ->
                    initWith
                        ProfMessage
                        (embed newModel ProfModel)
                        (Prof.init viewer)

                Nothing ->
                    changeRouteTo (Just Route.Home) model

        Just Route.Help ->
            initWith
                HelpMessage
                (embed newModel HelpModel)
                (Help.init session)

        Just Route.Polls ->
            case Session.toViewer session of
                Just viewer ->
                    initWith
                        PollMessage
                        (embed newModel PollModel)
                        (Poll.init viewer)

                Nothing ->
                    changeRouteTo (Just Route.Home) model

        Just Route.NewPoll ->
            case Session.toViewer session of
                Just viewer ->
                    initWith
                        DisplayPollMessage
                        (embed newModel DisplayPollModel)
                        (DisplayPoll.initCreate viewer)

                Nothing ->
                    changeRouteTo (Just Route.Home) model

        Just (Route.DisplayPoll poll) ->
            case Session.toViewer session of
                Just viewer ->
                    initWith
                        DisplayPollMessage
                        (embed newModel DisplayPollModel)
                        (DisplayPoll.initDisplay viewer poll)

                Nothing ->
                    changeRouteTo (Just Route.Home) model

        Just (Route.LivePoll poll) ->
            case Session.toViewer session of
                Just viewer ->
                    initWith
                        LiveMessage
                        (embed newModel LiveModel)
                        (Live.init viewer poll)

                Nothing ->
                    changeRouteTo (Just Route.Home) model



-- APPLICATION


main : Program Json.Decode.Value Model Message
main =
    Api.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
