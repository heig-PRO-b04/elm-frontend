module Route exposing
    ( Route(..)
    , badCredentials
    , fromUrl
    , href
    , replaceUrl
    )

import Api.Polls exposing (PollDiscriminator)
import Browser.Navigation as Nav
import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Login
    | Registration
    | Logout
    | BadCredentials
    | Polls
    | NewPoll
    | DisplayPoll PollDiscriminator
    | LivePoll PollDiscriminator



-- PUBLIC HELPERS


href : Route -> Html.Attribute msg
href page =
    Html.Attributes.href (routeToString page)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


{-| Performs navigation to a page that lets the user know that they currently have some bad
credentials, and automatically disconnects them.
-}
badCredentials : Nav.Key -> Cmd msg
badCredentials key =
    replaceUrl key BadCredentials



-- PARSER


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Home (s "home")
        , Parser.map Registration (s "register")
        , Parser.map Logout (s "logout")
        , Parser.map BadCredentials (s "disconnected")
        , Parser.map Polls (s "polls")
        , Parser.map NewPoll (s "newpoll")
        , Parser.map DisplayPoll (s "displaypoll" </> Api.Polls.urlParser)
        , Parser.map LivePoll (s "live" </> Api.Polls.urlParser)
        ]



-- INNER UTILITIES


routeToString : Route -> String
routeToString page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            [ "home" ]

        Login ->
            [ "login" ]

        Registration ->
            [ "register" ]

        Logout ->
            [ "logout" ]

        BadCredentials ->
            [ "disconnected" ]

        Polls ->
            [ "polls" ]

        NewPoll ->
            [ "newpoll" ]

        DisplayPoll poll ->
            [ "displaypoll", String.fromInt poll.idPoll ]

        LivePoll poll ->
            [ "live", String.fromInt poll.idPoll ]
