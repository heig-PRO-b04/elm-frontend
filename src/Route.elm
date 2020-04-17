module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , replaceUrl
    )

import Browser.Navigation as Nav
import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Home
    | Login
    | Registration
    | Logout
    | Polls
    | NewPoll



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



-- PARSER


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Home (s "home")
        , Parser.map Registration (s "register")
        , Parser.map Logout (s "logout")
        , Parser.map Polls (s "polls")
        , Parser.map NewPoll (s "newpoll")
        ]



-- INNER UTILITIES


routeToString : Route -> String
routeToString page =
    String.join "/" (routeToPieces page)


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

        Polls ->
            [ "polls" ]

        NewPoll ->
            [ "newpoll" ]
