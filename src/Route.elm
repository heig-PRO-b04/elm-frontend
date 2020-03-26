module Route exposing
    ( Route(..)
    , fromUrl
    , href
    )

import Browser.Navigation as Nav
import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Home
    | Login



-- PUBLIC HELPERS


href : Route -> Html.Attribute msg
href page =
    Html.Attributes.href (routeToString page)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- PARSER


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Home (s "home")
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
