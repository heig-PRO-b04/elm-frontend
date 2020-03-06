module Page.Header exposing (navigationHeader)

import Html exposing (Html, text)
import Session exposing (Session)


navigationHeader : Session -> Html msg
navigationHeader session =
    text <|
        if Session.isLoggedIn session then
            "Logged in"

        else
            "Not logged in"
