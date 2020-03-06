module Session exposing
    ( Session
    , guest
    , isLoggedIn
    , navKey
    )

import Api exposing (Credentials)
import Browser.Navigation as Nav


type Session
    = Guest Nav.Key
    | LoggedIn Nav.Key Credentials


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ ->
            key


guest : Nav.Key -> Session
guest =
    Guest


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        _ ->
            False
