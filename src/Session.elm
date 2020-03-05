module Session exposing
    ( Session
    , guest
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
