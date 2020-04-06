module Session exposing
    ( Session
    , extractCredentials
    , guest
    , isLoggedIn
    , navKey
    , withCredentials
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


extractCredentials : Session -> Maybe Credentials
extractCredentials session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn _ cred ->
            Just cred


withCredentials : Credentials -> Session -> Session
withCredentials credentials session =
    case session of
        Guest key ->
            LoggedIn key credentials

        LoggedIn key _ ->
            LoggedIn key credentials


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        _ ->
            False
