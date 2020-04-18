module Session exposing
    ( Session, guest
    , withCredentials
    , isLoggedIn, sessionCredentials, sessionNavKey
    , Viewer
    , viewerCredentials, viewerNavKey
    , toSession, toViewer
    )

{-| A module that represents the state of users in the application. A Session can always be obtained
and a Viewer guarantees that the user of the application is currently logged. That is, from a
Session, it may be possible to obtain a Viewer, and a Viewer can always produce a Session.


# Sessions

@docs Session, guest
@docs withCredentials
@docs isLoggedIn, sessionCredentials, sessionNavKey


# Viewers

@docs Viewer
@docs viewerCredentials, viewerNavKey


# Conversions

@docs toSession, toViewer

-}

import Api exposing (Credentials)
import Browser.Navigation as Nav



-- SESSION


type Session
    = Guest Nav.Key
    | LoggedIn Nav.Key Credentials


guest : Nav.Key -> Session
guest =
    Guest


withCredentials : Credentials -> Session -> Session
withCredentials credentials session =
    case session of
        Guest key ->
            LoggedIn key credentials

        LoggedIn key _ ->
            LoggedIn key credentials



-- SESSION EXTRACT


sessionNavKey : Session -> Nav.Key
sessionNavKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ ->
            key


sessionCredentials : Session -> Maybe Credentials
sessionCredentials session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn _ cred ->
            Just cred


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        _ ->
            False



-- VIEWER


type Viewer
    = Viewer Nav.Key Credentials


toViewer : Session -> Maybe Viewer
toViewer session =
    case session of
        LoggedIn key credentials ->
            Just (Viewer key credentials)

        Guest _ ->
            Nothing


toSession : Viewer -> Session
toSession (Viewer key credentials) =
    LoggedIn key credentials


viewerCredentials : Viewer -> Credentials
viewerCredentials (Viewer _ credentials) =
    credentials


viewerNavKey : Viewer -> Nav.Key
viewerNavKey (Viewer key _) =
    key
