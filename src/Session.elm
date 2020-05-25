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


{-| Builds a session for a non-logged in user
-}
guest : Nav.Key -> Session
guest =
    Guest


{-| Builds a session for a logged in user
-}
withCredentials : Credentials -> Session -> Session
withCredentials credentials session =
    case session of
        Guest key ->
            LoggedIn key credentials

        LoggedIn key _ ->
            LoggedIn key credentials



-- SESSION EXTRACT


{-| Returns the Navigation Key for a session
-}
sessionNavKey : Session -> Nav.Key
sessionNavKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ ->
            key


{-| Returns the session credentials, if they exist
-}
sessionCredentials : Session -> Maybe Credentials
sessionCredentials session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn _ cred ->
            Just cred


{-| Function that tells if the session is for a logged in user
-}
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


{-| Builds a viewer from the session, if possible
-}
toViewer : Session -> Maybe Viewer
toViewer session =
    case session of
        LoggedIn key credentials ->
            Just (Viewer key credentials)

        Guest _ ->
            Nothing


{-| Builds a session from a viewer
-}
toSession : Viewer -> Session
toSession (Viewer key credentials) =
    LoggedIn key credentials


{-| Returns the credentials for a viewer
-}
viewerCredentials : Viewer -> Credentials
viewerCredentials (Viewer _ credentials) =
    credentials


{-| Returns the navigation key for a viewer
-}
viewerNavKey : Viewer -> Nav.Key
viewerNavKey (Viewer key _) =
    key
