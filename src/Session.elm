module Session exposing
    ( Session
    , guest
    , navKey
    )

import Browser.Navigation as Nav


type Session
    = Guest Nav.Key


navKey : Session -> Nav.Key
navKey (Guest key) =
    key


guest : Nav.Key -> Session
guest =
    Guest
