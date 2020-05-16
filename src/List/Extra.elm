module List.Extra exposing (grouped)


grouped : Int -> List a -> List (List a)
grouped size list =
    if size <= 0 || list == [] then
        []

    else
        List.take size list :: (grouped size <| List.drop size list)
