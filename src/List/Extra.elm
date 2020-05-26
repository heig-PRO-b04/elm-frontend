module List.Extra exposing (grouped)

{-| A module that provides additional List functions


# functions

@docs grouped

-}


{-| A helper function that will separate a list in sublists of a specific size
-}
grouped : Int -> List a -> List (List a)
grouped size list =
    if size <= 0 || list == [] then
        []

    else
        List.take size list :: (grouped size <| List.drop size list)
