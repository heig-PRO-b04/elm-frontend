module Json.Decode.Extra exposing (map9)

{-| A module that provides additional Json Decoding capabilities


# functions

@docs map9

-}

import Json.Decode as Decode exposing (Decoder, andThen)


{-| -}
map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder value
map9 apply da db dc dd de df dg dh di =
    -- There is no such thing as do notation and monad-friendly elm-format.
    andThen
        (\a ->
            andThen
                (\b ->
                    andThen
                        (\c ->
                            andThen
                                (\d ->
                                    andThen
                                        (\e ->
                                            andThen
                                                (\f ->
                                                    andThen
                                                        (\g ->
                                                            andThen
                                                                (\h ->
                                                                    andThen
                                                                        (\i ->
                                                                            Decode.succeed <|
                                                                                apply
                                                                                    a
                                                                                    b
                                                                                    c
                                                                                    d
                                                                                    e
                                                                                    f
                                                                                    g
                                                                                    h
                                                                                    i
                                                                        )
                                                                        di
                                                                )
                                                                dh
                                                        )
                                                        dg
                                                )
                                                df
                                        )
                                        de
                                )
                                dd
                        )
                        dc
                )
                db
        )
        da
