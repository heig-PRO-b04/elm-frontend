module Username exposing
    ( Username
    , decoder
    , encode
    )

import Json.Decode
import Json.Encode


type Username
    = Username String


decoder : Json.Decode.Decoder Username
decoder =
    Json.Decode.string
        |> Json.Decode.map Username


encode : Username -> Json.Encode.Value
encode (Username name) =
    Json.Encode.string name
