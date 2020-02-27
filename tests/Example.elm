module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    test "Elm Test is correctly set up" <|
        \_ -> Expect.pass
