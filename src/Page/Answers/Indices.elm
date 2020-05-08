module Page.Answers.Indices exposing (forIndex)


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


{-| Returns an alphabetical prefix for a certain index, in lexicographic order.
-}
forIndex : Int -> String
forIndex index =
    if index < 0 then
        forIndexHelper index |> String.toUpper

    else
        forIndexHelper index


forIndexHelper index =
    if index < String.length alphabet then
        String.slice index (index + 1) alphabet

    else
        let
            last =
                forIndexHelper (modBy (String.length alphabet) index)

            rest =
                (index // String.length alphabet) - 1
        in
        forIndexHelper rest ++ last
