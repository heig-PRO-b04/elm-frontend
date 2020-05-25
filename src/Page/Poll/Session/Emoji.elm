module Page.Poll.Session.Emoji exposing (img)

{-| A module that provides emoji images for the session code


# functions

@docs img

-}

import Api.Sessions
import Html exposing (Html)
import Html.Attributes exposing (class)


img : List (Html.Attribute msg) -> List (Html msg) -> Api.Sessions.Emoji -> Html msg
img attrs html emoji =
    Html.img
        ([ src emoji, class "p-1 md:p-3 w-12 h-12 md:w-16 md:h-16 bg-white shadow rounded-lg overflow-hidden" ] ++ attrs)
        html


src : Api.Sessions.Emoji -> Html.Attribute msg
src emoji =
    Html.Attributes.src <|
        case emoji of
            Api.Sessions.Emoji0 ->
                "/emoji/emoji_0.png"

            Api.Sessions.Emoji1 ->
                "/emoji/emoji_1.png"

            Api.Sessions.Emoji2 ->
                "/emoji/emoji_2.png"

            Api.Sessions.Emoji3 ->
                "/emoji/emoji_3.png"

            Api.Sessions.Emoji4 ->
                "/emoji/emoji_4.png"

            Api.Sessions.Emoji5 ->
                "/emoji/emoji_5.png"

            Api.Sessions.Emoji6 ->
                "/emoji/emoji_6.png"

            Api.Sessions.Emoji7 ->
                "/emoji/emoji_7.png"

            Api.Sessions.Emoji8 ->
                "/emoji/emoji_8.png"

            Api.Sessions.Emoji9 ->
                "/emoji/emoji_9.png"

            Api.Sessions.EmojiA ->
                "/emoji/emoji_a.png"

            Api.Sessions.EmojiB ->
                "/emoji/emoji_b.png"

            Api.Sessions.EmojiC ->
                "/emoji/emoji_c.png"

            Api.Sessions.EmojiD ->
                "/emoji/emoji_d.png"

            Api.Sessions.EmojiE ->
                "/emoji/emoji_e.png"

            Api.Sessions.EmojiF ->
                "/emoji/emoji_f.png"
