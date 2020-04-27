module Page.PollList.Sorting exposing
    ( Order(..)
    , ordering
    )

import Api.Polls as Api


type Order
    = TitleAsc
    | TitleDes


ordering : Order -> Api.Poll -> Api.Poll -> Basics.Order
ordering order a b =
    case order of
        TitleAsc ->
            compare a.title b.title

        TitleDes ->
            compare b.title a.title
