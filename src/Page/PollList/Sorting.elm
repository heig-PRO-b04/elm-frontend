module Page.PollList.Sorting exposing
    ( Order(..)
    , ordering
    )

{-| A module that provide list sorting utilities


# types

@docs Order


# functions

@docs ordering

-}

import Api.Polls as Api


type Order
    = TitleAsc
    | TitleDes


ordering : Order -> Api.ServerPoll -> Api.ServerPoll -> Basics.Order
ordering order a b =
    case order of
        TitleAsc ->
            compare a.title b.title

        TitleDes ->
            compare b.title a.title
