module Page.QuestionList.Visibility exposing
    ( Visibility(..)
    , display
    )

{-| A module that helps to decide if a question should be displayed


# types

@docs Visibility


# functions

@docs display

-}

import Api.Questions as Api


type Visibility
    = All
    | Active
    | Archived


display : Visibility -> Api.ServerQuestion -> Bool
display visibility question =
    case visibility of
        All ->
            True

        Active ->
            question.visibility /= Api.Archived

        Archived ->
            question.visibility == Api.Archived
