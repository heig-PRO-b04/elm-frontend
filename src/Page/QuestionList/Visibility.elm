module Page.QuestionList.Visibility exposing
    ( Visibility(..)
    , display
    )

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
