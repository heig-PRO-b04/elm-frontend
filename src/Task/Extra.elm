module Task.Extra exposing (execute)

{-| A module that provides extra Task functions


# Tasks

@docs execute

-}

import Task exposing (Task)


{-| Tasks for which the error type matches the result type may be flattened
when they are executed.
-}
execute : Task a a -> Cmd a
execute task =
    Task.attempt
        (\result ->
            case result of
                Result.Ok x ->
                    x

                Result.Err x ->
                    x
        )
        task
