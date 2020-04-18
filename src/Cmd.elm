module Cmd exposing
    ( withCmd, withNoCmd
    , initWith
    , updateWith
    , andThen
    )

{-| A few utilities for the update functions of the applications, documents and
everything that works with commands essentially.


# Commands

@docs withCmd, withNoCmd


# Init

@docs initWith


# Update

@docs updateWith


# Chaining

@docs andThen

-}

import Task



-- COMMANDS


{-| Generates a new update result with a list of commands to be applied.
-}
withCmd : List (Cmd msg) -> a -> ( a, Cmd msg )
withCmd msg m =
    ( m, Cmd.batch msg )


{-| Generates a new update result with no associated commands.
-}
withNoCmd : a -> ( a, Cmd msg )
withNoCmd m =
    ( m, Cmd.none )


{-| Generates a command that returns a value in a pure fashion.
-}
succeed : a -> Cmd a
succeed value =
    Task.succeed value
        |> Task.perform identity



-- INIT


{-| A helper function that is helpful to init parts of a sub-model and propagate
the changes into a parent model.
-}
initWith :
    (subMessage -> message)
    -> (subModel -> model)
    -> ( subModel, Cmd subMessage )
    -> ( model, Cmd message )
initWith toMessage toModel subInit =
    let
        ( m, c ) =
            subInit
    in
    ( toModel m, Cmd.map toMessage c )



-- UPDATE


{-| A helper function that is helpful to updates parts of a sub-model and
propagate the changes into a parent model.

In our case, this will be used whenever we want to dispatch some messages that
are specific to certain screens only and should not be shared across the whole
app.

    type alias Model
        { positive : Int
        , negative : Int
        }

    type Message
        = Positive Plus
        | Negative Minus

    type Plus
        = One
        | Two

    type Minus
        = One
        | Two

    -- Assuming we have this inner update function to use.
    updatePlus : Plus -> Int -> (Int, Cmd msg)
    updatePlus msg counter =
        case msg of
            One -> (counter + 1, Cmd.none)
            Two -> (counter + 2, Cmd.none)

    -- Wrapping the sub-model in an existing model.
    -- Notice how we can't update an existing model. This requires model
    -- partitions.
    wrap : Int -> Model
    wrap counter model = { positive = counter, negative = 0 }

    -- Proper update function, that delegates by partitioning. Communication can
    -- be done through extra messages, by using a shared state (for instance a
    -- Session instance).
    update : Message -> Model -> (Model, Cmd msg)
    update msg model =
        case msg of
            Positive plus ->
                updateWith
                    Positive
                    wrap
                    updatePlus
                    msg
                    counter.positive
            Negative mins -> -- Identical.

-}
updateWith :
    (subMessage -> message)
    -> (subModel -> model)
    -> (subMessage -> subModel -> ( subModel, Cmd subMessage ))
    -> subMessage
    -> subModel
    -> ( model, Cmd message )
updateWith toMessage toModel subUpdate subMessage subModel =
    let
        ( m, c ) =
            subUpdate subMessage subModel
    in
    toModel m
        |> withCmd [ Cmd.map toMessage c ]



-- CHAINING


{-| A function that makes it possible to chain commands on the result of an update or init function.
-}
andThen :
    (subModel -> ( model, Cmd result ))
    -> ( subModel, Cmd result )
    -> ( model, Cmd result )
andThen f ( subModel, subCmd ) =
    let
        ( model, cmd ) =
            f subModel
    in
    ( model, Cmd.batch [ subCmd, cmd ] )
