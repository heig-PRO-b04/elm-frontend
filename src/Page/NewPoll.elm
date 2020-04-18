module Page.NewPoll exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api
import Cmd exposing (withNoCmd)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Picasso.Button exposing (button, elevated, filled, filledDisabled)
import Picasso.Input as Input
import Picasso.Text exposing (styledH2)
import Session exposing (Session, Viewer)


type Message
    = WriteNewTitle String
    | CreatePoll
    | UpdateTitle
    | GotCreateError
    | GotCreateSuccess


type CreationState
    = NotCreated
    | Pending
    | Created
    | BadNetwork


type alias Model =
    { viewer : Viewer
    , state : CreationState
    , title : String
    }


init : Viewer -> ( Model, Cmd Message )
init viewer =
    { viewer = viewer
    , state = NotCreated
    , title = ""
    }
        |> withNoCmd


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        WriteNewTitle title ->
            { model | title = title }
                |> withNoCmd

        CreatePoll ->
            model
                |> withNoCmd

        GotCreateError ->
            model
                |> withNoCmd

        GotCreateSuccess ->
            model
                |> withNoCmd

        UpdateTitle ->
            model
                |> withNoCmd


view : Model -> List (Html Message)
view model =
    [ div
        [ class "flex flex-col"
        , class "m-auto my-4 md:my-16"

        -- Card appearance
        , class "bg-white"
        , class "shadow"
        , class "p-8"
        , class "md:rounded-lg"
        , class "md:w-1/2"
        , class "md:max-w-lg"
        ]
        [ styledH2 "Create a new poll"
        , inputTitle <| model.title
        , buttonCreatePoll model.state
        ]
    ]


inputTitle : String -> Html Message
inputTitle content =
    Input.inputWithTitle "Poll title"
        [ onInput WriteNewTitle
        , placeholder "Et tu, Brute?"
        , value content
        ]
        []
        |> withMargin


buttonCreatePoll : CreationState -> Html Message
buttonCreatePoll state =
    let
        fillIn =
            if state == Pending then
                filledDisabled

            else
                filled ++ elevated ++ [ onClick CreatePoll ]

        message =
            case state of
                NotCreated ->
                    "Create poll"

                Pending ->
                    "Loading..."

                Created ->
                    "Success !"

                BadNetwork ->
                    "Network error"
    in
    button
        (fillIn ++ [ class "mt-8" ])
        [ text message ]


withMargin : Html msg -> Html msg
withMargin html =
    div [ class "mt-8" ]
        [ html ]
