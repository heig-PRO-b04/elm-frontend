module Page.DisplayPoll.Questions exposing
    ( Message
    , Model
    , init
    , update
    , view
    )

import Api.Polls exposing (Poll)
import Api.Questions exposing (QuestionDiscriminator, ServerQuestion)
import Cmd exposing (withCmd, withNoCmd)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Session exposing (Viewer)


type alias Model =
    { viewer : Viewer
    , poll : Poll
    , questions : List ServerQuestion
    }


type Message
    = GotAllQuestions (List ServerQuestion)
    | GotInvalidCredentials
    | NowRequestQuestions
    | NowDeleteQuestion QuestionDiscriminator


init : Viewer -> Api.Polls.Poll -> ( Model, Cmd Message )
init viewer poll =
    { viewer = viewer
    , poll = poll
    , questions = []
    }
        |> withCmd [ Cmd.succeed NowRequestQuestions ]


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    model |> withNoCmd


view : Model -> List (Html Message)
view model =
    []
