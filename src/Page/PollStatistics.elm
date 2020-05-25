module Page.PollStatistics exposing
    ( Model, Message
    , update, view
    , init, subscriptions
    )

{-| A page sub-module that displays a poll's global statistics


# TEA

@docs Model, Message
@docs update, view


# functions

@docs init, subscriptions

-}

import Api.Polls exposing (ServerPoll)
import Api.Statistics as Api
import Html exposing (Html)
import Html.Attributes as Attribute
import Page.PollStatistics.Charts as Charts
import Session exposing (Viewer)
import Task
import Task.Extra
import Time



-- MODEL


updateFrequency : number
updateFrequency =
    5


type alias Model =
    { viewer : Viewer
    , poll : ServerPoll
    , statistics : List Api.QuestionStatistics
    }


init : Viewer -> ServerPoll -> ( Model, Cmd Message )
init viewer poll =
    ( { viewer = viewer, poll = poll, statistics = [] }
    , cmdRetrievePollStatistics viewer poll
    )



-- UPDATE


type Message
    = PerformReload
    | GotStatistics (List Api.QuestionStatistics)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GotStatistics retrieved ->
            ( { model | statistics = retrieved }, Cmd.none )

        PerformReload ->
            ( model, cmdRetrievePollStatistics model.viewer model.poll )


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every (updateFrequency * 1000) (always PerformReload)



-- EFFECTS


cmdRetrievePollStatistics : Viewer -> ServerPoll -> Cmd Message
cmdRetrievePollStatistics viewer poll =
    Api.getStatisticsForPoll (Session.viewerCredentials viewer) poll GotStatistics
        |> Task.mapError (always <| GotStatistics [])
        |> Task.Extra.execute



-- VIEW


view : Model -> List (Html Message)
view model =
    List.singleton <|
        viewStatistics model.statistics


viewStatisticsHeader : Html Message
viewStatisticsHeader =
    Html.div
        [ Attribute.class "block p-4 font-bold font-archivo tracking-wider text-gray-500"
        , Attribute.class "bg-gray-100 rounded-t-lg border-b-2 pl-6"
        ]
        [ Html.text "Statistics" ]


viewStatisticsFooter : Html Message
viewStatisticsFooter =
    Html.div
        [ Attribute.class "h-4 rounded-b-lg bg-gray-100" ]
        []


viewStatisticsEmptyState : Html Message
viewStatisticsEmptyState =
    Html.div
        [ Attribute.class "p-4 font-bold text-gray-600 border-b-2 w-full" ]
        [ Html.text "Real-time statistics for this poll will be displayed right here 🎯" ]


viewStatistics : List Api.QuestionStatistics -> Html Message
viewStatistics questions =
    let
        questionsHtml =
            List.sortBy .title questions
                |> List.map viewQuestion
                |> (\x ->
                        if List.isEmpty x then
                            List.singleton viewStatisticsEmptyState

                        else
                            x
                   )
                |> Html.div [ Attribute.class "flex flex-row flex-wrap justify-around" ]
    in
    Html.div
        [ Attribute.class "bg-white shadow rounded-lg mb-32"
        , Attribute.class "block align-middle mx-2 md:mx-8 mt-8 mb-8"
        ]
        (viewStatisticsHeader :: questionsHtml :: (List.singleton <| viewStatisticsFooter))


viewQuestion : Api.QuestionStatistics -> Html Message
viewQuestion question =
    Html.div [ Attribute.class "w-full max-w-md md:max-w-xl" ]
        [ Charts.answersChart question ]
