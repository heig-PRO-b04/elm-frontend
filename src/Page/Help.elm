module Page.Help exposing
    ( Message
    , Model
    , init
    , toSession
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Page.Help.Content as Content exposing (SectionContent(..))
import Session exposing (Session)
import Set exposing (Set)



-- MODEL


type alias Model =
    { session : Session
    , open : Set Int
    }


type alias Step =
    { imageUrl : String
    , number : Int
    , description : String
    }


toSession : Model -> Session
toSession =
    .session


init : Session -> ( Model, Cmd Message )
init session =
    ( { session = session, open = Set.empty }, Cmd.none )



-- UPDATE


type Message
    = Toggle Int


update : Message -> Model -> ( Model, Cmd Message )
update (Toggle index) model =
    let
        new =
            if Set.member index model.open then
                Set.remove index model.open

            else
                Set.insert index model.open
    in
    ( { model | open = new }, Cmd.none )



-- VIEW


view : Model -> List (Html Message)
view model =
    [ Html.div
        [ Attribute.class "m-auto mt-4 md:mt-16 mb-4 md:mb-16"

        -- Card appearance
        , Attribute.class "bg-white shadow-xl overflow-hidden"
        , Attribute.class "md:rounded-lg md:w-1/2 md:max-w-l"
        ]
        [ viewTitle
        , viewTripleStep ( stepOne, stepTwo, stepThree )
        , section 0 "How do I manage polls ?" model.open <| List.map sectionContent Content.managingPolls
        , section 1 "How can I organize questions ?" model.open <| List.map sectionContent Content.organizingQuestions
        , section 2 "How do poll statistics work ?" model.open <| List.map sectionContent Content.statistics
        , section 3 "How do participants vote ? " model.open <| List.map sectionContent Content.voting
        ]
    ]


stepOne : Step
stepOne =
    { imageUrl = "https://via.placeholder.com/150/E1F5FE/E1F5FE"
    , number = 1
    , description = "Create a poll"
    }


stepTwo : Step
stepTwo =
    { imageUrl = "https://via.placeholder.com/150/E1F5FE/E1F5FE"
    , number = 2
    , description = "Add a question"
    }


stepThree : Step
stepThree =
    { imageUrl = "https://via.placeholder.com/150/E1F5FE/E1F5FE"
    , number = 3
    , description = "Open your poll"
    }



-- DISPLAYING STEPS


viewTitle : Html Message
viewTitle =
    Html.div [ Attribute.class "p-8" ]
        [ Html.span [ Attribute.class "block font-archivo text-3xl font-bold" ] [ Html.text "How to get started" ]
        , Html.span [ Attribute.class "block font-archivo text-xl text-gray-500" ] [ Html.text "To get started, all you need is a poll and some questions. This help section contains some additional information about more advanced features of the application." ]
        ]


viewStep : Step -> Html Message
viewStep step =
    Html.div
        [ Attribute.class "shadow rounded-lg overflow-hidden border-2 bg-gray-100 border-gray-100"
        , Attribute.class "flex flex-col"
        ]
        [ Html.img
            [ Attribute.class "object-cover rounded-lg overflow-hidden w-56 h-56"
            , Attribute.src step.imageUrl
            ]
            []
        , Html.div [ Attribute.class "font-archivo font-bold font-xl p-2 w-full" ]
            [ Html.span [ Attribute.class "text-gray-500" ] [ Html.text <| String.fromInt step.number ++ ". " ]
            , Html.text step.description
            ]
        ]


viewTripleStep : ( Step, Step, Step ) -> Html Message
viewTripleStep ( first, second, third ) =
    Html.div
        [ Attribute.class "flex flex-col xl:flex-row items-center justify-between px-8 pb-8" ]
        [ viewStep first
        , Html.div [ Attribute.class "mt-4 xl:ml-4" ] []
        , viewStep second
        , Html.div [ Attribute.class "mt-4 xl:ml-4" ] []
        , viewStep third
        ]


section : Int -> String -> Set Int -> List (Html Message) -> Html Message
section index title allOpen contents =
    let
        open =
            Set.member index allOpen

        tail =
            if open then
                [ Attribute.class "transform duration-200 opacity-100 scale-100 pt-8 h-auto" ]

            else
                [ Attribute.class "transform duration-200 opacity-0 scale-95 h-0" ]

        background =
            if open then
                Attribute.class "bg-white"

            else
                Attribute.class "bg-gray-100 hover:bg-gray-200"

        arrow =
            if open then
                Attribute.class "transform duration-200 rotate-90"

            else
                Attribute.class "transform duration-200 rotate-0"
    in
    Html.div
        [ Attribute.class "select-none cursor-pointer p-8 border-t  "
        , Attribute.class "transform duration-200"
        , background
        , Event.onClick <| Toggle index
        ]
        [ Html.div [ Attribute.class "flex flex-row justify-between" ]
            [ Html.span
                [ Attribute.class "text-xl md:text-2xl font-semibold font-archivo text-gray-600" ]
                [ Html.text title ]
            , Html.img [ arrow, Attribute.class "w-8 h-8", Attribute.src "/icon/chevron-right.svg" ] []
            ]
        , Html.div tail contents
        ]


sectionContent : SectionContent Message -> Html Message
sectionContent content =
    case content of
        SectionTip title html ->
            sectionDescriptionCard title html

        SectionTitle title ->
            sectionDescriptionTitle title

        SectionIllustration number title url ->
            sectionDescriptionIllustration number title url

        SectionDescription html ->
            sectionDescriptionText html


sectionDescriptionCard : String -> List (Html Message) -> Html Message
sectionDescriptionCard title text =
    Html.div
        [ Attribute.class "border-seaside-100 border-2 bg-seaside-050 text-black font-archivo rounded-lg p-4" ]
        [ Html.span [ Attribute.class "block font-semibold text-gray-600" ] [ Html.text title ]
        , Html.span [ Attribute.class "font-light text-gray-800" ] text
        ]


sectionDescriptionTitle : String -> Html Message
sectionDescriptionTitle title =
    Html.span
        [ Attribute.class "text-gray-500 font-archivo text-xl font-semibold"
        , Attribute.class "block pb-8"
        ]
        [ Html.text title ]


sectionDescriptionText : String -> Html Message
sectionDescriptionText description =
    Html.span
        [ Attribute.class "text-gray-500 font-archivo"
        , Attribute.class "block pb-8"
        ]
        [ Html.text description ]


sectionDescriptionIllustration : Int -> String -> String -> Html Message
sectionDescriptionIllustration number description url =
    Html.div [ Attribute.class "flex flex-row justify-center pb-8" ]
        [ Html.div
            [ Attribute.class "shadow rounded-lg overflow-hidden border-2 bg-seaside-100 border-seaside-100"
            , Attribute.class "flex flex-col"
            ]
            [ Html.img
                [ Attribute.class "object-cover rounded-lg overflow-hidden w-56 h-56"
                , Attribute.src url
                ]
                []
            , Html.div [ Attribute.class "font-archivo font-bold font-xl p-2 w-full text-seaside-A700" ]
                [ Html.span [ Attribute.class "text-seaside-A700 opacity-50" ] [ Html.text <| String.fromInt number ++ ". " ]
                , Html.text description
                ]
            ]
        ]
