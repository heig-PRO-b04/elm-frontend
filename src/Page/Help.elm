module Page.Help exposing
    ( Message
    , Model
    , init
    , toSession
    , update
    , view
    )

import Cmd.Extra exposing (withNoCmd)
import Html exposing (Html, div)
import Html.Attributes as Attribute
import Picasso.Text exposing (styledH3)
import Session exposing (Session)


type alias Message =
    Never


type alias Model =
    Session


toSession : Model -> Session
toSession =
    identity


init : Session -> ( Model, Cmd Message )
init session =
    ( session, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    model
        |> withNoCmd


view : Model -> List (Html Message)
view _ =
    [ Html.div
        [ Attribute.class "flex flex-col"
        , Attribute.class "m-auto mt-4 md:mt-16 mb-4 md:mb-16"

        -- Card appearance
        , Attribute.class "bg-white shadow px-8 pb-8"
        , Attribute.class "md:rounded-lg md:w-1/2 md:max-w-l"
        ]
        [ one
        , two
        , three
        ]
    ]


one : Html Message
one =
    rightImg "1. Lorem ipsum" "img/newPoll.png" "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In ac diam ac nisi posuere mollis vel quis enim. Aliquam eu ipsum diam. Cras neque neque, suscipit quis quam sed, tincidunt fermentum ipsum. Sed quis efficitur urna. Praesent euismod euismod turpis vel ullamcorper. Nunc interdum nec augue non ullamcorper. Phasellus imperdiet ut erat a fermentum."


two : Html Message
two =
    leftImg "2. Lorem ipsum" "img/newPoll.png" "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In ac diam ac nisi posuere mollis vel quis enim. Aliquam eu ipsum diam. Cras neque neque, suscipit quis quam sed, tincidunt fermentum ipsum. Sed quis efficitur urna. Praesent euismod euismod turpis vel ullamcorper. Nunc interdum nec augue non ullamcorper. Phasellus imperdiet ut erat a fermentum."


three : Html Message
three =
    rightImg "3. Lorem ipsum" "img/github.jpg" "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In ac diam ac nisi posuere mollis vel quis enim. Aliquam eu ipsum diam. Cras neque neque, suscipit quis quam sed, tincidunt fermentum ipsum. Sed quis efficitur urna. Praesent euismod euismod turpis vel ullamcorper. Nunc interdum nec augue non ullamcorper. Phasellus imperdiet ut erat a fermentum."


rightImg : String -> String -> String -> Html Message
rightImg title source text =
    nextToEachOther title (Html.text text) (sideImage source)


leftImg : String -> String -> String -> Html Message
leftImg title source text =
    nextToEachOther title (sideImage source) (Html.text text)


nextToEachOther : String -> Html Message -> Html Message -> Html Message
nextToEachOther title left right =
    div
        [ Attribute.class "flex flex-col md:flex-row items-center"
        , Attribute.class "pt-4"
        ]
        [ left
        , right
        ]
        |> withTitle title


withTitle : String -> Html Message -> Html Message
withTitle title html =
    div
        [ Attribute.class "flex flex-col"
        , Attribute.class "pt-8"
        ]
        [ styledH3 title
        , html
        ]


sideImage : String -> Html Message
sideImage source =
    Html.img
        [ Attribute.src source
        , Attribute.class "p-4"
        ]
        []
