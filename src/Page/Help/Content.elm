module Page.Help.Content exposing
    ( SectionContent(..)
    , managingPolls, organizingQuestions, statistics, voting
    )

{-| A module that provides a clear way to manage content of the Help pages


# types

@docs SectionContent


# content

@docs managingPolls, organizingQuestions, statistics, voting

-}

import Html exposing (Html)
import Html.Attributes as Attribute


type SectionContent msg
    = SectionTitle String
    | SectionDescription String
    | SectionIllustration Int String String
    | SectionTip String (List (Html msg))


{-| Represents the help section about managing polls
-}
managingPolls : List (SectionContent msg)
managingPolls =
    [ SectionTitle "What's a poll ?"
    , SectionDescription "Polls are how you communicate with your audience. They have a title, and can be modified from the web interface. Opening a poll is easy - all you have to do is click on it from the Home screen. Polls contain a list of questions, and are the atomic bits of content that participants will answer to. Participants are granted access to a poll as a whole, and can all answer the exact same set of questions in them."
    , SectionIllustration 1 "The list of your polls" "/help/polls-all.png"
    , SectionDescription "To create a new poll, all you need to do is click on the bottom right button, and give the poll a name. To navigate back to the home screen, use the navigation menu on the top right and click on \"My Polls\"."
    , SectionDescription "By default, polls are sorted in alphabetical order. It's nevertheless possible to inverse the sort order by selecting the arrow at the top of the titles column. "
    , SectionIllustration 2 "Creating a poll" "/help/polls-new.png"
    , SectionTip "Pro Tip : Polls update in real-time 🚀"
        [ Html.text "And without any participant interaction ! If you want to update the contents of your poll, it's possible to do so while participants are still on it. "
        , Html.text "Want to add a new question that's more relevant"
        , Html.span [ Attribute.class "font-semibold" ] [ Html.text " now  " ]
        , Html.text "? "
        , Html.text "That's definitely possible !"
        ]
    ]


{-| Represents the help section about organizing questions
-}
organizingQuestions : List (SectionContent msg)
organizingQuestions =
    [ SectionTitle "What's a question ?"
    , SectionDescription "Questions are the core component to interact with a participant. Each question has a set of answers, which can then be voted for and against by your poll participants."
    , SectionDescription "After you've created a question with the button at the bottom right of the \"Poll details\" screen, click on it to expand it, and see the list of answers assigned to that question."
    , SectionIllustration 1 "Question details" "/help/questions-1.png"
    , SectionTitle "Answers"
    , SectionDescription "A question has a set of answers that you must provide. Participants will essentially be asked to vote yes or no to each of the provided answer, but you can also select how many answers participants must at least select, and how many they might select at most, too."
    , SectionTip "Pro Tip : Questions can be hidden 👻 and archived 📦"
        [ Html.text "When making a poll, you sometimes realize that the audience is not quite responding the same way each time. "
        , Html.text "Wouldn't it be nice if you could swap out a prepared set of questions based on what the "
        , Html.span [ Attribute.class "font-semibold" ] [ Html.text "current" ]
        , Html.text " audience is responding ? "
        , Html.text "That's what question visibility is for. Just press the eye icon, and your question will become hidden (resp. visible) to the participants. You can prepare a set of questions before the poll starts, and swap out questions while it's already running."
        ]
    ]


{-| Represents the help section about statistics
-}
statistics : List (SectionContent msg)
statistics =
    [ SectionTitle "I see two types of statistics. Why ?"
    , SectionDescription "Statistics can be displayed either for the whole poll globally, or on a question basis."
    , SectionIllustration 1 "Global statistics" "/help/stats-1.png"
    , SectionDescription "Global statistics are displayed at the bottom of a poll. They reflect the current state of the answers, and are available for all the questions of the poll, even if they're hidden or not. This lets you easily glance at the results, and compare the answers."
    , SectionIllustration 2 "Question statistics" "/help/stats-2.png"
    , SectionDescription "More precise statistics are provided in the question details. Just open a question, and click the dedicated icon to see answers varying over time for the said question. This is useful when you know that there are some \"hot topics\" in your presentation, and want real-time feedback and appreciation on what people think."
    , SectionDescription "Maybe try it with a question for which answers vary over time, like \"Am I enjoying this presentation ?\" - this will give you some true real-time feedback 😉"
    ]


{-| Represents the help section about voting
-}
voting : List (SectionContent msg)
voting =
    [ SectionTitle "The Android companion app"
    , SectionDescription "To submit votes, participants must download the associated app from Google Play. They'll be prompted with screen that asks them to either fill in the emoji code, or flash a QR code. Both of these can be accessed from the \"Participant View\"."
    , SectionIllustration 1 "The app on Google Play" "/help/voting-1.png"
    , SectionDescription "Before participants can connect, you must open the poll. A poll can be open, closed, or closed to newcomers - this way, if you feel like you don't want new participants to join the poll, you can still allow currently connected participants to answer questions."
    , SectionTitle "What participants can do"
    , SectionDescription "Participants can navigate in the poll, and will automatically get poll and question updates as you make them. They can also provide some votes for your answers for each visible question. If a question is hidden or archived, it won't be visible to them."
    , SectionIllustration 2 "Listing the questions" "/help/voting-2.png"
    , SectionDescription "On mobile, navigation between questions is performed via either the question list, or by clicking the dedicated \"previous\" and \"next\" buttons."
    , SectionTip "Pro Tip : Disconnecting participants 🎯"
        [ Html.text "To disconnect all the participants, you should simply "
        , Html.span [ Attribute.class "font-semibold" ] [ Html.text "close" ]
        , Html.text " the poll. All the current access keys for participants will be revoked, and no more answers will be accepted !"
        ]
    ]
