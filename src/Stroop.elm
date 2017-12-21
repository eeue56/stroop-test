module Stroop exposing (..)

import Html
import Html.Attributes
import Html.Events
import Keyboard.Extra exposing (..)
import Json.Decode as Json


type StroopDirection
    = Left
    | Right
    | Up
    | Down


type alias ChoiceStroop =
    { left : String
    , right : String
    , up : String
    , down : String
    , correct : StroopDirection
    , resource : String
    }


type Color
    = Red
    | Green
    | Blue
    | Yellow


type alias ColouredStroop =
    { rightColor : Color
    , text : String
    }


type Stroop
    = ColouredStroopValue ColouredStroop
    | ChoiceStroopValue ChoiceStroop


stringToColor : String -> Color
stringToColor string =
    if string == "red" then
        Red
    else if string == "green" then
        Green
    else if string == "blue" then
        Blue
    else
        Yellow


viewLeft : String -> Html.Html msg
viewLeft leftText =
    Html.div
        [ Html.Attributes.class "left-value"
        , Html.Attributes.style [ ( "position", "absolute" ), ( "left", "40%" ), ( "top", "50%" ) ]
        ]
        [ Html.text leftText ]


viewRight : String -> Html.Html msg
viewRight rightText =
    Html.div
        [ Html.Attributes.class "right-value"
        , Html.Attributes.style [ ( "position", "absolute" ), ( "left", "60%" ), ( "top", "50%" ) ]
        ]
        [ Html.text rightText ]


viewUp : String -> Html.Html msg
viewUp upText =
    Html.div
        [ Html.Attributes.class "up-value"
        , Html.Attributes.style [ ( "position", "absolute" ), ( "left", "50%" ), ( "top", "40%" ) ]
        ]
        [ Html.text upText ]


viewDown : String -> Html.Html msg
viewDown downText =
    Html.div
        [ Html.Attributes.class "down-value"
        , Html.Attributes.style [ ( "position", "absolute" ), ( "left", "50%" ), ( "top", "60%" ) ]
        ]
        [ Html.text downText ]


viewResource : String -> Html.Html msg
viewResource resourceText =
    Html.div
        [ Html.Attributes.class "resource-value"
        , Html.Attributes.style [ ( "position", "absolute" ), ( "left", "50%" ), ( "top", "50%" ) ]
        ]
        [ Html.text resourceText ]


onKey : (Keyboard.Extra.Key -> msg) -> Html.Attribute msg
onKey tagger =
    Html.Events.on "keydown" (Json.map tagger Keyboard.Extra.targetKey)


keyToStroopDirection : Keyboard.Extra.Key -> Maybe StroopDirection
keyToStroopDirection key =
    case key of
        ArrowLeft ->
            Just Left

        ArrowRight ->
            Just Right

        ArrowUp ->
            Just Up

        ArrowDown ->
            Just Down

        _ ->
            Nothing


onArrow : (Maybe StroopDirection -> msg) -> Html.Attribute msg
onArrow onGroupChosen =
    (keyToStroopDirection >> onGroupChosen)
        |> onKey


stringToSide : String -> StroopDirection
stringToSide str =
    if str == "Left" then
        Left
    else if str == "Right" then
        Right
    else if str == "Up" then
        Up
    else
        Down


choiceStroopWhichIsCorrect : String -> ChoiceStroop -> ChoiceStroop
choiceStroopWhichIsCorrect correct group =
    if group.left == correct then
        { group | correct = Left }
    else if group.right == correct then
        { group | correct = Right }
    else if group.up == correct then
        { group | correct = Up }
    else
        { group | correct = Down }


choiceStroopDirectionValue : ChoiceStroop -> StroopDirection -> String
choiceStroopDirectionValue group value =
    case value of
        Left ->
            group.left

        Right ->
            group.right

        Up ->
            group.up

        Down ->
            group.down


coloredStroopDirectionValue : ColouredStroop -> StroopDirection -> String
coloredStroopDirectionValue stroop value =
    case value of
        Left ->
            "Red"

        Right ->
            "Green"

        Up ->
            "Yellow"

        Down ->
            "Blue"


pickSide : Stroop -> StroopDirection -> String
pickSide stroop direction =
    case stroop of
        ColouredStroopValue colored ->
            coloredStroopDirectionValue colored direction

        ChoiceStroopValue choice ->
            choiceStroopDirectionValue choice direction


{-|
    >>> textToGroup 0 "noah | mimi | erica | dan | welsh | noah"
    { left = "noah", right = "mimi", up = "erica" down = "dan", resource = "welsh", correct = Left}

    >>> textToGroup 0 "#red | noah"
-}
textToGroup : Int -> String -> Result String Stroop
textToGroup rotation text =
    if String.startsWith "#" text then
        case String.split "|" <| String.dropLeft 1 text of
            [ color, word ] ->
                let
                    strippedColor =
                        String.trim color

                    strippedWord =
                        String.trim word
                in
                    case strippedColor of
                        "red" ->
                            { rightColor = Red, text = strippedWord }
                                |> ColouredStroopValue
                                |> Ok

                        "green" ->
                            { rightColor = Green, text = strippedWord }
                                |> ColouredStroopValue
                                |> Ok

                        "blue" ->
                            { rightColor = Blue, text = strippedWord }
                                |> ColouredStroopValue
                                |> Ok

                        "yellow" ->
                            { rightColor = Yellow, text = strippedWord }
                                |> ColouredStroopValue
                                |> Ok

                        _ ->
                            Err <| "Unknown color" ++ strippedColor

            _ ->
                Err "Invalid number of `|`"
    else
        case String.split "|" text of
            [ left, right, up, down, resource, correct ] ->
                if rotation == 0 then
                    { left = left, right = right, up = up, down = down, resource = resource, correct = Left }
                        |> choiceStroopWhichIsCorrect correct
                        |> ChoiceStroopValue
                        |> Ok
                else if rotation == 1 then
                    { left = right, right = up, up = down, down = left, resource = resource, correct = Left }
                        |> choiceStroopWhichIsCorrect correct
                        |> ChoiceStroopValue
                        |> Ok
                else if rotation == 2 then
                    { left = up, right = down, up = left, down = right, resource = resource, correct = Left }
                        |> choiceStroopWhichIsCorrect correct
                        |> ChoiceStroopValue
                        |> Ok
                else
                    { left = down, right = left, up = right, down = up, resource = resource, correct = Left }
                        |> choiceStroopWhichIsCorrect correct
                        |> ChoiceStroopValue
                        |> Ok

            [ color, value ] ->
                Err ""

            _ ->
                Err "Failed to find the right number of matches"


view : Stroop -> (Maybe StroopDirection -> msg) -> Html.Html msg
view stroop onGroupChosen =
    case stroop of
        ChoiceStroopValue group ->
            Html.div
                [ onArrow onGroupChosen
                , Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ]
                ]
                [ viewLeft group.left
                , viewRight group.right
                , viewUp group.up
                , viewDown group.down
                , viewResource group.resource
                ]

        _ ->
            Html.text ""
