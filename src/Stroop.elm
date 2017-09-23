module Stroop exposing (..)

import Html
import Html.Attributes
import Html.Events
import Keyboard.Extra exposing (..)
import Json.Decode as Json


type StroopGroupValue
    = Left
    | Right
    | Up
    | Down


type alias StroopGroup =
    { left : String
    , right : String
    , up : String
    , down : String
    , correct : StroopGroupValue
    , resource : String
    }


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


keyToStroopGroupValue : Keyboard.Extra.Key -> Maybe StroopGroupValue
keyToStroopGroupValue key =
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


onArrow : (Maybe StroopGroupValue -> msg) -> Html.Attribute msg
onArrow onGroupChosen =
    (keyToStroopGroupValue >> onGroupChosen)
        |> onKey


whichIsCorrect : String -> StroopGroup -> StroopGroup
whichIsCorrect correct group =
    if group.left == correct then
        { group | correct = Left }
    else if group.right == correct then
        { group | correct = Right }
    else if group.up == correct then
        { group | correct = Up }
    else
        { group | correct = Down }


pickSide : StroopGroup -> StroopGroupValue -> String
pickSide group value =
    case value of
        Left ->
            group.left

        Right ->
            group.right

        Up ->
            group.up

        Down ->
            group.down


{-|
    >>> textToGroup 0 "noah | mimi | erica | dan | welsh | noah"
    { left = "noah", right = "mimi", up = "erica" down = "dan", resource = "welsh", correct = Left}
-}
textToGroup : Int -> String -> Result String StroopGroup
textToGroup rotation text =
    case String.split "|" text of
        [ left, right, up, down, resource, correct ] ->
            if rotation == 0 then
                { left = left, right = right, up = up, down = down, resource = resource, correct = Left }
                    |> whichIsCorrect correct
                    |> Ok
            else if rotation == 1 then
                { left = right, right = up, up = down, down = left, resource = resource, correct = Left }
                    |> whichIsCorrect correct
                    |> Ok
            else if rotation == 2 then
                { left = up, right = down, up = left, down = right, resource = resource, correct = Left }
                    |> whichIsCorrect correct
                    |> Ok
            else
                { left = down, right = left, up = right, down = up, resource = resource, correct = Left }
                    |> whichIsCorrect correct
                    |> Ok

        _ ->
            Err "Failed to find the right number of matches"


view : StroopGroup -> (Maybe StroopGroupValue -> msg) -> Html.Html msg
view group onGroupChosen =
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
