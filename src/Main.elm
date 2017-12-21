module Main exposing (..)

import Array exposing (Array)
import Stroop exposing (..)
import Keyboard.Extra
import Html
import Html.Events
import Time
import Random.Pcg as Random
import Dict


type Msg
    = DecisionMade (Maybe StroopGroupValue)
    | UpdateTime Float
    | UpdateGroups String
    | UpdateSeeds String (List Int)
    | Start


type alias Answer =
    { group : Stroop.StroopGroup
    , answer : StroopGroupValue
    , time : Float
    }


type alias Model =
    { groups : Array Stroop.Stroop
    , currentGroup : Int
    , results : Array ( StroopGroupValue, Float )
    , startTime : Float
    , lastTime : Float
    , started : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecisionMade maybeDecision ->
            case maybeDecision of
                Nothing ->
                    ( model, Cmd.none )

                Just decision ->
                    ( { model
                        | currentGroup = model.currentGroup + 1
                        , startTime =
                            model.lastTime
                        , results =
                            Array.push
                                ( decision, model.lastTime - model.startTime )
                                model.results
                      }
                    , Cmd.none
                    )

        UpdateTime time ->
            if model.startTime == 0 then
                ( { model | startTime = time, lastTime = time }, Cmd.none )
            else
                ( { model | lastTime = time }, Cmd.none )

        UpdateGroups lines ->
            ( { model
                | groups =
                    String.split "\n" lines
                        |> List.filterMap (Stroop.textToGroup 0 >> Result.toMaybe)
                        |> Array.fromList
              }
            , Cmd.none
            )

        UpdateSeeds lines seeds ->
            ( { model
                | groups =
                    List.map2 (\seed line -> Stroop.textToGroup seed line |> Result.toMaybe) seeds (String.split "\n" lines)
                        |> List.filterMap (Basics.identity)
                        |> Array.fromList
              }
            , Cmd.none
            )

        Start ->
            ( { model | started = True, startTime = model.lastTime }, Cmd.none )


viewInstructions : Html.Html Msg
viewInstructions =
    Html.div
        []
        [ Html.text "You will shortly see the names of some of your friends, along with the country they came from. Use your keyboard arrows to choose the person."
        , Html.button [ Html.Events.onClick Start ] [ Html.text "Start" ]
        ]


groupBy : (v -> comparable) -> List v -> Dict.Dict comparable (List v)
groupBy f =
    let
        reducer g x d =
            let
                key =
                    g x

                newValue =
                    x :: Maybe.withDefault [] (Dict.get key d)
            in
                Dict.insert key newValue d
    in
        List.foldl (reducer f) Dict.empty


findOutliers : List Answer -> Int -> List Answer
findOutliers answers time =
    List.filter (\answer -> abs (answer.time - toFloat time) > 500) answers


outlierOverview : List Answer -> Html.Html msg
outlierOverview answers =
    groupBy (.group >> .correct >> toString) answers
        |> Dict.toList
        |> List.map
            (\( correct, answers ) ->
                let
                    averageTime =
                        answers
                            |> List.map .time
                            |> (\xs -> (List.sum xs |> Basics.round) // (List.length xs))

                    outliers =
                        findOutliers answers averageTime
                in
                    outliers
                        |> List.map
                            (\answer ->
                                Html.tr []
                                    [ Html.td [] [ Html.text <| toString answer.time ]
                                    , Html.td [] [ Html.text <| toString answer.group ]
                                    , Html.td [] [ Html.text <| toString answer.answer ]
                                    ]
                            )
            )
        |> (\xs ->
                Html.tr [] [ Html.th [] [ Html.text "Timing" ], Html.th [] [ Html.text "Group" ], Html.th [] [ Html.text "Answer" ] ]
                    :: List.concat xs
           )
        |> Html.table []


answerOverview : List Answer -> Html.Html msg
answerOverview answers =
    groupBy
        (\answer ->
            answer.group.correct |> Stroop.pickSide answer.group |> toString
        )
        answers
        |> Dict.toList
        |> List.map
            (\( correct, answers ) ->
                let
                    answerText =
                        answers
                            |> List.head
                            |> Maybe.map (\answer -> Stroop.pickSide answer.group <| Stroop.stringToSide correct)
                            |> Maybe.withDefault ""
                in
                    List.filter (\answer -> answer.group.correct == answer.answer) answers
                        |> List.map .time
                        |> (\xs -> (List.sum xs |> Basics.round) // (List.length xs))
                        |> (\average -> Html.tr [] [ Html.td [] [ Html.text answerText ], Html.td [] [ Html.text <| toString average ] ])
            )
        |> (\xs ->
                Html.tr [] [ Html.th [] [ Html.text "Value" ], Html.th [] [ Html.text "Average correct timing" ] ]
                    :: xs
           )
        |> Html.table []


resultsToAnswers : Model -> List Answer
resultsToAnswers model =
    List.map2
        (\group ( answer, time ) ->
            { group = group, answer = answer, time = time }
        )
        (Array.toList model.groups)
        (Array.toList model.results)


viewResults : Model -> Html.Html msg
viewResults model =
    List.map2
        (\group ( answer, time ) ->
            Html.tr []
                [ Html.td []
                    [ Html.text <|
                        if group.correct == answer then
                            "Correct"
                        else
                            "Wrong"
                    ]
                , Html.td [] [ Html.text <| toString time ]
                , Html.td [] [ Html.text <| toString <| Stroop.pickSide group answer ]
                , Html.td [] [ Html.text <| toString <| Stroop.pickSide group group.correct ]
                ]
        )
        (Array.toList model.groups)
        (Array.toList model.results)
        |> (\xs ->
                Html.tr [] [ Html.th [] [ Html.text "Correct?" ], Html.th [] [ Html.text "Time" ], Html.th [] [ Html.text "Your answer" ], Html.th [] [ Html.text "Actual answer" ] ]
                    :: xs
           )
        |> Html.table []


view : Model -> Html.Html Msg
view model =
    if model.started then
        case Array.get model.currentGroup model.groups of
            Nothing ->
                Html.div
                    []
                    [ answerOverview <| resultsToAnswers model
                    , outlierOverview <| resultsToAnswers model
                    , viewResults model
                    ]

            Just group ->
                Stroop.view group DecisionMade
    else
        viewInstructions


init : String -> ( Model, Cmd Msg )
init string =
    String.split "\n" string
        |> List.filterMap (Stroop.textToGroup 0 >> Result.toMaybe)
        |> (\groups ->
                ( { groups = Array.fromList groups
                  , currentGroup = 0
                  , startTime = 0
                  , lastTime = 0
                  , results = Array.empty
                  , started = False
                  }
                , Random.generate (UpdateSeeds string) (Random.list (List.length groups) <| Random.int 0 4)
                )
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.Extra.downs (Stroop.keyToStroopGroupValue >> DecisionMade)
        , Time.every Time.millisecond UpdateTime
        ]


main : Program String Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
