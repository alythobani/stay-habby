module Models.Graph exposing
    ( GraphData
    , IntervalSuccessStatus
    , NumberOfDaysToShow(..)
    , Point
    , amountAxisConfig
    , amountIntToTickConfig
    , customConfig
    , dateAxisConfig
    , getAllGraphData
    , getAllGraphIntervalData
    , intervalGraphDataToLine
    )

import Array
import Color
import DefaultServices.Util as Util
import Html exposing (div, text)
import Html.Attributes exposing (class)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Maybe.Extra as Maybe
import Models.Habit as Habit
import Models.HabitData exposing (HabitData)
import Models.HabitDayNote exposing (HabitDayNote)
import Models.HabitGoalIntervalList exposing (HabitGoalInterval)
import Models.YmdDate as YmdDate


type alias Point =
    { dateFloat : Float
    , amountFloat : Float
    , goalIntervalIndex : Int
    , note : Maybe String
    }


type alias GraphData =
    List Point


getAllGraphData : List HabitGoalInterval -> List HabitData -> List HabitDayNote -> String -> GraphData
getAllGraphData goalIntervals allHabitData allNotes graphHabitId =
    goalIntervals
        |> List.indexedMap
            (\goalIntervalIndex goalInterval ->
                YmdDate.numDaysSpanned goalInterval.startDate goalInterval.endDate
                    |> List.range 0
                    |> List.map
                        (\numDaysToAdd ->
                            let
                                pointDate : YmdDate.YmdDate
                                pointDate =
                                    YmdDate.addDays numDaysToAdd goalInterval.startDate

                                habitDatum : Int
                                habitDatum =
                                    List.filter (\{ habitId, date } -> habitId == graphHabitId && date == pointDate) allHabitData
                                        |> List.head
                                        |> Maybe.map .amount
                                        |> Maybe.withDefault 0

                                note : Maybe String
                                note =
                                    List.filter (\{ habitId, date } -> habitId == graphHabitId && date == pointDate) allNotes
                                        |> List.head
                                        |> Maybe.map .note
                            in
                            { amountFloat = toFloat habitDatum
                            , goalIntervalIndex = goalIntervalIndex
                            , note = note
                            }
                        )
            )
        |> List.concat
        |> List.indexedMap
            (\pointIndex point ->
                { dateFloat = toFloat pointIndex
                , amountFloat = point.amountFloat
                , goalIntervalIndex = point.goalIntervalIndex
                , note = point.note
                }
            )


type IntervalSuccessStatus
    = Successful
    | Failed
    | SuspendedOrIncomplete


getAllGraphIntervalData :
    List HabitGoalInterval
    -> Habit.Habit
    -> List HabitData
    -> List HabitDayNote
    -> String
    -> List ( IntervalSuccessStatus, GraphData )
getAllGraphIntervalData allGoalIntervals graphHabit allHabitData allNotes habitId =
    let
        allPoints =
            getAllGraphData allGoalIntervals allHabitData allNotes habitId
    in
    List.indexedMap
        (\goalIntervalIndex goalInterval ->
            let
                successStatus : IntervalSuccessStatus
                successStatus =
                    if goalInterval.suspended || not goalInterval.valid then
                        SuspendedOrIncomplete

                    else if goalInterval.successful then
                        Successful

                    else
                        Failed

                goalIntervalPoints =
                    List.filter (\point -> point.goalIntervalIndex == goalIntervalIndex) allPoints

                maybePreviousPointWithIndex =
                    Util.lastInstanceInArray (Array.fromList allPoints) (\point -> point.goalIntervalIndex == goalIntervalIndex - 1)

                goalIntervalPointsWithConnectorPoint : List Point
                goalIntervalPointsWithConnectorPoint =
                    case maybePreviousPointWithIndex of
                        Just ( previousPointIndex, previousPoint ) ->
                            previousPoint :: goalIntervalPoints

                        Nothing ->
                            goalIntervalPoints
            in
            ( successStatus, goalIntervalPointsWithConnectorPoint )
        )
        allGoalIntervals


intervalGraphDataToLine : Habit.Habit -> Bool -> ( IntervalSuccessStatus, GraphData ) -> LineChart.Series Point
intervalGraphDataToLine graphHabit darkModeOn ( successStatus, goalIntervalData ) =
    let
        ( successColor, failureColor ) =
            case graphHabit of
                Habit.GoodHabit _ ->
                    if darkModeOn then
                        ( Color.green, Color.blue )

                    else
                        ( Color.darkGreen, Color.darkBlue )

                Habit.BadHabit _ ->
                    if darkModeOn then
                        ( Color.yellow, Color.red )

                    else
                        ( Color.darkYellow, Color.red )

        suspendedColor =
            if darkModeOn then
                Color.lightGray

            else
                Color.darkGray

        lineColor =
            case successStatus of
                Successful ->
                    successColor

                Failed ->
                    failureColor

                SuspendedOrIncomplete ->
                    suspendedColor
    in
    LineChart.line
        lineColor
        Dots.circle
        "Goal Interval"
        goalIntervalData


type NumberOfDaysToShow
    = AllTime
    | LastMonth
    | LastThreeMonths
    | LastYear



-- Line Chart Configuration


customConfig : List HabitGoalInterval -> Bool -> List HabitData -> List HabitDayNote -> String -> Maybe Point -> (Maybe Point -> msg) -> LineChart.Config Point msg
customConfig goalIntervals darkModeOn allHabitData allNotes graphHabitId maybeHoveredPoint onGraphPointHover =
    let
        maybeStartYmd =
            List.head goalIntervals |> Maybe.map .startDate

        allGraphData : GraphData
        allGraphData =
            getAllGraphData goalIntervals allHabitData allNotes graphHabitId

        allGraphDataWithNotes : GraphData
        allGraphDataWithNotes =
            List.filter (\point -> Maybe.isJust point.note) allGraphData

        maybeHoveredPointWithNote : Maybe Point
        maybeHoveredPointWithNote =
            case maybeHoveredPoint of
                Just hoveredPoint ->
                    if List.member hoveredPoint allGraphDataWithNotes then
                        Just hoveredPoint

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    { x = dateAxisConfig maybeStartYmd darkModeOn
    , y = amountAxisConfig darkModeOn
    , container = Container.default "graph-container-id"
    , interpolation = Interpolation.linear
    , intersection = Intersection.at 0 0
    , legends = Legends.none
    , events = Events.hoverOne onGraphPointHover
    , junk =
        Junk.custom
            (\system ->
                { below = []
                , above = []
                , html =
                    case ( maybeHoveredPointWithNote, maybeStartYmd ) of
                        ( Just point, Just startYmd ) ->
                            [ div
                                [ class "graph-note-box" ]
                                [ div
                                    [ class "graph-note-box-date" ]
                                    [ text <| YmdDate.prettyPrintWithWeekday <| YmdDate.addDays (round point.dateFloat) startYmd ]
                                , div [ class "graph-note-box-date-line" ] []
                                , div
                                    [ class "graph-note-box-note" ]
                                    (List.map
                                        (\noteLine ->
                                            div
                                                [ class "graph-note-box-note-line" ]
                                                [ text <|
                                                    if noteLine == "" then
                                                        "--"

                                                    else
                                                        noteLine
                                                ]
                                        )
                                        (String.split "\n" (Maybe.withDefault "" point.note))
                                    )
                                ]
                            ]

                        _ ->
                            []
                }
            )
    , grid = Grid.default
    , area = Area.normal 0.5
    , line = Line.default
    , dots =
        Dots.customAny
            { legend = always <| Dots.full 0
            , individual =
                \point ->
                    if List.member point allGraphDataWithNotes then
                        if maybeHoveredPoint == Just point then
                            Dots.aura 10 10 0.5

                        else
                            Dots.aura 5 5 0.5

                    else
                        Dots.full 0
            }
    }


dateAxisConfig : Maybe YmdDate.YmdDate -> Bool -> Axis.Config Point msg
dateAxisConfig maybeStartYmd darkModeOn =
    Axis.custom
        { title = Title.default ""
        , variable = Just << .dateFloat
        , pixels = 700
        , range = Range.padded 20 20
        , axisLine = AxisLine.none
        , ticks =
            Ticks.custom <|
                \dateIntRange amountRange ->
                    List.map
                        (ceiling >> dateIntToTickConfig maybeStartYmd darkModeOn)
                        [ dateIntRange.min
                        , (dateIntRange.min + dateIntRange.max) / 2
                        , dateIntRange.max
                        ]
        }


dateIntToTickConfig : Maybe YmdDate.YmdDate -> Bool -> Int -> Tick.Config msg
dateIntToTickConfig maybeStartYmd darkModeOn numDaysToAdd =
    let
        labelStr =
            case maybeStartYmd of
                Just startYmd ->
                    startYmd |> YmdDate.addDays numDaysToAdd |> YmdDate.prettyPrintShortForm

                Nothing ->
                    String.fromInt numDaysToAdd

        labelColor =
            if darkModeOn then
                Color.white

            else
                Color.black
    in
    Tick.custom
        { position = toFloat numDaysToAdd
        , color = Colors.transparent
        , width = 2
        , length = 2
        , grid = False
        , direction = Tick.negative
        , label = Just <| Junk.label labelColor labelStr
        }


amountAxisConfig : Bool -> Axis.Config Point msg
amountAxisConfig darkModeOn =
    let
        axisColor =
            if darkModeOn then
                Color.white

            else
                Color.black
    in
    Axis.custom
        { title = Title.default ""
        , variable = Just << .amountFloat
        , pixels = 500
        , range =
            Range.custom
                (\range ->
                    { min = min 0 range.min
                    , max = range.max * 1.5
                    }
                )
        , axisLine = AxisLine.full axisColor
        , ticks = Ticks.intCustom 7 (amountIntToTickConfig darkModeOn)
        }


amountIntToTickConfig : Bool -> Int -> Tick.Config msg
amountIntToTickConfig darkModeOn amountInt =
    let
        labelColor =
            if darkModeOn then
                Color.white

            else
                Color.black
    in
    Tick.custom
        { position = toFloat amountInt
        , color = Colors.transparent
        , width = 2
        , length = 2
        , grid = False
        , direction = Tick.negative
        , label = Just <| Junk.label labelColor (String.fromInt amountInt)
        }
