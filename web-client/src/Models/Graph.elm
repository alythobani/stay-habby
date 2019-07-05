module Models.Graph exposing
    ( GraphData
    , IntervalSuccessStatus
    , NumberOfDaysToShow(..)
    , Point
    , amountAxisConfig
    , amountIntToTickConfig
    , customConfig
    , dateAxisConfig
    , dateIntToTickConfig
    , getAllGraphData
    , getAllGraphIntervalData
    , intervalGraphDataToLine
    )

import Array
import Color
import DefaultServices.Util as Util
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
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.HabitGoalIntervalList exposing (HabitGoalInterval)
import Models.YmdDate as YmdDate


type alias Point =
    { dateFloat : Float
    , amountFloat : Float
    , goalIntervalIndex : Int
    }


type alias GraphData =
    List Point


getAllGraphData : List HabitGoalInterval -> List HabitData.HabitData -> String -> GraphData
getAllGraphData goalIntervals allHabitData graphHabitId =
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
                            in
                            { amountFloat = toFloat habitDatum
                            , goalIntervalIndex = goalIntervalIndex
                            }
                        )
            )
        |> List.concat
        |> List.indexedMap
            (\pointIndex point ->
                { dateFloat = toFloat pointIndex
                , amountFloat = point.amountFloat
                , goalIntervalIndex = point.goalIntervalIndex
                }
            )


type IntervalSuccessStatus
    = Successful
    | Failed
    | SuspendedOrIncomplete


getAllGraphIntervalData :
    List HabitGoalInterval
    -> Habit.Habit
    -> List HabitData.HabitData
    -> String
    -> List ( IntervalSuccessStatus, GraphData )
getAllGraphIntervalData allGoalIntervals graphHabit allHabitData habitId =
    let
        allPoints =
            getAllGraphData allGoalIntervals allHabitData habitId
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
                Color.gray

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
        Dots.none
        "Goal Interval"
        goalIntervalData


type NumberOfDaysToShow
    = AllTime
    | LastMonth
    | LastThreeMonths
    | LastYear



-- Line Chart Configuration


customConfig : List HabitGoalInterval -> Bool -> LineChart.Config Point msg
customConfig goalIntervals darkModeOn =
    let
        maybeStartYmd =
            List.head goalIntervals |> Maybe.map .startDate
    in
    { x = dateAxisConfig maybeStartYmd
    , y = amountAxisConfig
    , container = Container.default "graph-container-id"
    , interpolation = Interpolation.linear
    , intersection = Intersection.at 0 0
    , legends = Legends.none
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.custom <| Dots.aura 1 1 0.5
    }


dateAxisConfig : Maybe YmdDate.YmdDate -> Axis.Config Point msg
dateAxisConfig maybeStartYmd =
    Axis.custom
        { title = Title.default ""
        , variable = Just << .dateFloat
        , pixels = 700
        , range = Range.padded 20 20
        , axisLine = AxisLine.none
        , ticks = Ticks.intCustom 3 (dateIntToTickConfig maybeStartYmd)
        }


dateIntToTickConfig : Maybe YmdDate.YmdDate -> Int -> Tick.Config msg
dateIntToTickConfig maybeStartYmd numDaysToAdd =
    let
        labelStr =
            case maybeStartYmd of
                Just startYmd ->
                    startYmd |> YmdDate.addDays numDaysToAdd |> YmdDate.prettyPrintShortForm

                Nothing ->
                    String.fromInt numDaysToAdd
    in
    Tick.custom
        { position = toFloat numDaysToAdd
        , color = Colors.transparent
        , width = 2
        , length = 2
        , grid = False
        , direction = Tick.negative
        , label = Just <| Junk.label Color.white labelStr
        }


amountAxisConfig : Axis.Config Point msg
amountAxisConfig =
    Axis.custom
        { title = Title.default ""
        , variable = Just << .amountFloat
        , pixels = 500
        , range = Range.padded 20 20
        , axisLine = AxisLine.full Color.white
        , ticks = Ticks.intCustom 7 amountIntToTickConfig
        }


amountIntToTickConfig : Int -> Tick.Config msg
amountIntToTickConfig amountInt =
    Tick.custom
        { position = toFloat amountInt
        , color = Colors.transparent
        , width = 2
        , length = 2
        , grid = False
        , direction = Tick.negative
        , label = Just <| Junk.label Color.white (String.fromInt amountInt)
        }
