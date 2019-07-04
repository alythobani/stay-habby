module Models.Graph exposing
    ( GraphData
    , NumberOfDaysToShow(..)
    , Point
    , amountAxisConfig
    , customConfig
    , dateAxisConfig
    , getGraphData
    , makeGraphDataLine
    )

import Color
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
import Models.HabitGoalIntervalList as HabitGoalIntervalList
import Models.YmdDate as YmdDate


type alias Point =
    { dateFloat : Float
    , amountFloat : Float
    }


type alias GraphData =
    List Point


getGraphData : List HabitGoalIntervalList.HabitGoalInterval -> GraphData
getGraphData goalIntervals =
    List.map
        (\goalInterval ->
            YmdDate.numDaysSpanned goalInterval.startDate goalInterval.endDate
                |> List.range 0
                |> List.map (always <| toFloat goalInterval.totalDone)
        )
        goalIntervals
        |> List.concat
        |> List.indexedMap (\index totalDoneFloat -> Point (toFloat index) totalDoneFloat)


type NumberOfDaysToShow
    = AllTime
    | LastMonth
    | LastThreeMonths
    | LastYear



-- Line Chart Configuration


customConfig : LineChart.Config Point msg
customConfig =
    { x = dateAxisConfig
    , y = amountAxisConfig
    , container = Container.default "graph-container-id"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.at 0 0
    , legends = Legends.none
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.custom <| Dots.aura 1 1 0.5
    }


dateAxisConfig : Axis.Config Point msg
dateAxisConfig =
    Axis.custom
        { title = Title.default ""
        , variable = Just << .dateFloat
        , pixels = 700
        , range = Range.padded 20 20
        , axisLine = AxisLine.none
        , ticks = Ticks.intCustom 7 dateIntToTickConfig
        }


dateIntToTickConfig : Int -> Tick.Config msg
dateIntToTickConfig numDaysToAdd =
    Tick.custom
        { position = toFloat numDaysToAdd
        , color = Colors.transparent
        , width = 2
        , length = 2
        , grid = False
        , direction = Tick.negative
        , label = Just <| Junk.label Color.white (String.fromInt numDaysToAdd)
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


makeGraphDataLine : GraphData -> LineChart.Series Point
makeGraphDataLine graphData =
    LineChart.line Color.green Dots.none "Habit Data" graphData
