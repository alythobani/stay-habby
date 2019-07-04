module Models.Graph exposing (GraphData, NumberOfDaysToShow(..), Point, getGraphData)

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
