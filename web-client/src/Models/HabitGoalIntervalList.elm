module Models.HabitGoalIntervalList exposing
    ( HabitGoalInterval
    , HabitGoalIntervalList
    , decodeHabitGoalIntervalList
    , graphQLOutputString
    )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Models.YmdDate as YmdDate


type alias HabitGoalInterval =
    { startDate : YmdDate.YmdDate
    , endDate : YmdDate.YmdDate
    , totalDone : Int
    , successful : Bool
    , suspended : Bool
    , valid : Bool
    , goalAmount : Int
    }


type alias HabitGoalIntervalList =
    { habitId : String
    , goalIntervals : List HabitGoalInterval
    }


decodeHabitGoalInterval : Decode.Decoder HabitGoalInterval
decodeHabitGoalInterval =
    Decode.succeed HabitGoalInterval
        |> required "start_date" YmdDate.decodeYmdDate
        |> required "end_date" YmdDate.decodeYmdDate
        |> required "total_done" Decode.int
        |> required "successful" Decode.bool
        |> required "suspended" Decode.bool
        |> required "valid" Decode.bool
        |> required "goal_amount" Decode.int


decodeHabitGoalIntervalList : Decode.Decoder HabitGoalIntervalList
decodeHabitGoalIntervalList =
    Decode.succeed HabitGoalIntervalList
        |> required "habit_id" Decode.string
        |> required "goal_intervals" (Decode.list decodeHabitGoalInterval)


graphQLOutputString : String
graphQLOutputString =
    """{
        habit_id
        goal_intervals {
          start_date {
            day
            month
            year
          }
          end_date {
            day
            month
            year
          }
          total_done
          successful
          suspended
          valid
          goal_amount
        }
    }"""
