module Models.FrequencyStats exposing
    ( FrequencyStats
    , decodeFrequencyStats
    , graphQLOutputString
    )

import Json.Decode as Decode exposing (null)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias FrequencyStats =
    { habitId : String
    , totalFragments : Int
    , successfulFragments : Int
    , totalDone : Int
    , currentFragmentStreak : Int
    , bestFragmentStreak : Int
    , currentFragmentTotal : Int
    , currentFragmentGoal : Int
    , currentFragmentDaysLeft : Int
    , habitHasStarted : Bool
    , currentlySuspended : Bool
    }


decodeFrequencyStats : Decode.Decoder FrequencyStats
decodeFrequencyStats =
    Decode.succeed FrequencyStats
        |> required "habit_id" Decode.string
        |> required "total_fragments" Decode.int
        |> required "successful_fragments" Decode.int
        |> required "total_done" Decode.int
        |> required "current_fragment_streak" Decode.int
        |> required "best_fragment_streak" Decode.int
        |> required "current_fragment_total" Decode.int
        |> required "current_fragment_goal" Decode.int
        |> required "current_fragment_days_left" Decode.int
        |> required "habit_has_started" Decode.bool
        |> required "currently_suspended" Decode.bool


graphQLOutputString : String
graphQLOutputString =
    """{
        habit_id
        total_fragments
        successful_fragments
        total_done
        current_fragment_streak
        best_fragment_streak
        current_fragment_total
        current_fragment_goal
        current_fragment_days_left
        habit_has_started
        currently_suspended
    }"""
