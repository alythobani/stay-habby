module Api exposing (HabitsAndHabitDataAndFrequencyStats, QueriedFrequencyStats, graphQLRequest, mutationAddHabit, mutationSetHabitData, mutationToggleSuspendedHabit, queryHabitsAndHabitDataAndFrequencyStats, queryPastFrequencyStats)

import DefaultServices.Http exposing (post)
import DefaultServices.Util as Util
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode
import Models.ApiError exposing (ApiError)
import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.SuspendedToggleEvent as SuspendedToggleEvent
import Models.YmdDate as YmdDate


{-| Send the `query` to the graphql endpoint.
-}
graphQLRequest : String -> Decode.Decoder a -> String -> (ApiError -> b) -> (a -> b) -> Cmd b
graphQLRequest query decoder url handleError handleSuccess =
    post url decoder (Encode.object [ ( "query", Encode.string query ) ]) handleError handleSuccess


type alias HabitsAndHabitDataAndFrequencyStats =
    { habits : List Habit.Habit
    , habitData : List HabitData.HabitData
    , frequencyStatsList : List FrequencyStats.FrequencyStats
    }


{-| Query for all fields on all habits and habit data, plus their frequency stats.
-}
queryHabitsAndHabitDataAndFrequencyStats :
    YmdDate.YmdDate
    -> String
    -> (ApiError -> b)
    -> (HabitsAndHabitDataAndFrequencyStats -> b)
    -> Cmd b
queryHabitsAndHabitDataAndFrequencyStats ymd =
    let
        queryString =
            """{
  habits: get_habits {
    __typename
    ... on good_habit {
      _id
      description
      name
      unit_name_singular
      unit_name_plural
      target_frequencies {
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
        new_frequency {
          __typename
          ... on every_x_days_frequency {
            days
            times
          }
          ... on total_week_frequency {
            week
          }
          ... on specific_day_of_week_frequency {
            monday
            tuesday
            wednesday
            thursday
            friday
            saturday
            sunday
          }
        }
      }
      time_of_day
    }
    ... on bad_habit {
      _id
      description
      name
      unit_name_singular
      unit_name_plural
      threshold_frequencies {
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
        new_frequency {
          __typename
          ... on every_x_days_frequency {
            days
            times
          }
          ... on total_week_frequency {
            week
          }
          ... on specific_day_of_week_frequency {
            monday
            tuesday
            wednesday
            thursday
            friday
            saturday
            sunday
          }
        }
      }
    }
  }
  habitData: get_habit_data {
    _id
    amount
    date {
      day
      month
      year
    }
    habit_id
  }
  frequencyStatsList: get_frequency_stats(current_client_date: {year: """
                ++ toString ymd.year
                ++ ", month: "
                ++ toString ymd.month
                ++ ", day: "
                ++ toString ymd.day
                ++ """}) {
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
  }
}"""
    in
    graphQLRequest
        queryString
        (decode HabitsAndHabitDataAndFrequencyStats
            |> required "habits" (Decode.list Habit.decodeHabit)
            |> required "habitData" (Decode.list HabitData.decodeHabitData)
            |> required "frequencyStatsList" (Decode.list FrequencyStats.decodeFrequencyStats)
            |> Decode.at [ "data" ]
        )


type alias QueriedFrequencyStats =
    { frequencyStatsList : List FrequencyStats.FrequencyStats }


{-| Query for all fields on all habits and habit data, plus their frequency stats.
-}
queryPastFrequencyStats :
    YmdDate.YmdDate
    -> List String
    -> String
    -> (ApiError -> b)
    -> (QueriedFrequencyStats -> b)
    -> Cmd b
queryPastFrequencyStats ymd habitIds =
    let
        queryString =
            "{frequencyStatsList: get_frequency_stats(current_client_date: {year: "
                ++ toString ymd.year
                ++ ", month: "
                ++ toString ymd.month
                ++ ", day: "
                ++ toString ymd.day
                ++ "}"
                ++ (if List.isEmpty habitIds then
                        ""

                    else
                        ", habit_ids: " ++ toString habitIds
                   )
                ++ """) {
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
  }
}"""
    in
    graphQLRequest
        queryString
        (decode QueriedFrequencyStats
            |> required "frequencyStatsList" (Decode.list FrequencyStats.decodeFrequencyStats)
            |> Decode.at [ "data" ]
        )


mutationAddHabit : Habit.CreateHabit -> YmdDate.YmdDate -> String -> (ApiError -> b) -> (Habit.Habit -> b) -> Cmd b
mutationAddHabit createHabit { day, month, year } =
    let
        commonFields =
            Habit.getCommonCreateFields createHabit

        templateDict =
            Dict.fromList
                [ ( "type_name"
                  , if isGoodHabit then
                        "good_habit"

                    else
                        "bad_habit"
                  )
                , ( "name", commonFields.name )
                , ( "description", commonFields.description )
                , ( "time_of_day"
                  , case createHabit of
                        Habit.CreateGoodHabit { timeOfDay } ->
                            "time_of_day: " ++ (toString timeOfDay |> String.toUpper) ++ ","

                        _ ->
                            ""
                  )
                , ( "unit_name_singular", commonFields.unitNameSingular )
                , ( "unit_name_plural", commonFields.unitNamePlural )
                , ( "initial_frequency_name"
                  , if isGoodHabit then
                        "initial_target_frequency"

                    else
                        "initial_threshold_frequency"
                  )
                , ( "initial_frequency_value"
                  , case commonFields.initialFrequency of
                        Habit.EveryXDayFrequency { days, times } ->
                            Util.templater
                                (Dict.fromList [ ( "days", toString days ), ( "times", toString times ) ])
                                """{
                                type_name: "every_x_days_frequency",
                                every_x_days_frequency: {
                                    days: {{days}},
                                    times: {{times}}
                                }
                                }"""

                        Habit.TotalWeekFrequency times ->
                            Util.templater
                                (Dict.fromList [ ( "times", toString times ) ])
                                """{
                                type_name: "total_week_frequency",
                                total_week_frequency: {
                                    week: {{times}}
                                }
                                }"""

                        Habit.SpecificDayOfWeekFrequency { monday, tuesday, wednesday, thursday, friday, saturday, sunday } ->
                            Util.templater
                                (Dict.fromList
                                    [ ( "monday", toString monday )
                                    , ( "tuesday", toString tuesday )
                                    , ( "wednesday", toString wednesday )
                                    , ( "thursday", toString thursday )
                                    , ( "friday", toString friday )
                                    , ( "saturday", toString saturday )
                                    , ( "sunday", toString sunday )
                                    ]
                                )
                                """{
                                type_name: "specific_day_of_week_frequency",
                                specific_day_of_week_frequency: {
                                monday: {{monday}},
                                tuesday: {{tuesday}},
                                wednesday: {{wednesday}},
                                thursday: {{thursday}},
                                friday: {{friday}},
                                saturday: {{saturday}},
                                sunday: {{sunday}}
                                }
                                }"""
                  )
                , ( "day", toString day )
                , ( "month", toString month )
                , ( "year", toString year )
                ]

        isGoodHabit =
            case createHabit of
                Habit.CreateGoodHabit _ ->
                    True

                _ ->
                    False

        queryString =
            """mutation {
              add_habit(create_habit_data: {
                type_name: "{{type_name}}",
                {{type_name}}: {
                  name: "{{name}}",
                  description: "{{description}}",
                  {{time_of_day}}
                  {{initial_frequency_name}}: {{initial_frequency_value}},
                  unit_name_singular: "{{unit_name_singular}}",
                  unit_name_plural: "{{unit_name_plural}}"
                }
              }, creation_date: {
                day: {{day}},
                month: {{month}},
                year: {{year}}
              }) {
                __typename
                ... on good_habit {
                  _id
                  description
                  name
                  unit_name_singular
                  unit_name_plural
                  target_frequencies {
                    frequency_change_date {
                      day
                      month
                      year
                    }
                    new_frequency {
                      __typename
                      ... on every_x_days_frequency {
                        days
                        times
                      }
                      ... on total_week_frequency {
                        week
                      }
                      ... on specific_day_of_week_frequency {
                        monday
                        tuesday
                        wednesday
                        thursday
                        friday
                        saturday
                        sunday
                      }
                    }
                  }
                  time_of_day
                }
                ... on bad_habit {
                  _id
                  description
                  name
                  unit_name_singular
                  unit_name_plural
                  threshold_frequencies {
                    frequency_change_date {
                      day
                      month
                      year
                    }
                    new_frequency {
                      __typename
                      ... on every_x_days_frequency {
                        days
                        times
                      }
                      ... on total_week_frequency {
                        week
                      }
                      ... on specific_day_of_week_frequency {
                        monday
                        tuesday
                        wednesday
                        thursday
                        friday
                        saturday
                        sunday
                      }
                    }
                  }
                }
              }
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest queryString <| Decode.at [ "data", "add_habit" ] Habit.decodeHabit


mutationSetHabitData : YmdDate.YmdDate -> String -> Int -> String -> (ApiError -> b) -> (HabitData.HabitData -> b) -> Cmd b
mutationSetHabitData { day, month, year } habitId amount =
    let
        templateDict =
            Dict.fromList <|
                [ ( "day", toString day )
                , ( "month", toString month )
                , ( "year", toString year )
                , ( "amount", toString amount )
                , ( "habit_id", habitId )
                ]

        query =
            """mutation {
\tset_habit_data(date: { day: {{day}}, month: {{month}}, year: {{year}}}, amount: {{amount}}, habit_id: "{{habit_id}}") {
\t\t_id,
\t\tamount,
\t\tdate {
\t\t\tyear,
\t\t\tmonth,
\t\t\tday
\t\t},
\t\thabit_id
\t}
}"""
                |> Util.templater templateDict
    in
    graphQLRequest query (Decode.at [ "data", "set_habit_data" ] HabitData.decodeHabitData)


mutationToggleSuspendedHabit :
    YmdDate.YmdDate
    -> String
    -> Bool
    -> String
    -> (ApiError -> b)
    -> (SuspendedToggleEvent.SuspendedToggleEvent -> b)
    -> Cmd b
mutationToggleSuspendedHabit { day, month, year } habitId suspended =
    let
        templateDict =
            Dict.fromList <|
                [ ( "day", toString day )
                , ( "month", toString month )
                , ( "year", toString year )
                , ( "suspended", Util.encodeBool suspended )
                , ( "habit_id", habitId )
                ]

        query =
            """mutation {
\ttoggle_suspended_habit(toggle_date: { day: {{day}}, month: {{month}}, year: {{year}}}, suspended: {{suspended}}, habit_id: "{{habit_id}}") {
\t\t_id,
\t\tsuspended,
\t\ttoggle_date {
\t\t\tyear,
\t\t\tmonth,
\t\t\tday
\t\t},
\t\thabit_id
\t}
}"""
                |> Util.templater templateDict
    in
    graphQLRequest query (Decode.at [ "data", "toggle_suspended_habit" ] SuspendedToggleEvent.decodeSuspendedToggleEvent)
