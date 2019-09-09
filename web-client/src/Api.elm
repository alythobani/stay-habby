module Api exposing
    ( AllRemoteData
    , QueriedFrequencyStats
    , QueriedHabitGoalIntervalLists
    , QueriedUser
    , frequencyToGraphQLString
    , graphQLRequest
    , mutationAddHabit
    , mutationAddUser
    , mutationEditHabitGoalFrequencies
    , mutationEditHabitInfo
    , mutationEditHabitSuspensions
    , mutationSetHabitArchived
    , mutationSetHabitData
    , mutationSetHabitDayNote
    , queryAllRemoteData
    , queryFrequencyStats
    , queryHabitGoalIntervalLists
    , queryLoginUser
    )

import DefaultServices.Http exposing (post)
import DefaultServices.Util as Util
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Models.ApiError exposing (ApiError)
import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.HabitDayNote as HabitDayNote
import Models.HabitGoalIntervalList as HabitGoalIntervalList
import Models.Login as Login
import Models.User as User
import Models.YmdDate as YmdDate


{-| Send the `query` to the graphql endpoint.
-}
graphQLRequest : String -> Decode.Decoder a -> String -> (ApiError -> b) -> (a -> b) -> Cmd b
graphQLRequest query decoder url handleError handleSuccess =
    post url decoder (Encode.object [ ( "query", Encode.string query ) ]) handleError handleSuccess


type alias QueriedUser =
    { maybeUser : Maybe User.User
    }


queryLoginUser :
    String
    -> String
    -> String
    -> (ApiError -> b)
    -> (QueriedUser -> b)
    -> Cmd b
queryLoginUser loginFormUsername loginFormPassword =
    let
        templateDict =
            Dict.fromList
                [ ( "user_name_input", Util.encodeString loginFormUsername )
                , ( "user_password_input", Util.encodeString loginFormPassword )
                , ( "maybe_user_output", User.graphQLOutputString )
                ]

        queryString =
            """{
        maybeUser: login_user(
          user_name_input: {{user_name_input}},
          user_password_input: {{user_password_input}}
        ) {{maybe_user_output}}
        }""" |> Util.templater templateDict
    in
    graphQLRequest
        queryString
        (Decode.succeed QueriedUser
            |> required "maybeUser" (Decode.maybe User.decodeUser)
            |> Decode.at [ "data" ]
        )


type alias AllRemoteData =
    { habits : List Habit.Habit
    , habitData : List HabitData.HabitData
    , frequencyStatsList : List FrequencyStats.FrequencyStats
    , habitDayNotes : List HabitDayNote.HabitDayNote
    }


{-| Query for all fields on all habits and habit data, plus their frequency stats.
-}
queryAllRemoteData :
    User.User
    -> YmdDate.YmdDate
    -> String
    -> (ApiError -> b)
    -> (AllRemoteData -> b)
    -> Cmd b
queryAllRemoteData user ymd =
    let
        templateDict =
            Dict.fromList
                [ ( "user_id", Util.encodeString user.id )
                , ( "habit_output", Habit.graphQLOutputString )
                , ( "habit_data_output", HabitData.graphQLOutputString )
                , ( "current_client_date", YmdDate.encodeYmdDate ymd )
                , ( "frequency_stats_output", FrequencyStats.graphQLOutputString )
                , ( "habit_day_note_output", HabitDayNote.graphQLOutputString )
                ]

        queryString =
            """{
  habits: get_habits(user_id: {{user_id}}) {{habit_output}}
  habitData: get_habit_data(user_id: {{user_id}}) {{habit_data_output}}
  frequencyStatsList: get_frequency_stats(
    user_id: {{user_id}},
    current_client_date: {{current_client_date}}) {{frequency_stats_output}}
  habitDayNotes: get_habit_day_notes(user_id: {{user_id}}) {{habit_day_note_output}}
}"""
                |> Util.templater templateDict
    in
    graphQLRequest
        queryString
        (Decode.succeed AllRemoteData
            |> required "habits" (Decode.list Habit.decodeHabit)
            |> required "habitData" (Decode.list HabitData.decodeHabitData)
            |> required "frequencyStatsList" (Decode.list FrequencyStats.decodeFrequencyStats)
            |> required "habitDayNotes" (Decode.list HabitDayNote.decodeHabitDayNote)
            |> Decode.at [ "data" ]
        )


type alias QueriedHabitGoalIntervalLists =
    { habitGoalIntervalLists : List HabitGoalIntervalList.HabitGoalIntervalList }


{-| Query for goal intervals for the given habits.
An empty habit list means we will query for all habits.
-}
queryHabitGoalIntervalLists :
    User.User
    -> Maybe YmdDate.YmdDate
    -> YmdDate.YmdDate
    -> List String
    -> String
    -> (ApiError -> b)
    -> (QueriedHabitGoalIntervalLists -> b)
    -> Cmd b
queryHabitGoalIntervalLists user maybeStartYmd endYmd habitIds =
    let
        templateDict =
            Dict.fromList
                [ ( "user_id", Util.encodeString user.id )
                , ( "start_date", Util.encodeMaybe maybeStartYmd YmdDate.encodeYmdDate )
                , ( "end_date", YmdDate.encodeYmdDate endYmd )
                , ( "habit_ids_input"
                  , if List.isEmpty habitIds then
                        ""

                    else
                        ", habit_ids: " ++ Util.encodeListOfStrings habitIds
                  )
                , ( "goal_interval_list_output", HabitGoalIntervalList.graphQLOutputString )
                ]

        queryString =
            """{habitGoalIntervalLists: get_habit_goal_interval_lists(
                  user_id: {{user_id}}
                  start_date: {{start_date}}
                  end_date: {{end_date}}
                  {{habit_ids_input}}) {{goal_interval_list_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest
        queryString
        (Decode.succeed QueriedHabitGoalIntervalLists
            |> required "habitGoalIntervalLists" (Decode.list HabitGoalIntervalList.decodeHabitGoalIntervalList)
            |> Decode.at [ "data" ]
        )


type alias QueriedFrequencyStats =
    { frequencyStatsList : List FrequencyStats.FrequencyStats }


{-| Query for frequency stats for the given habits, based on habit data up until the given `ymd`.
An empty habit list means we will query for all habits.
-}
queryFrequencyStats :
    User.User
    -> YmdDate.YmdDate
    -> List String
    -> String
    -> (ApiError -> b)
    -> (QueriedFrequencyStats -> b)
    -> Cmd b
queryFrequencyStats user ymd habitIds =
    let
        templateDict =
            Dict.fromList
                [ ( "user_id", Util.encodeString user.id )
                , ( "current_client_date", YmdDate.encodeYmdDate ymd )
                , ( "habit_ids_input"
                  , if List.isEmpty habitIds then
                        ""

                    else
                        ", habit_ids: " ++ Util.encodeListOfStrings habitIds
                  )
                , ( "frequency_stats_output", FrequencyStats.graphQLOutputString )
                ]

        queryString =
            """{frequencyStatsList: get_frequency_stats(
                  user_id: {{user_id}},
                  current_client_date: {{current_client_date}}
                  {{habit_ids_input}}) {{frequency_stats_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest
        queryString
        (Decode.succeed QueriedFrequencyStats
            |> required "frequencyStatsList" (Decode.list FrequencyStats.decodeFrequencyStats)
            |> Decode.at [ "data" ]
        )


mutationAddHabit : Habit.CreateHabit -> YmdDate.YmdDate -> String -> (ApiError -> b) -> (Habit.Habit -> b) -> Cmd b
mutationAddHabit createHabit ymd =
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
                , ( "user_id", Util.encodeString commonFields.userId )
                , ( "name", Util.encodeString commonFields.name )
                , ( "description", Util.encodeString commonFields.description )
                , ( "time_of_day"
                  , case createHabit of
                        Habit.CreateGoodHabit { timeOfDay } ->
                            "time_of_day: " ++ (Debug.toString timeOfDay |> String.toUpper) ++ ","

                        _ ->
                            ""
                  )
                , ( "unit_name_singular", Util.encodeString commonFields.unitNameSingular )
                , ( "unit_name_plural", Util.encodeString commonFields.unitNamePlural )
                , ( "initial_frequency_name"
                  , if isGoodHabit then
                        "initial_target_frequency"

                    else
                        "initial_threshold_frequency"
                  )
                , ( "initial_frequency_value", frequencyToGraphQLString commonFields.initialFrequency )
                , ( "frequency_start_date", YmdDate.encodeYmdDate ymd )
                , ( "habit_output", Habit.graphQLOutputString )
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
                  user_id: {{user_id}},
                  name: {{name}},
                  description: {{description}},
                  {{time_of_day}}
                  {{initial_frequency_name}}: {{initial_frequency_value}},
                  unit_name_singular: {{unit_name_singular}},
                  unit_name_plural: {{unit_name_plural}}
                }
              }, frequency_start_date: {{frequency_start_date}}) {{habit_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest queryString <| Decode.at [ "data", "add_habit" ] Habit.decodeHabit


mutationAddUser : Login.CreateUserFields -> String -> (ApiError -> b) -> (Maybe User.User -> b) -> Cmd b
mutationAddUser createUserFields =
    let
        templateDict =
            Dict.fromList
                [ ( "new_username", Util.encodeString createUserFields.newUsername )
                , ( "new_display_name", Util.encodeString createUserFields.newDisplayName )
                , ( "new_email_address", Util.encodeMaybe createUserFields.newEmailAddress Util.encodeString )
                , ( "new_password", Util.encodeString createUserFields.newPassword )
                , ( "user_output", User.graphQLOutputString )
                ]

        queryString =
            """mutation {
              add_user(
                new_username: {{new_username}},
                new_display_name: {{new_display_name}},
                new_email_address: {{new_email_address}},
                new_password: {{new_password}}
              ) {{user_output}}
          }"""
                |> Util.templater templateDict
    in
    graphQLRequest queryString <| Decode.at [ "data", "add_user" ] (Decode.maybe User.decodeUser)


frequencyToGraphQLString : Habit.Frequency -> String
frequencyToGraphQLString frequency =
    case frequency of
        Habit.EveryXDayFrequency { days, times } ->
            Util.templater
                (Dict.fromList [ ( "days", String.fromInt days ), ( "times", String.fromInt times ) ])
                """{
                  type_name: "every_x_days_frequency",
                  every_x_days_frequency: {
                      days: {{days}},
                      times: {{times}}
                  }
                  }"""

        Habit.TotalWeekFrequency times ->
            Util.templater
                (Dict.fromList [ ( "times", String.fromInt times ) ])
                """{
                  type_name: "total_week_frequency",
                  total_week_frequency: {
                      week: {{times}}
                  }
                  }"""

        Habit.SpecificDayOfWeekFrequency { monday, tuesday, wednesday, thursday, friday, saturday, sunday } ->
            Util.templater
                (Dict.fromList
                    [ ( "monday", String.fromInt monday )
                    , ( "tuesday", String.fromInt tuesday )
                    , ( "wednesday", String.fromInt wednesday )
                    , ( "thursday", String.fromInt thursday )
                    , ( "friday", String.fromInt friday )
                    , ( "saturday", String.fromInt saturday )
                    , ( "sunday", String.fromInt sunday )
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


mutationEditHabitGoalFrequencies :
    User.User
    -> String
    -> List Habit.FrequencyChangeRecord
    -> String
    -> String
    -> (ApiError -> b)
    -> (Habit.Habit -> b)
    -> Cmd b
mutationEditHabitGoalFrequencies user habitId newFrequencies habitType =
    let
        frequencyChangeRecordToGraphQLString : Habit.FrequencyChangeRecord -> String
        frequencyChangeRecordToGraphQLString fcr =
            let
                fcrTemplateDict =
                    Dict.fromList
                        [ ( "start_date", YmdDate.encodeYmdDate fcr.startDate )
                        , ( "end_date", Util.encodeMaybe fcr.endDate YmdDate.encodeYmdDate )
                        , ( "new_frequency", frequencyToGraphQLString fcr.newFrequency )
                        ]
            in
            """{
          start_date: {{start_date}},
          end_date: {{end_date}},
          new_frequency: {{new_frequency}}
          }"""
                |> Util.templater fcrTemplateDict

        templateDict =
            Dict.fromList
                [ ( "user_id", Util.encodeString user.id )
                , ( "habit_type"
                  , if isGoodHabit then
                        "good_habit"

                    else
                        "bad_habit"
                  )
                , ( "habit_id", Util.encodeString habitId )
                , ( "new_frequencies"
                  , newFrequencies
                        |> List.map frequencyChangeRecordToGraphQLString
                        |> String.join ", "
                  )
                , ( "habit_output", Habit.graphQLOutputString )
                ]

        isGoodHabit =
            case habitType of
                "good_habit" ->
                    True

                _ ->
                    False

        queryString =
            """mutation {
            edit_habit_goal_frequencies(
              user_id: {{user_id}},
              habit_id: {{habit_id}},
              habit_type: "{{habit_type}}",
              new_frequencies: [{{new_frequencies}}]
            ) {{habit_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest queryString <| Decode.at [ "data", "edit_habit_goal_frequencies" ] Habit.decodeHabit


mutationEditHabitInfo :
    User.User
    -> String
    -> String
    -> Habit.EditInfoInputData
    -> String
    -> (ApiError -> b)
    -> (Habit.Habit -> b)
    -> Cmd b
mutationEditHabitInfo user habitId habitType newInfo =
    let
        templateDict =
            Dict.fromList <|
                [ ( "user_id", Util.encodeString user.id )
                , ( "habit_id", Util.encodeString habitId )
                , ( "habit_type", Util.encodeString habitType )
                , ( "name", Util.encodeString newInfo.name )
                , ( "description", Util.encodeString newInfo.description )
                , ( "time_of_day", Debug.toString newInfo.goodHabitTime |> String.toUpper )
                , ( "unit_name_singular", Util.encodeString newInfo.unitNameSingular )
                , ( "unit_name_plural", Util.encodeString newInfo.unitNamePlural )
                , ( "habit_output", Habit.graphQLOutputString )
                ]

        query =
            """mutation {
              edit_habit_info(
                user_id: {{user_id}},
                habit_id: {{habit_id}}
                habit_type: {{habit_type}}
                new_info: {
                  name: {{name}}
                  description: {{description}}
                  time_of_day: {{time_of_day}}
                  unit_name_singular: {{unit_name_singular}}
                  unit_name_plural: {{unit_name_plural}}
                }
              ) {{habit_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest query (Decode.at [ "data", "edit_habit_info" ] Habit.decodeHabit)


mutationSetHabitData :
    User.User
    -> YmdDate.YmdDate
    -> String
    -> Int
    -> String
    -> (ApiError -> b)
    -> (HabitData.HabitData -> b)
    -> Cmd b
mutationSetHabitData user ymd habitId amount =
    let
        templateDict =
            Dict.fromList <|
                [ ( "user_id", Util.encodeString user.id )
                , ( "date", YmdDate.encodeYmdDate ymd )
                , ( "amount", Util.encodeInt amount )
                , ( "habit_id", Util.encodeString habitId )
                , ( "habit_data_output", HabitData.graphQLOutputString )
                ]

        query =
            """mutation {
              set_habit_data(
                user_id: {{user_id}},
                date: {{date}},
                amount: {{amount}},
                habit_id: {{habit_id}}
              ) {{habit_data_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest query (Decode.at [ "data", "set_habit_data" ] HabitData.decodeHabitData)


mutationSetHabitDayNote :
    User.User
    -> YmdDate.YmdDate
    -> String
    -> String
    -> String
    -> (ApiError -> b)
    -> (HabitDayNote.HabitDayNote -> b)
    -> Cmd b
mutationSetHabitDayNote user ymd habitId note =
    let
        templateDict =
            Dict.fromList <|
                [ ( "user_id", Util.encodeString user.id )
                , ( "date", YmdDate.encodeYmdDate ymd )
                , ( "note", Util.encodeString note )
                , ( "habit_id", Util.encodeString habitId )
                , ( "habit_day_note_output", HabitDayNote.graphQLOutputString )
                ]

        query =
            """mutation {
              set_habit_day_note(
                user_id: {{user_id}},
                date: {{date}},
                note: {{note}},
                habit_id: {{habit_id}}
              ) {{habit_day_note_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest query (Decode.at [ "data", "set_habit_day_note" ] HabitDayNote.decodeHabitDayNote)


mutationEditHabitSuspensions :
    User.User
    -> String
    -> List Habit.SuspendedInterval
    -> String
    -> (ApiError -> b)
    -> (Habit.Habit -> b)
    -> Cmd b
mutationEditHabitSuspensions user habitId newSuspensions =
    let
        encodeSuspendedInterval : Habit.SuspendedInterval -> String
        encodeSuspendedInterval suspendedInterval =
            let
                suspendedIntervalTemplateDict =
                    Dict.fromList
                        [ ( "start_date", YmdDate.encodeYmdDate suspendedInterval.startDate )
                        , ( "end_date", Util.encodeMaybe suspendedInterval.endDate YmdDate.encodeYmdDate )
                        ]
            in
            """{
              start_date: {{start_date}},
              end_date: {{end_date}}
            }"""
                |> Util.templater suspendedIntervalTemplateDict

        templateDict =
            Dict.fromList
                [ ( "user_id", Util.encodeString user.id )
                , ( "habit_id", Util.encodeString habitId )
                , ( "new_suspensions"
                  , newSuspensions
                        |> List.map encodeSuspendedInterval
                        |> String.join ", "
                  )
                , ( "habit_output", Habit.graphQLOutputString )
                ]

        queryString =
            """mutation {
            edit_habit_suspensions(
              user_id: {{user_id}},
              habit_id: {{habit_id}},
              new_suspensions: [{{new_suspensions}}]
            ) {{habit_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest queryString <| Decode.at [ "data", "edit_habit_suspensions" ] Habit.decodeHabit


mutationSetHabitArchived :
    User.User
    -> String
    -> Bool
    -> String
    -> (ApiError -> b)
    -> (Habit.Habit -> b)
    -> Cmd b
mutationSetHabitArchived user habitId newArchived =
    let
        templateDict =
            Dict.fromList <|
                [ ( "user_id", Util.encodeString user.id )
                , ( "habit_id", Util.encodeString habitId )
                , ( "new_archived", Util.encodeBool newArchived )
                , ( "habit_output", Habit.graphQLOutputString )
                ]

        query =
            """mutation {
              set_habit_archived(
                user_id: {{user_id}},
                habit_id: {{habit_id}},
                new_archived: {{new_archived}}
              ) {{habit_output}}
            }"""
                |> Util.templater templateDict
    in
    graphQLRequest query (Decode.at [ "data", "set_habit_archived" ] Habit.decodeHabit)
