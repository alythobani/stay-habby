module Models.Habit exposing (AddHabitInputData, BadHabitRecord, CreateBadHabitRecord, CreateGoodHabitRecord, CreateHabit(..), EditGoalInputData, EveryXDayFrequencyRecord, Frequency(..), FrequencyChangeRecord, FrequencyKind(..), GoodHabitRecord, Habit(..), HabitKind(..), HabitTime(..), SpecificDayOfWeekFrequencyRecord, SuspendedInterval, decodeFrequency, decodeFrequencyChangeRecord, decodeHabit, decodeHabitTime, decodeSuspendedInterval, extractCreateHabit, extractNewGoal, getCommonCreateFields, getCommonFields, graphQLOutputString, initAddHabitData, initEditGoalData, prettyPrintEveryXDayFrequency, prettyPrintFrequency, prettyPrintSpecificDayOfWeekFrequency, prettyPrintTotalWeekFrequency, splitHabits)

import DefaultServices.Util as Util
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Models.YmdDate exposing (YmdDate, decodeYmdDate)


type Habit
    = GoodHabit GoodHabitRecord
    | BadHabit BadHabitRecord


type HabitKind
    = GoodHabitKind
    | BadHabitKind


type alias GoodHabitRecord =
    { id : String
    , name : String
    , description : Maybe String
    , unitNameSingular : String
    , unitNamePlural : String
    , targetFrequencies : List FrequencyChangeRecord
    , timeOfDay : HabitTime
    , suspensions : List SuspendedInterval
    }


type alias BadHabitRecord =
    { id : String
    , name : String
    , description : Maybe String
    , unitNameSingular : String
    , unitNamePlural : String
    , thresholdFrequencies : List FrequencyChangeRecord
    , suspensions : List SuspendedInterval
    }


type alias AddHabitInputData =
    { description : String
    , frequencyKind : FrequencyKind
    , goodHabitTime : HabitTime
    , kind : HabitKind
    , name : String
    , openView : Bool
    , unitNamePlural : String
    , unitNameSingular : String
    , timesPerWeek : Maybe Int
    , mondayTimes : Maybe Int
    , tuesdayTimes : Maybe Int
    , wednesdayTimes : Maybe Int
    , thursdayTimes : Maybe Int
    , fridayTimes : Maybe Int
    , saturdayTimes : Maybe Int
    , sundayTimes : Maybe Int
    , times : Maybe Int
    , days : Maybe Int
    }


type alias EditGoalInputData =
    { frequencyKind : FrequencyKind
    , timesPerWeek : Maybe Int
    , mondayTimes : Maybe Int
    , tuesdayTimes : Maybe Int
    , wednesdayTimes : Maybe Int
    , thursdayTimes : Maybe Int
    , fridayTimes : Maybe Int
    , saturdayTimes : Maybe Int
    , sundayTimes : Maybe Int
    , times : Maybe Int
    , days : Maybe Int
    }


initEditGoalData : EditGoalInputData
initEditGoalData =
    { frequencyKind = TotalWeekFrequencyKind
    , timesPerWeek = Nothing
    , mondayTimes = Nothing
    , tuesdayTimes = Nothing
    , wednesdayTimes = Nothing
    , thursdayTimes = Nothing
    , fridayTimes = Nothing
    , saturdayTimes = Nothing
    , sundayTimes = Nothing
    , times = Nothing
    , days = Nothing
    }


type CreateHabit
    = CreateGoodHabit CreateGoodHabitRecord
    | CreateBadHabit CreateBadHabitRecord


type alias CreateGoodHabitRecord =
    { name : String
    , description : String
    , timeOfDay : HabitTime
    , unitNameSingular : String
    , unitNamePlural : String
    , initialTargetFrequency : Frequency
    }


type alias CreateBadHabitRecord =
    { name : String
    , description : String
    , unitNameSingular : String
    , unitNamePlural : String
    , initialThresholdFrequency : Frequency
    }


type alias SuspendedInterval =
    { startDate : YmdDate
    , endDate : Maybe YmdDate
    }


type alias FrequencyChangeRecord =
    { startDate : YmdDate
    , endDate : Maybe YmdDate
    , newFrequency : Frequency
    }


type Frequency
    = EveryXDayFrequency EveryXDayFrequencyRecord
    | TotalWeekFrequency Int
    | SpecificDayOfWeekFrequency SpecificDayOfWeekFrequencyRecord


type FrequencyKind
    = EveryXDayFrequencyKind
    | TotalWeekFrequencyKind
    | SpecificDayOfWeekFrequencyKind


type alias EveryXDayFrequencyRecord =
    { days : Int, times : Int }


type alias SpecificDayOfWeekFrequencyRecord =
    { monday : Int
    , tuesday : Int
    , wednesday : Int
    , thursday : Int
    , friday : Int
    , saturday : Int
    , sunday : Int
    }


type HabitTime
    = Morning
    | Evening
    | Anytime


initAddHabitData : AddHabitInputData
initAddHabitData =
    { openView = False
    , kind = GoodHabitKind
    , name = ""
    , description = ""
    , goodHabitTime = Anytime
    , unitNameSingular = ""
    , unitNamePlural = ""
    , frequencyKind = TotalWeekFrequencyKind
    , timesPerWeek = Nothing
    , mondayTimes = Nothing
    , tuesdayTimes = Nothing
    , wednesdayTimes = Nothing
    , thursdayTimes = Nothing
    , fridayTimes = Nothing
    , saturdayTimes = Nothing
    , sundayTimes = Nothing
    , times = Nothing
    , days = Nothing
    }


{-| Returns the habits split by good/bad: (good habits, bad habits).
-}
splitHabits : List Habit -> ( List Habit, List Habit )
splitHabits habits =
    let
        goodHabits =
            List.filter
                (\habit ->
                    case habit of
                        GoodHabit goodHabitRecord ->
                            True

                        _ ->
                            False
                )
                habits

        badHabits =
            List.filter
                (\habit ->
                    case habit of
                        BadHabit badHabitRecord ->
                            True

                        _ ->
                            False
                )
                habits
    in
    ( goodHabits, badHabits )


{-| Retrieve fields that exist on both good and bad habits.
-}
getCommonFields :
    Habit
    ->
        { id : String
        , name : String
        , description : Maybe String
        , unitNameSingular : String
        , unitNamePlural : String
        , suspensions : List SuspendedInterval
        }
getCommonFields habit =
    case habit of
        GoodHabit { id, name, description, unitNameSingular, unitNamePlural, suspensions } ->
            { id = id
            , name = name
            , description = description
            , unitNameSingular = unitNameSingular
            , unitNamePlural = unitNamePlural
            , suspensions = suspensions
            }

        BadHabit { id, name, description, unitNameSingular, unitNamePlural, suspensions } ->
            { id = id
            , name = name
            , description = description
            , unitNameSingular = unitNameSingular
            , unitNamePlural = unitNamePlural
            , suspensions = suspensions
            }


getCommonCreateFields :
    CreateHabit
    ->
        { name : String
        , description : String
        , unitNameSingular : String
        , unitNamePlural : String
        , initialFrequency : Frequency
        }
getCommonCreateFields createHabit =
    case createHabit of
        CreateGoodHabit { name, description, unitNameSingular, unitNamePlural, initialTargetFrequency } ->
            { name = name
            , description = description
            , unitNameSingular = unitNameSingular
            , unitNamePlural = unitNamePlural
            , initialFrequency = initialTargetFrequency
            }

        CreateBadHabit { name, description, unitNameSingular, unitNamePlural, initialThresholdFrequency } ->
            { name = name
            , description = description
            , unitNameSingular = unitNameSingular
            , unitNamePlural = unitNamePlural
            , initialFrequency = initialThresholdFrequency
            }


extractCreateHabit : AddHabitInputData -> Maybe CreateHabit
extractCreateHabit addHabitInputData =
    let
        extractedName =
            Util.notEmpty addHabitInputData.name

        extractedDesc =
            Util.notEmpty addHabitInputData.description

        goodHabitTime =
            addHabitInputData.goodHabitTime

        extractedUnitNameSingular =
            Util.notEmpty addHabitInputData.unitNameSingular

        extractedUnitNamePlural =
            Util.notEmpty addHabitInputData.unitNamePlural

        extractedFrequency =
            case addHabitInputData.frequencyKind of
                EveryXDayFrequencyKind ->
                    case ( addHabitInputData.days, addHabitInputData.times ) of
                        ( Just days, Just times ) ->
                            Just <| EveryXDayFrequency { times = times, days = days }

                        _ ->
                            Nothing

                TotalWeekFrequencyKind ->
                    Maybe.map TotalWeekFrequency addHabitInputData.timesPerWeek

                SpecificDayOfWeekFrequencyKind ->
                    case
                        [ addHabitInputData.mondayTimes
                        , addHabitInputData.tuesdayTimes
                        , addHabitInputData.wednesdayTimes
                        , addHabitInputData.thursdayTimes
                        , addHabitInputData.fridayTimes
                        , addHabitInputData.saturdayTimes
                        , addHabitInputData.sundayTimes
                        ]
                    of
                        [ Just monday, Just tuesday, Just wednesday, Just thursday, Just friday, Just saturday, Just sunday ] ->
                            Just <|
                                SpecificDayOfWeekFrequency
                                    { monday = monday
                                    , tuesday = tuesday
                                    , wednesday = wednesday
                                    , thursday = thursday
                                    , friday = friday
                                    , saturday = saturday
                                    , sunday = sunday
                                    }

                        _ ->
                            Nothing
    in
    case addHabitInputData.kind of
        GoodHabitKind ->
            case
                ( ( extractedName, extractedDesc )
                , ( extractedUnitNameSingular, extractedUnitNamePlural )
                , extractedFrequency
                )
            of
                ( ( Just name, Just description ), ( Just unitNameSingular, Just unitNamePlural ), Just frequency ) ->
                    Just <| CreateGoodHabit <| CreateGoodHabitRecord name description goodHabitTime unitNameSingular unitNamePlural frequency

                _ ->
                    Nothing

        BadHabitKind ->
            case
                ( ( extractedName, extractedDesc )
                , ( extractedUnitNameSingular, extractedUnitNamePlural )
                , extractedFrequency
                )
            of
                ( ( Just name, Just description ), ( Just unitNameSingular, Just unitNamePlural ), Just frequency ) ->
                    Just <| CreateBadHabit <| CreateBadHabitRecord name description unitNameSingular unitNamePlural frequency

                _ ->
                    Nothing


extractNewGoal : EditGoalInputData -> Maybe Frequency
extractNewGoal editGoal =
    case editGoal.frequencyKind of
        EveryXDayFrequencyKind ->
            case ( editGoal.days, editGoal.times ) of
                ( Just days, Just times ) ->
                    Just <| EveryXDayFrequency { days = days, times = times }

                _ ->
                    Nothing

        SpecificDayOfWeekFrequencyKind ->
            case
                [ editGoal.mondayTimes
                , editGoal.tuesdayTimes
                , editGoal.wednesdayTimes
                , editGoal.thursdayTimes
                , editGoal.fridayTimes
                , editGoal.saturdayTimes
                , editGoal.sundayTimes
                ]
            of
                [ Just mo, Just tu, Just we, Just th, Just fr, Just sa, Just su ] ->
                    Just <|
                        SpecificDayOfWeekFrequency
                            { monday = mo
                            , tuesday = tu
                            , wednesday = we
                            , thursday = th
                            , friday = fr
                            , saturday = sa
                            , sunday = su
                            }

                _ ->
                    Nothing

        TotalWeekFrequencyKind ->
            case editGoal.timesPerWeek of
                Just timesPerWeek ->
                    Just <| TotalWeekFrequency timesPerWeek

                _ ->
                    Nothing


prettyPrintEveryXDayFrequency : EveryXDayFrequencyRecord -> String -> String -> String
prettyPrintEveryXDayFrequency { days, times } unitNameSingular unitNamePlural =
    String.fromInt times
        ++ " "
        ++ (if times == 1 then
                unitNameSingular

            else
                unitNamePlural
           )
        ++ " per "
        ++ (if days == 1 then
                "day"

            else
                String.fromInt days ++ " days"
           )


prettyPrintTotalWeekFrequency : Int -> String -> String -> String
prettyPrintTotalWeekFrequency timesPerWeek unitNameSingular unitNamePlural =
    String.fromInt timesPerWeek
        ++ " "
        ++ (if timesPerWeek == 1 then
                unitNameSingular

            else
                unitNamePlural
           )
        ++ " per week"


prettyPrintSpecificDayOfWeekFrequency : SpecificDayOfWeekFrequencyRecord -> String
prettyPrintSpecificDayOfWeekFrequency { monday, tuesday, wednesday, thursday, friday, saturday, sunday } =
    "Mo "
        ++ String.fromInt monday
        ++ " Tu "
        ++ String.fromInt tuesday
        ++ " We "
        ++ String.fromInt wednesday
        ++ " Th "
        ++ String.fromInt thursday
        ++ " Fr "
        ++ String.fromInt friday
        ++ " Sa "
        ++ String.fromInt saturday
        ++ " Su "
        ++ String.fromInt sunday


prettyPrintFrequency : Frequency -> String -> String -> String
prettyPrintFrequency frequency unitNameSingular unitNamePlural =
    case frequency of
        EveryXDayFrequency everyXDayFrequencyRecord ->
            prettyPrintEveryXDayFrequency everyXDayFrequencyRecord unitNameSingular unitNamePlural

        TotalWeekFrequency int ->
            prettyPrintTotalWeekFrequency int unitNameSingular unitNamePlural

        SpecificDayOfWeekFrequency specificDayOfWeekFrequencyRecord ->
            prettyPrintSpecificDayOfWeekFrequency specificDayOfWeekFrequencyRecord


graphQLOutputString : String
graphQLOutputString =
    """{
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
        suspensions {
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
        }
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
        suspensions {
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
        }
      }
    }"""


decodeHabit : Decode.Decoder Habit
decodeHabit =
    let
        decodeGoodHabitRecord =
            Decode.succeed GoodHabitRecord
                |> required "_id" Decode.string
                |> required "name" Decode.string
                |> optional "description" (Decode.maybe Decode.string) Nothing
                |> required "unit_name_singular" Decode.string
                |> required "unit_name_plural" Decode.string
                |> required "target_frequencies" (Decode.list decodeFrequencyChangeRecord)
                |> required "time_of_day" decodeHabitTime
                |> required "suspensions" (Decode.list decodeSuspendedInterval)

        decodeBadHabitRecord =
            Decode.succeed BadHabitRecord
                |> required "_id" Decode.string
                |> required "name" Decode.string
                |> optional "description" (Decode.maybe Decode.string) Nothing
                |> required "unit_name_singular" Decode.string
                |> required "unit_name_plural" Decode.string
                |> required "threshold_frequencies" (Decode.list decodeFrequencyChangeRecord)
                |> required "suspensions" (Decode.list decodeSuspendedInterval)
    in
    Decode.at [ "__typename" ] Decode.string
        |> Decode.andThen
            (\typeName ->
                case typeName of
                    "good_habit" ->
                        decodeGoodHabitRecord |> Decode.map GoodHabit

                    "bad_habit" ->
                        decodeBadHabitRecord |> Decode.map BadHabit

                    _ ->
                        Decode.fail <| "Unable to decode habit, invalid __typename: " ++ typeName
            )


decodeSuspendedInterval : Decode.Decoder SuspendedInterval
decodeSuspendedInterval =
    Decode.succeed SuspendedInterval
        |> required "start_date" decodeYmdDate
        |> optional "end_date" (Decode.maybe decodeYmdDate) Nothing


decodeFrequencyChangeRecord : Decode.Decoder FrequencyChangeRecord
decodeFrequencyChangeRecord =
    Decode.succeed FrequencyChangeRecord
        |> required "start_date" decodeYmdDate
        |> optional "end_date" (Decode.maybe decodeYmdDate) Nothing
        |> required "new_frequency" decodeFrequency


decodeFrequency : Decode.Decoder Frequency
decodeFrequency =
    let
        decodeEveryXDayFrequencyRecord =
            Decode.succeed EveryXDayFrequencyRecord
                |> required "days" Decode.int
                |> required "times" Decode.int

        decodeTotalWeekFrequencyRecord =
            Decode.at [ "week" ] Decode.int

        decodeSpecificDayOfWeekFrequencyRecord =
            Decode.succeed SpecificDayOfWeekFrequencyRecord
                |> optional "monday" Decode.int 0
                |> optional "tuesday" Decode.int 0
                |> optional "wednesday" Decode.int 0
                |> optional "thursday" Decode.int 0
                |> optional "friday" Decode.int 0
                |> optional "saturday" Decode.int 0
                |> optional "sunday" Decode.int 0
    in
    Decode.at [ "__typename" ] Decode.string
        |> Decode.andThen
            (\typeName ->
                case typeName of
                    "specific_day_of_week_frequency" ->
                        decodeSpecificDayOfWeekFrequencyRecord |> Decode.map SpecificDayOfWeekFrequency

                    "total_week_frequency" ->
                        decodeTotalWeekFrequencyRecord |> Decode.map TotalWeekFrequency

                    "every_x_days_frequency" ->
                        decodeEveryXDayFrequencyRecord |> Decode.map EveryXDayFrequency

                    _ ->
                        Decode.fail <| "Unable to decode frequency, invalid __typename: " ++ typeName
            )


decodeHabitTime : Decode.Decoder HabitTime
decodeHabitTime =
    let
        fromStringDecoder str =
            case str of
                "ANYTIME" ->
                    Decode.succeed Anytime

                "EVENING" ->
                    Decode.succeed Evening

                "MORNING" ->
                    Decode.succeed Morning

                _ ->
                    Decode.fail <| str ++ " is not a valid habit time."
    in
    Decode.string |> Decode.andThen fromStringDecoder
