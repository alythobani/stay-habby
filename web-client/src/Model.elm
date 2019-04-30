module Model exposing (Model)

import Array
import Browser.Navigation as Navigation
import DefaultServices.Keyboard as Keyboard
import Dict
import Models.ApiError as ApiError
import Models.DialogScreen as DialogScreen
import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.HabitDayNote as HabitDayNote
import Models.YmdDate as YmdDate
import RemoteData
import Time
import Url


type alias Model =
    { key : Navigation.Key
    , url : Url.Url
    , currentPosix : Time.Posix
    , currentTimeZone : Maybe Time.Zone
    , ymd : Maybe YmdDate.YmdDate
    , apiBaseUrl : String
    , darkModeOn : Bool
    , editingTodayHabitAmount : Dict.Dict String Int
    , editingHistoryHabitAmount : Dict.Dict String (Dict.Dict String Int)

    -- Remote Data
    , allHabits : RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    , allHabitData : RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    , allFrequencyStats : RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)

    -- Add Habit
    , addHabit :
        { openView : Bool
        , kind : Habit.HabitKind
        , name : String
        , description : String
        , goodHabitTime : Habit.HabitTime
        , unitNameSingular : String
        , unitNamePlural : String
        , frequencyKind : Habit.FrequencyKind
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
    , openTodayViewer : Bool
    , openHistoryViewer : Bool
    , historyViewerDateInput : String
    , historyViewerSelectedDate : Maybe YmdDate.YmdDate
    , historyViewerFrequencyStats : RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    , todayViewerHabitActionsDropdown : Maybe String
    , historyViewerHabitActionsDropdown : Maybe String
    , keysDown : Keyboard.Model

    -- Set Habit Data Shortcut
    , setHabitDataShortcutHabitNameFilterText : String
    , setHabitDataShortcutFilteredHabits : Array.Array Habit.Habit
    , setHabitDataShortcutSelectedHabitIndex : Int
    , showSetHabitDataShortcutAmountForm : Bool
    , setHabitDataShortcutInputtedAmount : Maybe Int

    -- Edit goal
    , editGoalDialogHabit : Maybe Habit.Habit
    , editGoal : Habit.EditGoalInputData

    -- Error messages
    , errorMessage : Maybe String

    -- Full screen dialogs
    , activeDialogScreen : Maybe DialogScreen.DialogScreen

    -- Add note
    , addNoteDialogHabit : Maybe Habit.Habit
    , addNoteDialogInput : String
    }
