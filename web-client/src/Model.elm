module Model exposing (Model)

import Array
import Browser.Navigation as Navigation
import DefaultServices.Keyboard as Keyboard
import Dict
import LineChart
import Models.ApiError as ApiError
import Models.DialogScreen as DialogScreen
import Models.FrequencyStats as FrequencyStats
import Models.Graph as Graph
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.HabitDayNote as HabitDayNote
import Models.HabitGoalIntervalList as HabitGoalIntervalList
import Models.KeyboardShortcut as KeyboardShortcut
import Models.Login as Login
import Models.User as User
import Models.YmdDate as YmdDate
import Msg exposing (Msg)
import RemoteData
import Time
import Url


type alias Model =
    { key : Navigation.Key
    , url : Url.Url

    -- Authentication
    , user : Maybe User.User
    , loginPageFields : Login.LoginPageFields

    -- Time / Date
    , currentPosix : Time.Posix
    , currentTimeZone : Maybe Time.Zone
    , selectedYmd : Maybe YmdDate.YmdDate
    , actualYmd : Maybe YmdDate.YmdDate
    , openTopPanelDateDropdown : Bool
    , chooseDateDialogChosenYmd : Maybe YmdDate.YmdDate

    --
    , apiBaseUrl : String

    -- Top right corner
    , darkModeOn : Bool
    , openUserActionsDropdown : Bool

    -- User-Inputted Habit Amounts
    , editingHabitAmountDict : Dict.Dict String Int

    -- Remote Data
    , allHabits : RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    , allHabitData : RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    , allFrequencyStats : RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    , allHabitDayNotes : RemoteData.RemoteData ApiError.ApiError (List HabitDayNote.HabitDayNote)

    -- Add Habit
    , addHabit : Habit.AddHabitInputData

    -- Dropdowns
    , habitActionsDropdown : Maybe String

    -- Keyboard Shortcuts
    , keysDown : Keyboard.Model
    , keyboardShortcutsList : List KeyboardShortcut.KeyboardShortcut
    , showAvailableKeyboardShortcutsScreen : Bool

    -- Set Habit Data Shortcut Habit Selection
    , setHabitDataShortcutHabitNameFilterText : String
    , setHabitDataShortcutFilteredHabits : Array.Array Habit.Habit
    , setHabitDataShortcutSelectedHabitIndex : Int

    -- Set Habit Data Shortcut Amount Screen
    , setHabitDataShortcutAmountScreenHabit : Maybe Habit.Habit
    , setHabitDataShortcutAmountScreenInputInt : Maybe Int

    -- Edit Goal Habit Selection
    , editGoalHabitSelectionFilterText : String
    , editGoalHabitSelectionFilteredHabits : Array.Array Habit.Habit
    , editGoalHabitSelectionSelectedHabitIndex : Int

    -- Edit Goal
    , editGoalDialogHabit : Maybe Habit.Habit
    , editGoalDialogHabitCurrentFcrWithIndex : Maybe ( Int, Habit.FrequencyChangeRecord )
    , editGoalConfirmationMessage : Maybe String
    , editGoalNewFrequenciesList : Maybe (List Habit.FrequencyChangeRecord)
    , editGoal : Habit.EditGoalInputData

    -- Edit Info Habit Selection
    , editInfoHabitSelectionFilterText : String
    , editInfoHabitSelectionFilteredHabits : Array.Array Habit.Habit
    , editInfoHabitSelectionSelectedHabitIndex : Int

    -- Edit Info
    , editInfoDialogHabit : Maybe Habit.Habit
    , editInfo : Habit.EditInfoInputData

    -- Error messages
    , errorMessage : Maybe String

    -- Full screen dialogs
    , activeDialogScreen : Maybe DialogScreen.DialogScreen

    -- Add note habit selection
    , addNoteHabitSelectionFilterText : String
    , addNoteHabitSelectionFilteredHabits : Array.Array Habit.Habit
    , addNoteHabitSelectionSelectedHabitIndex : Int

    -- Add note
    , addNoteDialogHabit : Maybe Habit.Habit
    , addNoteDialogInput : String

    -- Suspend Or Resume Habit Selection
    , suspendOrResumeHabitSelectionFilterText : String
    , suspendOrResumeHabitSelectionFilteredHabits : Array.Array Habit.Habit
    , suspendOrResumeHabitSelectionSelectedHabitIndex : Int

    -- Suspend Or Resume Confirmation Dialog
    , suspendOrResumeHabit : Maybe Habit.Habit
    , suspendOrResumeHabitConfirmationMessage : String
    , suspendOrResumeHabitNewSuspensions : Maybe (List Habit.SuspendedInterval)

    -- Graph Habit Selection
    , graphHabitSelectionFilterText : String
    , graphHabitSelectionFilteredHabits : Array.Array Habit.Habit
    , graphHabitSelectionSelectedHabitIndex : Int

    -- Graph Dialog
    , graphHabit : Maybe Habit.Habit
    , graphNumDaysToShow : Graph.NumberOfDaysToShow
    , graphGoalIntervals : RemoteData.RemoteData ApiError.ApiError (List HabitGoalIntervalList.HabitGoalInterval)
    , graphIntervalsData : RemoteData.RemoteData ApiError.ApiError (List ( Graph.IntervalSuccessStatus, Graph.GraphData ))
    , graphHoveredPoint : Maybe Graph.Point
    }
