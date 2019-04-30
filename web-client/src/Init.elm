module Init exposing (init)

import Api
import Array
import Browser.Navigation as Navigation
import DefaultServices.Keyboard as Keyboard
import Dict
import Flags exposing (Flags)
import Model exposing (Model)
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData
import Task
import Time
import TimeZone
import Url


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { apiBaseUrl, currentTime } url key =
    let
        currentPosix : Time.Posix
        currentPosix =
            currentTime |> Time.millisToPosix
    in
    ( { key = key
      , url = url
      , currentPosix = currentPosix
      , currentTimeZone = Nothing
      , ymd = Nothing
      , apiBaseUrl = apiBaseUrl
      , darkModeOn = True
      , editingTodayHabitAmount = Dict.empty
      , editingHistoryHabitAmount = Dict.empty
      , allHabitData = RemoteData.Loading
      , allHabits = RemoteData.Loading
      , allFrequencyStats = RemoteData.Loading
      , addHabit = Habit.initAddHabitData
      , openTodayViewer = True
      , openHistoryViewer = False
      , historyViewerDateInput = ""
      , historyViewerSelectedDate = Nothing
      , historyViewerFrequencyStats = RemoteData.NotAsked
      , todayViewerHabitActionsDropdown = Nothing
      , historyViewerHabitActionsDropdown = Nothing
      , keysDown = Keyboard.init

      -- Set Habit Data Shortcut
      , setHabitDataShortcutHabitNameFilterText = ""
      , setHabitDataShortcutFilteredHabits = Array.empty
      , setHabitDataShortcutSelectedHabitIndex = 0
      , showSetHabitDataShortcutAmountForm = False
      , setHabitDataShortcutInputtedAmount = Nothing

      -- Edit goal
      , editGoalDialogHabit = Nothing
      , editGoal = Habit.initEditGoalData

      -- Error messages
      , errorMessage = Nothing

      -- Full screen dialogs
      , activeDialogScreen = Nothing
      }
    , Task.attempt OnTimeZoneRetrieval TimeZone.getZone
    )
