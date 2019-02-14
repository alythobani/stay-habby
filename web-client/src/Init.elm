module Init exposing (init)

import Api
import Array
import Date
import Dict
import Flags exposing (Flags)
import Keyboard.Extra as KK
import Material
import Model exposing (Model)
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import Navigation
import RemoteData


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { apiBaseUrl, currentTime } location =
    let
        ymd =
            currentTime |> Date.fromTime |> YmdDate.fromDate
    in
    ( { currentYmd = ymd
      , ymd = ymd
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
      , todayViewerHabitActionsDropdowns = Dict.empty
      , historyViewerHabitActionsDropdowns = Dict.empty
      , mdl = Material.model
      , showSetHabitDataShortcut = False
      , keysDown = KK.init
      , setHabitDataShortcutHabitNameFilterText = ""
      , setHabitDataShortcutFilteredHabits = Array.empty
      , setHabitDataShortcutSelectedHabitIndex = 0
      , showSetHabitDataShortcutAmountForm = False
      , setHabitDataShortcutInputtedAmount = Nothing
      , showEditGoalDialog = False
      , editGoalDialogHabit = Nothing
      , editGoal = Habit.initEditGoalData
      , errorMessage = Just "Test error"
      , showErrorMessage = True -- TODO: temporary, this should be default False
      }
    , Api.queryHabitsAndHabitDataAndFrequencyStats
        ymd
        apiBaseUrl
        OnGetHabitsAndHabitDataAndFrequencyStatsFailure
        OnGetHabitsAndHabitDataAndFrequencyStatsSuccess
    )
