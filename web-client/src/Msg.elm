module Msg exposing (Msg(..))

import Api
import Array
import Browser
import Browser.Dom as Dom
import DefaultServices.Keyboard as Keyboard
import Models.ApiError exposing (ApiError)
import Models.Graph as Graph
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.HabitDayNote as HabitDayNote
import Models.YmdDate as YmdDate
import Time
import TimeZone
import Url


type Msg
    = NoOp
    | OnInitialTimeZoneRetrieval (Result TimeZone.Error ( String, Time.Zone ))
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
      -- Time / Date
    | TickMinute Time.Posix
    | OnTimeZoneRetrieval (Result TimeZone.Error ( String, Time.Zone ))
      -- Top Panel Date
    | ToggleTopPanelDateDropdown
    | ChangeSelectedYmd YmdDate.YmdDate
    | SetSelectedDateToXDaysFromToday Int
    | OpenChooseCustomDateDialog
    | OnChooseDateDialogScreenKeydown Keyboard.Key
    | OnChooseDateDialogPreviousMonthClick YmdDate.YmdDate
    | OnChooseDateDialogNextMonthClick YmdDate.YmdDate
    | OnChooseDateDialogPreviousDayClick YmdDate.YmdDate
    | OnChooseDateDialogNextDayClick YmdDate.YmdDate
    | OnChooseDateDialogPreviousYearClick YmdDate.YmdDate
    | OnChooseDateDialogNextYearClick YmdDate.YmdDate
    | SetChooseDateDialogChosenYmd YmdDate.YmdDate
    | OnChooseDateDialogSubmitClick YmdDate.YmdDate
      -- All Habit Data
    | OnGetAllRemoteDataFailure ApiError
    | OnGetAllRemoteDataSuccess Api.AllRemoteData
      -- Add Habit
    | OpenAddHabitForm
    | OnAddHabitFormKeydown Keyboard.Key
    | OnSelectAddHabitKind Habit.HabitKind
    | OnAddHabitNameInput String
    | OnAddHabitDescriptionInput String
    | OnSelectAddGoodHabitTime Habit.HabitTime
    | OnAddHabitUnitNameSingularInput String
    | OnAddHabitUnitNamePluralInput String
    | OnAddHabitSelectFrequencyKind Habit.FrequencyKind
    | OnAddHabitTimesPerWeekInput String
    | OnAddHabitSpecificDayMondayInput String
    | OnAddHabitSpecificDayTuesdayInput String
    | OnAddHabitSpecificDayWednesdayInput String
    | OnAddHabitSpecificDayThursdayInput String
    | OnAddHabitSpecificDayFridayInput String
    | OnAddHabitSpecificDaySaturdayInput String
    | OnAddHabitSpecificDaySundayInput String
    | OnAddHabitTimesInput String
    | OnAddHabitDaysInput String
    | OnAddHabitSubmit Habit.CreateHabit
    | OnAddHabitFailure ApiError
    | OnAddHabitSuccess Habit.Habit
      -- Set Habit Data
    | OnHabitAmountInput String String
    | SetHabitData YmdDate.YmdDate String (Maybe Int)
    | OnSetHabitDataFailure ApiError
    | OnSetHabitDataSuccess HabitData.HabitData
      -- Frequency Statistics
    | OnGetFrequencyStatsFailure ApiError
    | OnGetFrequencyStatsSuccess Api.QueriedFrequencyStats
      -- Habit Actions Dropdowns
    | ToggleHabitActionsDropdown String
      -- Dark Mode
    | OnToggleDarkMode
      -- Keyboard
    | KeyboardMsg Keyboard.Msg
    | OnMainScreenKeydown Keyboard.Key
      -- Dom
    | FocusResult (Result Dom.Error ())
      -- Set Habit Data Shortcut
    | OpenSetHabitDataShortcutHabitSelectionScreen
    | OnSetHabitDataShortcutHabitSelectionFilterTextInput String
    | OnSetHabitDataShortcutSelectNextHabit
    | OnSetHabitDataShortcutSelectPreviousHabit
    | OpenSetHabitDataShortcutAmountScreen Habit.Habit
    | OnSetHabitDataShortcutAmountScreenInput String
    | OnSetHabitDataShortcutAmountScreenSubmit YmdDate.YmdDate String Int
      -- Edit Goal Habit Selection
    | OpenEditGoalHabitSelectionScreen
    | OnEditGoalHabitSelectionFilterTextInput String
    | OnEditGoalHabitSelectionSelectNextHabit
    | OnEditGoalHabitSelectionSelectPreviousHabit
      -- Edit Goal
    | OpenEditGoalScreen Habit.Habit
    | OnEditGoalScreenKeydown Keyboard.Key
    | OnEditGoalSelectFrequencyKind Habit.FrequencyKind
    | OnEditGoalTimesPerWeekInput String
    | OnEditGoalSpecificDayMondayInput String
    | OnEditGoalSpecificDayTuesdayInput String
    | OnEditGoalSpecificDayWednesdayInput String
    | OnEditGoalSpecificDayThursdayInput String
    | OnEditGoalSpecificDayFridayInput String
    | OnEditGoalSpecificDaySaturdayInput String
    | OnEditGoalSpecificDaySundayInput String
    | OnEditGoalTimesInput String
    | OnEditGoalDaysInput String
    | RefreshEditGoalConfirmationMessageAndNewSuspensions
    | OnEditGoalFailure ApiError
    | OnEditGoalSuccess Habit.Habit
    | OnEditGoalSubmit
      -- Error messages
    | OpenErrorMessageDialogScreen
      -- Full screen dialogs
    | OnExitDialogScreen
      -- Add Note Habit Selection
    | OpenAddNoteHabitSelectionDialogScreen
    | OnAddNoteHabitSelectionFilterTextInput String
    | OnAddNoteHabitSelectionScreenSelectNextHabit
    | OnAddNoteHabitSelectionScreenSelectPreviousHabit
      -- Add Note Dialog
    | OpenAddNoteDialog Habit.Habit
    | OnAddNoteKeydown Keyboard.Key
    | OnAddNoteDialogInput String
    | OnAddNoteSubmit
    | OnAddNoteFailure ApiError
    | OnAddNoteSuccess HabitDayNote.HabitDayNote
      -- Suspend Or Resume Habit Selection Screen
    | OpenSuspendOrResumeHabitSelectionScreen
    | OnSuspendOrResumeHabitSelectionFilterTextInput String
    | OnSuspendOrResumeHabitSelectionSelectNextHabit
    | OnSuspendOrResumeHabitSelectionSelectPreviousHabit
      -- Suspend Or Resume Confirmation Screen
    | OpenSuspendOrResumeConfirmationScreen Habit.Habit
    | OnSuspendOrResumeConfirmationScreenKeydown Keyboard.Key
    | OnResumeOrSuspendSubmitClick String (List Habit.SuspendedInterval)
    | OnResumeOrSuspendHabitFailure ApiError
    | OnResumeOrSuspendHabitSuccess Habit.Habit
      -- Graph Habit Selection Screen
    | OpenGraphHabitSelectionScreen
    | OnGraphHabitSelectionFilterTextInput String
    | OnGraphHabitSelectionSelectNextHabit
    | OnGraphHabitSelectionSelectPreviousHabit
      -- Graph Dialog Screen
    | OpenGraphDialogScreen Habit.Habit
    | OnGraphDialogScreenKeydown Keyboard.Key
    | SetGraphNumDaysToShow Graph.NumberOfDaysToShow
    | OnGetGraphHabitGoalIntervalListFailure ApiError
    | OnGetGraphHabitGoalIntervalListSuccess Api.QueriedHabitGoalIntervalLists
    | OnExitGraphScreen
