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
import Models.Login as Login
import Models.User as User
import Models.YmdDate as YmdDate
import Time
import TimeZone
import Url


type Msg
    = NoOp
    | OnInitialTimeZoneRetrieval (Result TimeZone.Error ( String, Time.Zone ))
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
      -- Authentication
    | OnClickChooseLoginFormButton
    | OnClickChooseCreateUserFormButton
    | OnLoginFormUsernameInput String
    | OnLoginFormPasswordInput String
    | OnLoginUserClick
    | OnLoginUserGraphqlFailure ApiError
    | OnLoginUserGraphqlSuccess Api.QueriedUser
    | OnCreateUserFormDisplayNameInput String
    | OnCreateUserFormEmailAddressInput String
    | OnCreateUserFormUsernameInput String
    | OnCreateUserFormPasswordInput String
    | OnCreateUserFormRepeatPasswordInput String
    | OnSignUpUserClick Login.CreateUserFields
    | OnSignUpUserGraphqlFailure ApiError
    | OnSignUpUserGraphqlSuccess (Maybe User.User)
      -- Time / Date
    | TickMinute Time.Posix
    | OnTimeZoneRetrieval (Result TimeZone.Error ( String, Time.Zone ))
      -- Top Panel Date
    | ToggleTopPanelDateDropdown
    | ChangeSelectedYmd YmdDate.YmdDate
    | SetSelectedDateToXDaysFromToday Int
    | OpenChooseCustomDateDialog
    | OnChooseDateDialogPreviousMonthClick YmdDate.YmdDate
    | OnChooseDateDialogNextMonthClick YmdDate.YmdDate
    | OnChooseDateDialogPreviousDayClick YmdDate.YmdDate
    | OnChooseDateDialogNextDayClick YmdDate.YmdDate
    | OnChooseDateDialogPreviousYearClick YmdDate.YmdDate
    | OnChooseDateDialogNextYearClick YmdDate.YmdDate
    | SetChooseDateDialogChosenYmd YmdDate.YmdDate
    | SetChooseDateDialogChosenYmdToToday
    | OnChooseDateDialogArrowDown
    | OnChooseDateDialogArrowUp
    | OnChooseDateDialogArrowLeft
    | OnChooseDateDialogArrowRight
    | OnChooseDateDialogSubmitClick
      -- Top Panel User Actions Dropdown
    | ToggleTopPanelUserActionsDropdown
    | OnLogoutUserClick
      -- All Habit Data
    | OnGetAllRemoteDataFailure ApiError
    | OnGetAllRemoteDataSuccess Api.AllRemoteData
      -- Add Habit
    | OpenAddHabitForm
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
    | AddHabitFormSubmit
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
      -- Keyboard Shortcuts
    | KeyboardMsg Keyboard.Msg
    | AttemptKeyboardShortcut Keyboard.Key
    | ToggleAvailableKeyboardShortcutsScreen
      -- Dom
    | FocusResult (Result Dom.Error ())
      -- Set Habit Data Shortcut
    | OpenSetHabitDataShortcutHabitSelectionScreen
    | OnSetHabitDataShortcutHabitSelectionFilterTextInput String
    | OnSetHabitDataShortcutSelectNextHabit
    | OnSetHabitDataShortcutSelectPreviousHabit
    | OpenSetHabitDataShortcutAmountScreen
    | OnSetHabitDataShortcutAmountScreenInput String
    | OnSetHabitDataShortcutAmountScreenSubmit
      -- Edit Goal Habit Selection
    | OpenEditGoalHabitSelectionScreen
    | OnEditGoalHabitSelectionFilterTextInput String
    | OnEditGoalHabitSelectionSelectNextHabit
    | OnEditGoalHabitSelectionSelectPreviousHabit
    | OnEditGoalHabitSelectionEnterKeydown
      -- Edit Goal
    | OpenEditGoalScreen Habit.Habit
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
    | OnAddNoteHabitSelectionEnterKeydown
      -- Add Note Dialog
    | OpenAddNoteDialog Habit.Habit
    | OnAddNoteDialogInput String
    | OnAddNoteSubmit
    | OnAddNoteFailure ApiError
    | OnAddNoteSuccess HabitDayNote.HabitDayNote
      -- Suspend Or Resume Habit Selection Screen
    | OpenSuspendOrResumeHabitSelectionScreen
    | OnSuspendOrResumeHabitSelectionFilterTextInput String
    | OnSuspendOrResumeHabitSelectionSelectNextHabit
    | OnSuspendOrResumeHabitSelectionSelectPreviousHabit
    | OnSuspendOrResumeHabitSelectionEnterKeydown
      -- Suspend Or Resume Confirmation Screen
    | OpenSuspendOrResumeConfirmationScreen Habit.Habit
    | OnResumeOrSuspendSubmitClick
    | OnResumeOrSuspendHabitFailure ApiError
    | OnResumeOrSuspendHabitSuccess Habit.Habit
      -- Graph Habit Selection Screen
    | OpenGraphHabitSelectionScreen
    | OnGraphHabitSelectionFilterTextInput String
    | OnGraphHabitSelectionSelectNextHabit
    | OnGraphHabitSelectionSelectPreviousHabit
    | OnGraphHabitSelectionEnterKeydown
      -- Graph Dialog Screen
    | OpenGraphDialogScreen Habit.Habit
    | SetGraphNumDaysToShow Graph.NumberOfDaysToShow
    | OnGetGraphHabitGoalIntervalListFailure ApiError
    | OnGetGraphHabitGoalIntervalListSuccess Api.QueriedHabitGoalIntervalLists
    | OnGraphPointHover (Maybe Graph.Point)
