module Msg exposing (Msg(..))

import Api
import Browser
import Browser.Dom as Dom
import DefaultServices.Keyboard as Keyboard
import Models.ApiError exposing (ApiError)
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.YmdDate as YmdDate
import Time
import Url


type Msg
    = NoOp
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
    | TickMinute Time.Posix
    | OnGetHabitsAndHabitDataAndFrequencyStatsFailure ApiError
    | OnGetHabitsAndHabitDataAndFrequencyStatsSuccess Api.HabitsAndHabitDataAndFrequencyStats
    | OnOpenAddHabit
    | OnCancelAddHabit
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
    | AddHabit Habit.CreateHabit
    | OnAddHabitFailure ApiError
    | OnAddHabitSuccess Habit.Habit
    | OnHabitDataInput String String
    | SetHabitData YmdDate.YmdDate String (Maybe Int)
    | OnSetHabitDataFailure ApiError
    | OnSetHabitDataSuccess HabitData.HabitData
    | OnToggleHistoryViewer
    | OnToggleTodayViewer
    | OnHistoryViewerDateInput String
    | OnHistoryViewerSelectYesterday
    | OnHistoryViewerSelectBeforeYesterday
    | OnHistoryViewerSelectDateInput
    | SetHistoryViewerSelectedDate YmdDate.YmdDate
    | OnGetTodayFrequencyStatsFailure ApiError
    | OnGetTodayFrequencyStatsSuccess Api.QueriedFrequencyStats
    | OnGetPastFrequencyStatsFailure ApiError
    | OnGetPastFrequencyStatsSuccess Api.QueriedFrequencyStats
    | OnHistoryViewerChangeDate
    | OnHistoryViewerHabitDataInput YmdDate.YmdDate String String
    | ToggleTodayViewerHabitActionsDropdown String Bool
    | ToggleHistoryViewerHabitActionsDropdown String Bool
    | OnToggleDarkMode
    | OnToggleShowSetHabitDataShortcut
    | KeyboardExtraMsg Keyboard.Msg
    | FocusResult (Result Dom.Error ())
    | OnSetHabitDataShortcutInput String
    | OnSetHabitDataShortcutSelectNextHabit
    | OnSetHabitDataShortcutSelectPreviousHabit
    | OnToggleShowSetHabitDataShortcutAmountForm
    | OnSetHabitDataShortcutAmountFormInput String
    | OnSetHabitDataShortcutAmountFormSubmit YmdDate.YmdDate String (Maybe Int)
    | OnEditGoalClick String
    | CloseEditGoalDialog
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
    | OnEditGoalFailure ApiError
    | OnEditGoalSuccess Habit.Habit
    | OnEditGoalSubmitClick String (List Habit.FrequencyChangeRecord) String
      -- Suspending Habits
    | OnResumeOrSuspendHabitClick String Bool Bool (List Habit.SuspendedInterval)
    | OnResumeOrSuspendHabitFailure ApiError
    | OnResumeOrSuspendHabitSuccess Habit.Habit
      -- Error messages
    | OnToggleShowErrorMessage
