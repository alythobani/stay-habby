module Msg exposing (Msg(..))

import Api
import Dom
import Dropdown
import Keyboard.Extra as KK
import Material
import Models.ApiError exposing (ApiError)
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.YmdDate as YmdDate
import Navigation
import Time


type Msg
    = NoOp
    | OnLocationChange Navigation.Location
    | TickMinute Time.Time
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
    | ToggleTodayViewerHabitActionsDropdown String Dropdown.State
    | ToggleHistoryViewerHabitActionsDropdown String Dropdown.State
    | OnToggleDarkMode
    | Mdl (Material.Msg Msg)
    | OnToggleShowSetHabitDataShortcut
    | KeyboardExtraMsg KK.Msg
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
