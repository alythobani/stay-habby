module Models.DialogScreen exposing (DialogScreen(..))

{-| Module related to fullscreen dialogs
-}


type DialogScreen
    = AddNewHabitScreen
    | EditGoalHabitSelectionScreen
    | EditGoalScreen
    | EditInfoHabitSelectionScreen
    | EditInfoScreen
    | SetHabitDataShortcutHabitSelectionScreen
    | SetHabitDataShortcutAmountScreen
    | ErrorMessageScreen
    | AddNoteHabitSelectionScreen
    | AddNoteScreen
    | ChooseDateDialogScreen
    | SuspendOrResumeHabitSelectionScreen
    | SuspendOrResumeConfirmationScreen
    | GraphHabitSelectionScreen
    | GraphDialogScreen
