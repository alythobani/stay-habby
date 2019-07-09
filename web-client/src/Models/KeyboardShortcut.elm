module Models.KeyboardShortcut exposing
    ( KeyboardShortcut
    , addNewHabitScreenShortcuts
    , addNoteHabitSelectionShortcuts
    , addNoteScreenShortcuts
    , chooseDateScreenShortcuts
    , editGoalHabitSelectionShortcuts
    , editGoalScreenShortcuts
    , errorMessageScreenShortcuts
    , graphHabitSelectionShortcuts
    , graphScreenShortcuts
    , mainScreenShortcuts
    , setHabitDataAmountScreenShortcuts
    , setHabitDataHabitSelectionShortcuts
    , suspendOrResumeConfirmationScreenShortcuts
    , suspendOrResumeHabitSelectionShortcuts
    )

import DefaultServices.Keyboard as Keyboard
import Models.Graph as Graph
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (..)


type alias KeyboardShortcut =
    { keys : List Keyboard.Key
    , msg : Msg
    , keysStr : String
    , description : String
    }


singleKeyShortcut : Keyboard.Key -> Msg -> String -> KeyboardShortcut
singleKeyShortcut key msg desc =
    { keys = [ key ], msg = msg, keysStr = Keyboard.prettyPrintKey key, description = desc }


multiKeyShortcut : List Keyboard.Key -> Msg -> String -> KeyboardShortcut
multiKeyShortcut keys msg desc =
    { keys = keys
    , msg = msg
    , keysStr = String.join " + " (List.map Keyboard.prettyPrintKey keys)
    , description = desc
    }


toggleAvailableKeyboardShortcutsScreenShortcut : KeyboardShortcut
toggleAvailableKeyboardShortcutsScreenShortcut =
    singleKeyShortcut Keyboard.Slash ToggleAvailableKeyboardShortcutsScreen "Toggle Shortcuts Screen"


toggleAvailableKeyboardShortcutsScreenMultiKeyShortcut : KeyboardShortcut
toggleAvailableKeyboardShortcutsScreenMultiKeyShortcut =
    multiKeyShortcut [ Keyboard.MetaLeft, Keyboard.Slash ] ToggleAvailableKeyboardShortcutsScreen "Toggle Shortcuts Screen"


closeFormShortcut : KeyboardShortcut
closeFormShortcut =
    singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Close Form"


cancelScreenShortcut : KeyboardShortcut
cancelScreenShortcut =
    singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Cancel"


darkModeShortcut : KeyboardShortcut
darkModeShortcut =
    singleKeyShortcut Keyboard.KeyD OnToggleDarkMode "Toggle Dark Mode"


mainScreenShortcuts : List KeyboardShortcut
mainScreenShortcuts =
    [ singleKeyShortcut Keyboard.KeyA OpenSetHabitDataShortcutHabitSelectionScreen "Set Habit Amount"
    , singleKeyShortcut Keyboard.KeyC OpenChooseCustomDateDialog "Change Date"
    , darkModeShortcut
    , singleKeyShortcut Keyboard.KeyE OpenEditGoalHabitSelectionScreen "Edit Goal"
    , singleKeyShortcut Keyboard.KeyG OpenGraphHabitSelectionScreen "View Graph"
    , singleKeyShortcut Keyboard.KeyH OpenAddHabitForm "Add New Habit"
    , singleKeyShortcut Keyboard.KeyN OpenAddNoteHabitSelectionDialogScreen "Add Note"
    , singleKeyShortcut Keyboard.KeyS OpenSuspendOrResumeHabitSelectionScreen "Suspend or Resume"
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


errorMessageScreenShortcuts : List KeyboardShortcut
errorMessageScreenShortcuts =
    [ singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Close Screen"
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


addNewHabitScreenShortcuts : List KeyboardShortcut
addNewHabitScreenShortcuts =
    [ multiKeyShortcut [ Keyboard.MetaLeft, Keyboard.Enter ] AddHabitFormSubmit "Submit Form"
    , closeFormShortcut
    , toggleAvailableKeyboardShortcutsScreenMultiKeyShortcut
    ]


chooseDateScreenShortcuts : List KeyboardShortcut
chooseDateScreenShortcuts =
    [ singleKeyShortcut Keyboard.KeyT SetChooseDateDialogChosenYmdToToday "Go To Today"
    , singleKeyShortcut Keyboard.ArrowDown OnChooseDateDialogArrowDown "Move Down"
    , singleKeyShortcut Keyboard.ArrowUp OnChooseDateDialogArrowUp "Move Up"
    , singleKeyShortcut Keyboard.ArrowLeft OnChooseDateDialogArrowLeft "Move Left"
    , singleKeyShortcut Keyboard.ArrowRight OnChooseDateDialogArrowRight "Move Right"
    , singleKeyShortcut Keyboard.Enter OnChooseDateDialogSubmitClick "Submit"
    , cancelScreenShortcut
    , darkModeShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


habitSelectionShortcuts : Msg -> Msg -> Msg -> List KeyboardShortcut
habitSelectionShortcuts onArrowUp onArrowDown onChooseHabit =
    [ singleKeyShortcut Keyboard.ArrowDown onArrowDown "Select Next Habit"
    , singleKeyShortcut Keyboard.ArrowUp onArrowUp "Select Previous Habit"
    , singleKeyShortcut Keyboard.Enter onChooseHabit "Confirm Selected Habit"
    , cancelScreenShortcut
    , toggleAvailableKeyboardShortcutsScreenMultiKeyShortcut
    ]


editGoalHabitSelectionShortcuts : List KeyboardShortcut
editGoalHabitSelectionShortcuts =
    habitSelectionShortcuts
        OnEditGoalHabitSelectionSelectPreviousHabit
        OnEditGoalHabitSelectionSelectNextHabit
        OnEditGoalHabitSelectionEnterKeydown


editGoalScreenShortcuts : List KeyboardShortcut
editGoalScreenShortcuts =
    [ singleKeyShortcut Keyboard.Enter OnEditGoalSubmit "Submit Form"
    , closeFormShortcut
    , singleKeyShortcut
        Keyboard.KeyX
        (OnEditGoalSelectFrequencyKind Habit.TotalWeekFrequencyKind)
        "New Goal: X Per Week"
    , singleKeyShortcut
        Keyboard.KeyS
        (OnEditGoalSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind)
        "New Goal: Specific Days Of Week"
    , singleKeyShortcut
        Keyboard.KeyY
        (OnEditGoalSelectFrequencyKind Habit.EveryXDayFrequencyKind)
        "New Goal: Y Per X Days"
    , darkModeShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


setHabitDataHabitSelectionShortcuts : List KeyboardShortcut
setHabitDataHabitSelectionShortcuts =
    habitSelectionShortcuts
        OnSetHabitDataShortcutSelectPreviousHabit
        OnSetHabitDataShortcutSelectNextHabit
        OpenSetHabitDataShortcutAmountScreen


setHabitDataAmountScreenShortcuts : List KeyboardShortcut
setHabitDataAmountScreenShortcuts =
    [ singleKeyShortcut Keyboard.Enter OnSetHabitDataShortcutAmountScreenSubmit "Submit Amount"
    , cancelScreenShortcut
    , darkModeShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


addNoteHabitSelectionShortcuts : List KeyboardShortcut
addNoteHabitSelectionShortcuts =
    habitSelectionShortcuts
        OnAddNoteHabitSelectionScreenSelectPreviousHabit
        OnAddNoteHabitSelectionScreenSelectNextHabit
        OnAddNoteHabitSelectionEnterKeydown


addNoteScreenShortcuts : List KeyboardShortcut
addNoteScreenShortcuts =
    [ multiKeyShortcut [ Keyboard.MetaLeft, Keyboard.Enter ] OnAddNoteSubmit "Submit Note"
    , closeFormShortcut
    , toggleAvailableKeyboardShortcutsScreenMultiKeyShortcut
    ]


suspendOrResumeHabitSelectionShortcuts : List KeyboardShortcut
suspendOrResumeHabitSelectionShortcuts =
    habitSelectionShortcuts
        OnSuspendOrResumeHabitSelectionSelectPreviousHabit
        OnSuspendOrResumeHabitSelectionSelectNextHabit
        OnSuspendOrResumeHabitSelectionEnterKeydown


suspendOrResumeConfirmationScreenShortcuts : List KeyboardShortcut
suspendOrResumeConfirmationScreenShortcuts =
    [ singleKeyShortcut Keyboard.Enter OnResumeOrSuspendSubmitClick "Confirm"
    , cancelScreenShortcut
    , darkModeShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


graphHabitSelectionShortcuts : List KeyboardShortcut
graphHabitSelectionShortcuts =
    habitSelectionShortcuts
        OnGraphHabitSelectionSelectPreviousHabit
        OnGraphHabitSelectionSelectNextHabit
        OnGraphHabitSelectionEnterKeydown


graphScreenShortcuts : List KeyboardShortcut
graphScreenShortcuts =
    [ singleKeyShortcut
        Keyboard.KeyM
        (SetGraphNumDaysToShow Graph.LastMonth)
        "Last Month"
    , singleKeyShortcut
        Keyboard.KeyT
        (SetGraphNumDaysToShow Graph.LastThreeMonths)
        "Last Three Months"
    , singleKeyShortcut
        Keyboard.KeyY
        (SetGraphNumDaysToShow Graph.LastYear)
        "Last Year"
    , singleKeyShortcut
        Keyboard.KeyA
        (SetGraphNumDaysToShow Graph.AllTime)
        "All Time"
    , singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Close Graph"
    , darkModeShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]
