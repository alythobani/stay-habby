module Models.KeyboardShortcut exposing
    ( KeyboardShortcut
    , addNoteScreenShortcuts
    , editGoalScreenShortcuts
    , mainScreenShortcuts
    , suspendOrResumeConfirmationScreenShortcuts
    )

import DefaultServices.Keyboard as Keyboard
import Models.Habit as Habit
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
    singleKeyShortcut Keyboard.Slash ToggleAvailableKeyboardShortcutsScreen "Toggle Keyboard Shortcuts Screen"


closeFormShortcut : KeyboardShortcut
closeFormShortcut =
    singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Close Form"


mainScreenShortcuts : List KeyboardShortcut
mainScreenShortcuts =
    [ singleKeyShortcut Keyboard.KeyA OpenSetHabitDataShortcutHabitSelectionScreen "Set Habit Amount"
    , singleKeyShortcut Keyboard.KeyC OpenChooseCustomDateDialog "Choose Calendar Date"
    , singleKeyShortcut Keyboard.KeyD OnToggleDarkMode "Toggle Dark Mode"
    , singleKeyShortcut Keyboard.KeyE OpenEditGoalHabitSelectionScreen "Edit Habit Goal"
    , singleKeyShortcut Keyboard.KeyG OpenGraphHabitSelectionScreen "View Habit Graph"
    , singleKeyShortcut Keyboard.KeyH OpenAddHabitForm "Add New Habit"
    , singleKeyShortcut Keyboard.KeyN OpenAddNoteHabitSelectionDialogScreen "Add Note"
    , singleKeyShortcut Keyboard.KeyS OpenSuspendOrResumeHabitSelectionScreen "Suspend or Resume"
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


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
    , singleKeyShortcut Keyboard.Slash ToggleAvailableKeyboardShortcutsScreen "Toggle Keyboard Shortcuts Screen"
    ]


addNoteScreenShortcuts : List KeyboardShortcut
addNoteScreenShortcuts =
    [ multiKeyShortcut [ Keyboard.MetaLeft, Keyboard.Enter ] OnAddNoteSubmit "Submit Note"
    , closeFormShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


suspendOrResumeConfirmationScreenShortcuts : List KeyboardShortcut
suspendOrResumeConfirmationScreenShortcuts =
    [ singleKeyShortcut Keyboard.Enter OnResumeOrSuspendSubmitClick "Confirm"
    , singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Cancel"
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]
