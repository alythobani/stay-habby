module Models.KeyboardShortcut exposing
    ( KeyboardShortcut
    , editGoalScreenShortcuts
    , mainScreenShortcuts
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
    , singleKeyShortcut Keyboard.Slash ToggleAvailableKeyboardShortcutsScreen "Toggle Keyboard Shortcuts Screen"
    ]


editGoalScreenShortcuts : List KeyboardShortcut
editGoalScreenShortcuts =
    [ singleKeyShortcut Keyboard.Enter OnEditGoalSubmit "Submit Form"
    , singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Cancel and Close Form"
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
