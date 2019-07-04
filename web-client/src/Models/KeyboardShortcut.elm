module Models.KeyboardShortcut exposing
    ( KeyboardShortcut
    , mainScreenShortcuts
    )

import DefaultServices.Keyboard as Keyboard
import Msg exposing (..)


type alias KeyboardShortcut =
    { key : Keyboard.Key
    , msg : Msg
    , keysStr : String
    , description : String
    }


singleKeyShortcut : Keyboard.Key -> Msg -> String -> KeyboardShortcut
singleKeyShortcut key msg desc =
    { key = key, msg = msg, keysStr = Keyboard.prettyPrintKey key, description = desc }


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
