module Models.KeyboardShortcut exposing
    ( KeyboardShortcut
    , addNewHabitScreenShortcuts
    , addNoteHabitSelectionShortcuts
    , addNoteScreenShortcuts
    , chooseDateScreenShortcuts
    , createUserFormShortcuts
    , editGoalHabitSelectionShortcuts
    , editGoalScreenShortcuts
    , editInfoHabitSelectionShortcuts
    , editInfoScreenShortcuts
    , errorMessageScreenShortcuts
    , graphHabitSelectionShortcuts
    , graphScreenShortcuts
    , loginFormShortcuts
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
import Set


type alias KeyboardShortcut =
    { rule : Keyboard.Key -> Keyboard.Model -> Bool
    , shortcutMsg : Msg
    , ruleStr : String
    , shortcutDesc : String
    }


singleKeyShortcut : Keyboard.Key -> Msg -> String -> KeyboardShortcut
singleKeyShortcut key msg desc =
    { rule = \keyDown keyCodeSet -> key == keyDown
    , shortcutMsg = msg
    , ruleStr = Keyboard.prettyPrintKey key
    , shortcutDesc = desc
    }


specialKeyShortcut : Keyboard.Key -> Msg -> String -> KeyboardShortcut
specialKeyShortcut key msg desc =
    { rule =
        \keyDown keyCodeSet ->
            let
                keysDownList =
                    keyCodeSet |> Set.toList |> List.map Keyboard.fromCode

                specialKeys =
                    [ Keyboard.MetaLeft
                    , Keyboard.MetaRight
                    , Keyboard.ControlLeft
                    , Keyboard.ControlRight
                    , Keyboard.OSLeft
                    , Keyboard.OSRight
                    ]
            in
            List.any (\specialKey -> List.member specialKey keysDownList) specialKeys && keyDown == key
    , shortcutMsg = msg
    , ruleStr = "Ctrl/⌘ + " ++ Keyboard.prettyPrintKey key
    , shortcutDesc = desc
    }


toggleAvailableKeyboardShortcutsScreenShortcut : KeyboardShortcut
toggleAvailableKeyboardShortcutsScreenShortcut =
    specialKeyShortcut Keyboard.Slash ToggleAvailableKeyboardShortcutsScreen "Toggle Shortcuts Screen"


closeFormShortcut : KeyboardShortcut
closeFormShortcut =
    singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Close Form"


cancelScreenShortcut : KeyboardShortcut
cancelScreenShortcut =
    singleKeyShortcut Keyboard.Escape OnExitDialogScreen "Cancel"


darkModeShortcut : KeyboardShortcut
darkModeShortcut =
    singleKeyShortcut Keyboard.KeyD OnToggleDarkMode "Toggle Dark Mode"


loginFormShortcuts : List KeyboardShortcut
loginFormShortcuts =
    [ specialKeyShortcut Keyboard.Enter OnLoginFormEnterKeydown "Log In"
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


createUserFormShortcuts : List KeyboardShortcut
createUserFormShortcuts =
    [ specialKeyShortcut Keyboard.Enter OnCreateUserFormEnterKeydown "Create User"
    , toggleAvailableKeyboardShortcutsScreenShortcut
    ]


mainScreenShortcuts : List KeyboardShortcut
mainScreenShortcuts =
    [ singleKeyShortcut Keyboard.KeyA OpenSetHabitDataShortcutHabitSelectionScreen "Set Habit Amount"
    , singleKeyShortcut Keyboard.KeyC OpenChooseCustomDateDialog "Change Date"
    , darkModeShortcut
    , singleKeyShortcut Keyboard.KeyE OpenEditGoalHabitSelectionScreen "Edit Goal"
    , singleKeyShortcut Keyboard.KeyG OpenGraphHabitSelectionScreen "View Graph"
    , singleKeyShortcut Keyboard.KeyH OpenAddHabitForm "Add New Habit"
    , singleKeyShortcut Keyboard.KeyI OpenEditInfoHabitSelectionScreen "Edit Info"
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
    [ specialKeyShortcut Keyboard.Enter AddHabitFormSubmit "Submit Form"
    , closeFormShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
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
    , toggleAvailableKeyboardShortcutsScreenShortcut
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


editInfoHabitSelectionShortcuts : List KeyboardShortcut
editInfoHabitSelectionShortcuts =
    habitSelectionShortcuts
        OnEditInfoHabitSelectionSelectPreviousHabit
        OnEditInfoHabitSelectionSelectNextHabit
        OnEditInfoHabitSelectionEnterKeydown


editInfoScreenShortcuts : List KeyboardShortcut
editInfoScreenShortcuts =
    [ specialKeyShortcut Keyboard.Enter OnEditInfoSubmitClick "Submit Form"
    , closeFormShortcut
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
    [ specialKeyShortcut Keyboard.Enter OnAddNoteSubmit "Submit Note"
    , closeFormShortcut
    , toggleAvailableKeyboardShortcutsScreenShortcut
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
