module View exposing (view)

import Array
import Browser
import Color
import DefaultServices.Keyboard as Keyboard
import DefaultServices.Util as Util
import Dict
import HabitUtil
import Html exposing (Html, button, div, hr, i, img, input, span, text, textarea)
import Html.Attributes exposing (class, classList, id, placeholder, src, tabindex, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import LineChart
import Maybe.Extra as Maybe
import Model exposing (Model)
import Models.ApiError as ApiError
import Models.DialogScreen as DialogScreen
import Models.FrequencyStats as FrequencyStats
import Models.Graph as Graph
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.HabitDayNote as HabitDayNote
import Models.HabitGoalIntervalList as HabitGoalIntervalList
import Models.KeyboardShortcut as KeyboardShortcut
import Models.Login as Login
import Models.User as User
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData


view : Model -> Browser.Document Msg
view model =
    { title = "Stay Habby"
    , body =
        case model.user of
            Just user ->
                [ div
                    [ classList
                        [ ( "logged-in-view", True )
                        , ( "dark-mode", model.darkModeOn )
                        ]
                    , onClick OnGlobalClick
                    ]
                    [ div
                        [ class "panels-container" ]
                        [ renderTopPanel
                            model.selectedYmd
                            model.actualYmd
                            model.darkModeOn
                            model.errorMessage
                            model.openTopPanelDateDropdown
                            model.openUserActionsDropdown
                        , renderHabitsPanel
                            model.selectedYmd
                            model.allHabits
                            model.allHabitData
                            model.allFrequencyStats
                            model.editingHabitAmountDict
                            model.habitActionsDropdown
                        , renderAddHabitForm
                            model.activeDialogScreen
                            user
                            model.addHabit
                            model.allHabits
                        ]
                    , renderDialogBackgroundScreen model.activeDialogScreen
                    , renderAvailableKeyboardShortcutsScreen
                        model.showAvailableKeyboardShortcutsScreen
                        model.keyboardShortcutsList
                    , renderChooseDateDialog
                        model.activeDialogScreen
                        model.chooseDateDialogChosenYmd
                        model.actualYmd
                    , renderSetHabitDataShortcutHabitSelectionScreen
                        model.activeDialogScreen
                        model.setHabitDataShortcutHabitNameFilterText
                        model.setHabitDataShortcutFilteredHabits
                        model.setHabitDataShortcutSelectedHabitIndex
                    , renderSetHabitDataShortcutAmountScreen
                        model.activeDialogScreen
                        model.allHabitData
                        model.setHabitDataShortcutAmountScreenHabit
                        model.setHabitDataShortcutAmountScreenInputInt
                        model.selectedYmd
                    , renderEditGoalHabitSelectionScreen
                        model.activeDialogScreen
                        model.editGoalHabitSelectionFilterText
                        model.editGoalHabitSelectionFilteredHabits
                        model.editGoalHabitSelectionSelectedHabitIndex
                    , renderEditGoalDialog
                        model.activeDialogScreen
                        model.editGoalDialogHabit
                        model.editGoalDialogHabitCurrentFcrWithIndex
                        model.editGoalConfirmationMessage
                        model.editGoalNewFrequenciesList
                        model.editGoal
                    , renderEditInfoHabitSelectionScreen
                        model.activeDialogScreen
                        model.editInfoHabitSelectionFilterText
                        model.editInfoHabitSelectionFilteredHabits
                        model.editInfoHabitSelectionSelectedHabitIndex
                    , renderEditInfoDialog
                        model.activeDialogScreen
                        model.editInfoDialogHabit
                        model.editInfo
                    , renderErrorMessage
                        model.errorMessage
                        model.activeDialogScreen
                    , renderAddNoteHabitSelectionScreen
                        model.activeDialogScreen
                        model.addNoteHabitSelectionFilterText
                        model.addNoteHabitSelectionFilteredHabits
                        model.addNoteHabitSelectionSelectedHabitIndex
                    , renderAddNoteDialog
                        model.activeDialogScreen
                        model.addNoteDialogHabit
                        model.addNoteDialogInput
                        model.selectedYmd
                        model.allHabitDayNotes
                    , renderSuspendOrResumeHabitSelectionScreen
                        model.activeDialogScreen
                        model.suspendOrResumeHabitSelectionFilterText
                        model.suspendOrResumeHabitSelectionFilteredHabits
                        model.suspendOrResumeHabitSelectionSelectedHabitIndex
                    , renderSuspendOrResumeConfirmationScreen
                        model.activeDialogScreen
                        model.suspendOrResumeHabit
                        model.selectedYmd
                        model.suspendOrResumeHabitConfirmationMessage
                        model.suspendOrResumeHabitNewSuspensions
                    , renderGraphHabitSelectionScreen
                        model.activeDialogScreen
                        model.graphHabitSelectionFilterText
                        model.graphHabitSelectionFilteredHabits
                        model.graphHabitSelectionSelectedHabitIndex
                    , renderGraphDialogScreen
                        model.activeDialogScreen
                        model.graphHabit
                        model.selectedYmd
                        model.graphNumDaysToShow
                        model.allHabitData
                        model.allHabitDayNotes
                        model.graphGoalIntervals
                        model.graphIntervalsData
                        model.darkModeOn
                        model.graphHoveredPoint
                    ]
                ]

            Nothing ->
                [ renderLoggedOutView model.loginPageFields
                , renderAvailableKeyboardShortcutsScreen
                    model.showAvailableKeyboardShortcutsScreen
                    model.keyboardShortcutsList
                ]
    }


renderLoggedOutView : Login.LoginPageFields -> Html Msg
renderLoggedOutView loginPageFields =
    let
        formErrorMessageDiv : String -> Html Msg
        formErrorMessageDiv errorMessageStr =
            div [ class "form-error-message" ] [ text errorMessageStr ]
    in
    div
        [ class "logged-out-view" ]
        [ div [ class "welcome-message" ] [ text "Welcome to Habby!" ]
        , img
            [ class "laughing-buddha-picture"
            , src "assets/laughing_buddha_white.png"
            ]
            []
        , div
            [ class "choose-login-form-or-create-user-form-buttons" ]
            [ button
                [ classList
                    [ ( "selected", loginPageFields.loginOrCreateUserForm == Login.LoginForm )
                    ]
                , onClick OnClickChooseLoginFormButton
                ]
                [ text "Existing User" ]
            , button
                [ classList
                    [ ( "selected", loginPageFields.loginOrCreateUserForm == Login.CreateUserForm )
                    ]
                , onClick OnClickChooseCreateUserFormButton
                ]
                [ text "New User" ]
            ]
        , div
            [ classList
                [ ( "login-form", True )
                , ( "display-none", loginPageFields.loginOrCreateUserForm /= Login.LoginForm )
                ]
            ]
            [ input
                [ id "login-form-username-input"
                , placeholder "Username..."
                , onInput OnLoginFormUsernameInput
                , value loginPageFields.loginFormUsername
                ]
                []
            , input
                [ placeholder "Password..."
                , type_ "password"
                , onInput OnLoginFormPasswordInput
                , value loginPageFields.loginFormPassword
                ]
                []
            , if loginPageFields.loginFormUsername == "" || loginPageFields.loginFormPassword == "" then
                Util.hiddenDiv

              else
                button [ onClick OnLoginUserClick ] [ text "Log In" ]
            , case loginPageFields.loginErrorMessage of
                Just loginErrorMessage ->
                    formErrorMessageDiv loginErrorMessage

                Nothing ->
                    Util.hiddenDiv
            ]
        , div
            [ classList
                [ ( "create-user-form", True )
                , ( "display-none", loginPageFields.loginOrCreateUserForm /= Login.CreateUserForm )
                ]
            ]
            [ input
                [ id "create-user-form-username-input"
                , placeholder "Username..."
                , onInput OnCreateUserFormUsernameInput
                , value loginPageFields.createUserFormUsername
                ]
                []
            , if loginPageFields.doesCreateUserFormUsernameHaveAtLeast1Character then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Username needs at least one character."
            , input
                [ placeholder "Display name..."
                , onInput OnCreateUserFormDisplayNameInput
                , value loginPageFields.createUserFormDisplayName
                ]
                []
            , if loginPageFields.doesCreateUserFormDisplayNameHaveAtLeast1Character then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Display name needs at least one character."
            , input
                [ placeholder "Email address (optional)..."
                , onInput OnCreateUserFormEmailAddressInput
                , value loginPageFields.createUserFormEmailAddress
                ]
                []
            , if loginPageFields.isCreateUserFormEmailAddressValid || loginPageFields.createUserFormEmailAddress == "" then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Invalid email address."
            , input
                [ placeholder "Password..."
                , type_ "password"
                , onInput OnCreateUserFormPasswordInput
                , value loginPageFields.createUserFormPassword
                ]
                []
            , if loginPageFields.doesCreateUserFormPasswordHaveAtLeast10Characters then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Password needs at least 10 characters."
            , if loginPageFields.doesCreateUserFormPasswordHaveAtMost128Characters then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Password must be at most 128 characters long."
            , if loginPageFields.doesCreateUserFormPasswordHaveALowerCaseCharacter then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Password needs at least one lowercase character (a-z)."
            , if loginPageFields.doesCreateUserFormPasswordHaveAnUpperCaseCharacter then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Password needs at least one uppercase character (A-Z)."
            , if loginPageFields.doesCreateUserFormPasswordHaveADigit then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Password needs at least one digit (0-9)."
            , input
                [ placeholder "Repeat password..."
                , type_ "password"
                , onInput OnCreateUserFormRepeatPasswordInput
                , value loginPageFields.createUserFormRepeatPassword
                ]
                []
            , if loginPageFields.doesCreateUserFormRepeatPasswordMatch then
                Util.hiddenDiv

              else
                formErrorMessageDiv "Passwords do not match."
            , case Login.extractCreateUserFields loginPageFields of
                Just createUserFields ->
                    button
                        [ onClick <| OnSignUpUserClick createUserFields ]
                        [ text "Sign Up" ]

                Nothing ->
                    Util.hiddenDiv
            , case loginPageFields.signUpErrorMessage of
                Just signUpErrorMessage ->
                    formErrorMessageDiv signUpErrorMessage

                Nothing ->
                    Util.hiddenDiv
            ]
        ]


renderTopPanel :
    Maybe YmdDate.YmdDate
    -> Maybe YmdDate.YmdDate
    -> Bool
    -> Maybe String
    -> Bool
    -> Bool
    -> Html Msg
renderTopPanel maybeSelectedYmd maybeActualYmd darkModeOn errorMessage openDateDropdown openUserActionsDropdown =
    let
        topPanelTitleText : String
        topPanelTitleText =
            case ( maybeSelectedYmd, maybeActualYmd ) of
                ( Just selectedYmd, Just actualYmd ) ->
                    if selectedYmd == actualYmd then
                        "Today's Progress"

                    else if selectedYmd == YmdDate.addDays 1 actualYmd then
                        "Tomorrow's Progress"

                    else if selectedYmd == YmdDate.addDays -1 actualYmd then
                        "Yesterday's Progress"

                    else if YmdDate.compareYmds selectedYmd actualYmd == LT then
                        "Past Progress"

                    else
                        "Future Progress"

                ( _, Nothing ) ->
                    -- We don't know the actual date
                    "..."

                ( Nothing, _ ) ->
                    "No Selected Date"
    in
    div
        [ class "top-panel" ]
        [ div [ class "top-panel-title" ] [ text topPanelTitleText ]
        , div
            [ classList
                [ ( "top-panel-error-message-icon", True )
                , ( "display-none", not <| Maybe.isJust errorMessage )
                ]
            , onClick OpenErrorMessageDialogScreen
            ]
            [ i [ class "material-icons" ] [] ]
        , div
            [ class "top-panel-dark-mode-switch"
            , onClick OnToggleDarkMode
            ]
            [ div
                [ classList
                    [ ( "top-panel-dark-mode-switch-toggler", True )
                    , ( "top-panel-dark-mode-switch-toggler-checked", darkModeOn )
                    ]
                ]
                [ span [ class "top-panel-dark-mode-switch-toggler-slider" ] [] ]
            , div
                [ class "top-panel-dark-mode-switch-text" ]
                [ text <|
                    if darkModeOn then
                        "Dark Mode"

                    else
                        "Light Mode"
                ]
            ]
        , div
            [ class "user-actions-dropdown" ]
            [ div
                [ class <|
                    if openUserActionsDropdown then
                        "top-panel-user-actions-dropdown-toggler-full"

                    else
                        "top-panel-user-actions-dropdown-toggler-default"
                , Util.onClickStopPropagation (Just ToggleTopPanelUserActionsDropdown)
                ]
                [ text "" ]
            , div
                [ classList
                    [ ( "top-panel-user-actions-dropdown-buttons", True )
                    , ( "display-none", not openUserActionsDropdown )
                    ]
                ]
                [ button
                    [ class "top-panel-user-actions-dropdown-button"
                    , onClick <| OnLogoutUserClick
                    ]
                    [ text "Log Out" ]
                ]
            ]
        , div
            [ class "top-panel-date" ]
            [ text <|
                Maybe.withDefault
                    "..."
                    (Maybe.map YmdDate.prettyPrintWithWeekday maybeSelectedYmd)
            , div [ class "top-panel-date-dropdown" ]
                [ div
                    [ class <|
                        if openDateDropdown then
                            "top-panel-date-dropdown-toggler-full"

                        else
                            "top-panel-date-dropdown-toggler-default"
                    , Util.onClickStopPropagation (Just ToggleTopPanelDateDropdown)
                    ]
                    [ text "" ]
                , div
                    [ class "top-panel-date-dropdown-buttons" ]
                    [ button
                        [ classList
                            [ ( "top-panel-date-dropdown-button", True )
                            , ( "display-none", not openDateDropdown )
                            ]
                        , onClick <| SetSelectedDateToXDaysFromToday 0
                        ]
                        [ text "Today" ]
                    , button
                        [ classList
                            [ ( "top-panel-date-dropdown-button", True )
                            , ( "display-none", not openDateDropdown )
                            ]
                        , onClick <| SetSelectedDateToXDaysFromToday -1
                        ]
                        [ text "Yesterday" ]
                    , button
                        [ classList
                            [ ( "top-panel-date-dropdown-button", True )
                            , ( "display-none", not openDateDropdown )
                            ]
                        , onClick <| SetSelectedDateToXDaysFromToday 1
                        ]
                        [ text "Tomorrow" ]
                    , button
                        [ classList
                            [ ( "top-panel-date-dropdown-button", True )
                            , ( "display-none", not openDateDropdown )
                            ]
                        , onClick OpenChooseCustomDateDialog
                        ]
                        [ text "Custom Date" ]
                    ]
                ]
            ]
        ]


renderHabitsPanel :
    Maybe YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Dict.Dict String Int
    -> Maybe String
    -> Html Msg
renderHabitsPanel maybeSelectedYmd rdHabits rdHabitData rdFrequencyStatsList editingHabitAmountDict habitActionsDropdown =
    div
        [ class "habits-panel" ]
        (case ( rdHabits, rdHabitData, maybeSelectedYmd ) of
            ( RemoteData.Success habits, RemoteData.Success habitData, Just selectedYmd ) ->
                let
                    ( goodHabits, badHabits ) =
                        Habit.splitHabits habits

                    ( sortedGoodHabits, sortedBadHabits, sortedSuspendedHabits ) =
                        case rdFrequencyStatsList of
                            RemoteData.Success frequencyStatsList ->
                                let
                                    ( goodActiveHabits, badActiveHabits, suspendedHabits ) =
                                        HabitUtil.splitHabitsByCurrentlySuspended frequencyStatsList goodHabits badHabits
                                in
                                ( HabitUtil.sortHabitsByCurrentFragment frequencyStatsList goodActiveHabits
                                , HabitUtil.sortHabitsByCurrentFragment frequencyStatsList badActiveHabits
                                , HabitUtil.sortHabitsByCurrentFragment frequencyStatsList suspendedHabits
                                )

                            _ ->
                                ( goodHabits, badHabits, [] )

                    firstHabit : Maybe Habit.Habit
                    firstHabit =
                        List.head <| sortedGoodHabits ++ sortedBadHabits ++ sortedSuspendedHabits

                    renderHabit habit =
                        renderHabitBox
                            (case rdFrequencyStatsList of
                                RemoteData.Success frequencyStatsList ->
                                    HabitUtil.findFrequencyStatsForHabit
                                        habit
                                        frequencyStatsList

                                _ ->
                                    Nothing
                            )
                            selectedYmd
                            habitData
                            editingHabitAmountDict
                            habitActionsDropdown
                            (firstHabit == Just habit)
                            habit
                in
                if List.isEmpty habits then
                    [ div
                        [ class "habits-panel-empty-habby-message" ]
                        [ div
                            [ class "habits-panel-empty-habby-message-header" ]
                            [ text "Welcome to Habby!" ]
                        , div
                            [ class "habits-panel-empty-habby-message-body" ]
                            [ text "Start by adding a habit below." ]
                        ]
                    ]

                else
                    [ div
                        [ class "habit-list good-habits" ]
                        (List.map renderHabit sortedGoodHabits)
                    , div
                        [ class "habit-list bad-habits" ]
                        (List.map renderHabit sortedBadHabits)
                    , div
                        [ class "habit-list suspended-habits" ]
                        (List.map renderHabit sortedSuspendedHabits)
                    ]

            ( RemoteData.Failure apiError, _, _ ) ->
                [ span [ class "retrieving-habits-status" ] [ text "Failure..." ] ]

            ( _, RemoteData.Failure apiError, _ ) ->
                [ span [ class "retrieving-habits-status" ] [ text "Failure..." ] ]

            _ ->
                [ span [ class "retrieving-habits-status" ] [ text "Loading..." ] ]
        )


renderTemplateHabit : Habit.AddHabitInputData -> Html Msg
renderTemplateHabit addHabit =
    let
        ( goalAmountToShow, numDaysLeftToShow ) =
            case addHabit.frequencyKind of
                Habit.TotalWeekFrequencyKind ->
                    ( Maybe.withDefault 0 addHabit.timesPerWeek, 6 )

                Habit.SpecificDayOfWeekFrequencyKind ->
                    ( Maybe.withDefault 0 addHabit.mondayTimes, 0 )

                Habit.EveryXDayFrequencyKind ->
                    ( Maybe.withDefault 0 addHabit.times
                    , case addHabit.days of
                        Just days ->
                            days - 1

                        Nothing ->
                            0
                    )
    in
    div
        [ classList
            [ ( "add-habit-template-habits-list-habit-box", True )
            , ( "good-habit", addHabit.kind == Habit.GoodHabitKind )
            ]
        , onClick <| OnAddHabitTemplateClick addHabit
        ]
        [ div [ class "name" ] [ text addHabit.name ]
        , div [ class "description" ] [ text addHabit.description ]
        , div
            [ class "frequency-stats-list" ]
            [ div
                [ class "current-progress" ]
                [ text <| "0 out of " ++ String.fromInt goalAmountToShow ++ " " ++ addHabit.unitNamePlural ]
            , div [ class "frequency-statistic" ] [ text <| "Days left: " ++ String.fromInt numDaysLeftToShow ]
            , div [ class "frequency-statistic" ] [ text "100%" ]
            , div [ class "frequency-statistic" ] [ text "Streak: 0" ]
            , div [ class "frequency-statistic" ] [ text "Best streak: 0" ]
            , div [ class "frequency-statistic" ] [ text "Total done: 0" ]
            ]
        , div
            [ class "habit-amount-complete" ]
            [ input [ placeholder <| "0 " ++ addHabit.unitNamePlural ] [] ]
        ]


renderAddHabitForm :
    Maybe DialogScreen.DialogScreen
    -> User.User
    -> Habit.AddHabitInputData
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> Html Msg
renderAddHabitForm activeDialogScreen user addHabit rdAllHabits =
    let
        maybeCreateHabitData =
            Habit.extractCreateHabit (Just user) addHabit

        showForm =
            activeDialogScreen == Just DialogScreen.AddNewHabitScreen
    in
    case rdAllHabits of
        RemoteData.Success allHabits ->
            let
                isDuplicateHabitName =
                    List.any (\otherHabit -> (otherHabit |> Habit.getCommonFields |> .name) == addHabit.name) allHabits

                everyXDayFrequencyTagNameString =
                    (case addHabit.times of
                        Just t ->
                            if t == 1 then
                                "1 Unit Per "

                            else
                                String.fromInt t ++ " Units Per "

                        Nothing ->
                            "X Units Per "
                    )
                        ++ (case addHabit.days of
                                Just days ->
                                    if days == 1 then
                                        "Day"

                                    else
                                        String.fromInt days ++ " Days"

                                Nothing ->
                                    " X Days"
                           )

                totalWeekFrequencyTagNameString =
                    case addHabit.timesPerWeek of
                        Just t ->
                            if t == 1 then
                                "1 Unit Per Week"

                            else
                                String.fromInt t ++ " Units Per Week"

                        Nothing ->
                            "X Units Per Week"

                initAddHabit =
                    Habit.initAddHabitData

                templateHabits : List Habit.AddHabitInputData
                templateHabits =
                    [ { initAddHabit
                        | name = "Meditation"
                        , description = "relax my mind"
                        , unitNameSingular = "minute"
                        , unitNamePlural = "minutes"
                        , frequencyKind = Habit.EveryXDayFrequencyKind
                        , goodHabitTime = Habit.Morning
                        , kind = Habit.GoodHabitKind
                        , times = Just 30
                        , days = Just 1
                      }
                    , { initAddHabit
                        | name = "Brush and Floss"
                        , description = "clean dem teeth n gums"
                        , unitNameSingular = "brush sesh"
                        , unitNamePlural = "brush seshes"
                        , frequencyKind = Habit.EveryXDayFrequencyKind
                        , goodHabitTime = Habit.Morning
                        , kind = Habit.GoodHabitKind
                        , times = Just 2
                        , days = Just 1
                      }
                    , { initAddHabit
                        | name = "Water"
                        , description = "hydrate for vitality"
                        , unitNameSingular = "mL"
                        , unitNamePlural = "mL"
                        , frequencyKind = Habit.EveryXDayFrequencyKind
                        , goodHabitTime = Habit.Anytime
                        , kind = Habit.GoodHabitKind
                        , times = Just 3000
                        , days = Just 1
                      }
                    , { initAddHabit
                        | name = "Workout"
                        , description = "break down muscles so they can become stronger"
                        , unitNameSingular = "session"
                        , unitNamePlural = "sessions"
                        , frequencyKind = Habit.EveryXDayFrequencyKind
                        , times = Just 3
                        , days = Just 4
                        , goodHabitTime = Habit.Anytime
                        , kind = Habit.GoodHabitKind
                      }
                    , { initAddHabit
                        | name = "Cold Shower"
                        , description = "strong mind, strong body, strong soul"
                        , unitNameSingular = "shower"
                        , unitNamePlural = "showers"
                        , frequencyKind = Habit.EveryXDayFrequencyKind
                        , kind = Habit.GoodHabitKind
                        , goodHabitTime = Habit.Morning
                        , times = Just 1
                        , days = Just 1
                      }
                    , { initAddHabit
                        | name = "Afternoon Nap"
                        , description = "zzzzzzzzzzzzzzzzzzzzzzzzzzzz"
                        , unitNameSingular = "minute"
                        , unitNamePlural = "minutes"
                        , frequencyKind = Habit.EveryXDayFrequencyKind
                        , kind = Habit.GoodHabitKind
                        , times = Just 20
                        , days = Just 1
                      }
                    , { initAddHabit
                        | name = "Coffee"
                        , description = "a delicious morning beverage"
                        , unitNameSingular = "shot of caffeine"
                        , unitNamePlural = "shots of caffeine"
                        , frequencyKind = Habit.TotalWeekFrequencyKind
                        , kind = Habit.BadHabitKind
                        , timesPerWeek = Just 3
                      }
                    , { initAddHabit
                        | name = "Alcohol"
                        , description = "the psychoactive kind"
                        , unitNameSingular = "unit of alcohol"
                        , unitNamePlural = "units of alcohol"
                        , frequencyKind = Habit.SpecificDayOfWeekFrequencyKind
                        , mondayTimes = Nothing
                        , tuesdayTimes = Nothing
                        , wednesdayTimes = Nothing
                        , thursdayTimes = Nothing
                        , fridayTimes = Just 4
                        , saturdayTimes = Nothing
                        , sundayTimes = Nothing
                        , kind = Habit.BadHabitKind
                      }
                    ]
            in
            div
                [ classList [ ( "add-habit", True ), ( "form-is-open", showForm ) ] ]
                [ div [ classList [ ( "add-habit-background", True ), ( "display-none", not showForm ) ], onClick OnExitDialogScreen ] []
                , div
                    [ classList [ ( "add-habit-main-section", True ), ( "display-none", not showForm ) ] ]
                    [ div
                        [ classList [ ( "add-habit-template-habits", True ), ( "display-none", not showForm ) ] ]
                        [ div [ class "add-habit-template-habits-title" ] [ text "Habit Templates" ]
                        , div [ class "add-habit-template-habits-list" ] (List.map renderTemplateHabit templateHabits)
                        ]
                    , div
                        [ classList [ ( "add-habit-form", True ), ( "form-is-open", showForm ) ] ]
                        [ div
                            [ classList
                                [ ( "add-habit-duplicate-habit-name-message", True )
                                , ( "display-none", not isDuplicateHabitName || not showForm )
                                ]
                            ]
                            [ text <| "A habit named \"" ++ addHabit.name ++ "\" already exists. Please choose a different name." ]
                        , div
                            [ classList
                                [ ( "add-habit-form-body", True )
                                , ( "display-none", not showForm )
                                ]
                            ]
                            [ div
                                [ class "add-habit-form-body-habit-tag-name" ]
                                [ button
                                    [ classList [ ( "selected", addHabit.kind == Habit.GoodHabitKind ) ]
                                    , onClick <| OnSelectAddHabitKind Habit.GoodHabitKind
                                    ]
                                    [ text "Good Habit" ]
                                , button
                                    [ classList [ ( "selected", addHabit.kind == Habit.BadHabitKind ) ]
                                    , onClick <| OnSelectAddHabitKind Habit.BadHabitKind
                                    ]
                                    [ text "Bad Habit" ]
                                ]
                            , div
                                [ class "add-habit-form-body-name-and-description" ]
                                [ input
                                    [ class "add-habit-form-body-name"
                                    , id "add-habit-form-body-name-input"
                                    , placeholder "Name..."
                                    , onInput OnAddHabitNameInput
                                    , value addHabit.name
                                    ]
                                    []
                                , textarea
                                    [ class "add-habit-form-body-description"
                                    , placeholder "Short description..."
                                    , onInput OnAddHabitDescriptionInput
                                    , value addHabit.description
                                    ]
                                    []
                                ]
                            , div
                                [ classList
                                    [ ( "add-habit-form-body-time-of-day", True )
                                    , ( "display-none", addHabit.kind /= Habit.GoodHabitKind )
                                    ]
                                ]
                                [ button
                                    [ classList [ ( "habit-time-of-day", True ), ( "selected", addHabit.goodHabitTime == Habit.Anytime ) ]
                                    , onClick <| OnSelectAddGoodHabitTime Habit.Anytime
                                    ]
                                    [ text "ANYTIME" ]
                                , button
                                    [ classList [ ( "habit-time-of-day", True ), ( "selected", addHabit.goodHabitTime == Habit.Morning ) ]
                                    , onClick <| OnSelectAddGoodHabitTime Habit.Morning
                                    ]
                                    [ text "MORNING" ]
                                , button
                                    [ classList [ ( "habit-time-of-day", True ), ( "selected", addHabit.goodHabitTime == Habit.Evening ) ]
                                    , onClick <| OnSelectAddGoodHabitTime Habit.Evening
                                    ]
                                    [ text "EVENING" ]
                                ]
                            , div
                                [ class "add-habit-form-body-unit-name" ]
                                [ input
                                    [ id "add-habit-form-body-unit-name-singular-input"
                                    , class "habit-unit-name-singular"
                                    , placeholder "unit name singular..."
                                    , onInput OnAddHabitUnitNameSingularInput
                                    , value addHabit.unitNameSingular
                                    ]
                                    []
                                , input
                                    [ class "habit-unit-name-plural"
                                    , placeholder "unit name plural..."
                                    , onInput OnAddHabitUnitNamePluralInput
                                    , value addHabit.unitNamePlural
                                    ]
                                    []
                                ]
                            , div
                                [ class "add-habit-form-body-frequency-tag-name" ]
                                [ button
                                    [ classList [ ( "selected", addHabit.frequencyKind == Habit.EveryXDayFrequencyKind ) ]
                                    , onClick <| OnAddHabitSelectFrequencyKind Habit.EveryXDayFrequencyKind
                                    ]
                                    [ text everyXDayFrequencyTagNameString ]
                                , button
                                    [ classList [ ( "selected", addHabit.frequencyKind == Habit.TotalWeekFrequencyKind ) ]
                                    , onClick <| OnAddHabitSelectFrequencyKind Habit.TotalWeekFrequencyKind
                                    ]
                                    [ text totalWeekFrequencyTagNameString ]
                                , button
                                    [ classList [ ( "selected", addHabit.frequencyKind == Habit.SpecificDayOfWeekFrequencyKind ) ]
                                    , onClick <| OnAddHabitSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind
                                    ]
                                    [ text "Specific Days of Week" ]
                                ]
                            , div
                                [ classList
                                    [ ( "add-habit-form-body-x-times-per-week", True )
                                    , ( "display-none", addHabit.frequencyKind /= Habit.TotalWeekFrequencyKind )
                                    ]
                                ]
                                [ input
                                    [ id "add-habit-form-body-times-per-week-input"
                                    , placeholder <|
                                        if addHabit.unitNamePlural == "" then
                                            "units"

                                        else
                                            addHabit.unitNamePlural
                                    , onInput OnAddHabitTimesPerWeekInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.timesPerWeek)
                                    ]
                                    []
                                ]
                            , div
                                [ classList
                                    [ ( "add-habit-form-body-specific-days-of-week", True )
                                    , ( "display-none", addHabit.frequencyKind /= Habit.SpecificDayOfWeekFrequencyKind )
                                    ]
                                ]
                                [ input
                                    [ id "add-habit-form-body-monday-input"
                                    , placeholder "monday"
                                    , onInput OnAddHabitSpecificDayMondayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.mondayTimes)
                                    ]
                                    []
                                , input
                                    [ placeholder "tuesday"
                                    , onInput OnAddHabitSpecificDayTuesdayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.tuesdayTimes)
                                    ]
                                    []
                                , input
                                    [ placeholder "wednesday"
                                    , onInput OnAddHabitSpecificDayWednesdayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.wednesdayTimes)
                                    ]
                                    []
                                , input
                                    [ placeholder "thursday"
                                    , onInput OnAddHabitSpecificDayThursdayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.thursdayTimes)
                                    ]
                                    []
                                , input
                                    [ placeholder "friday"
                                    , onInput OnAddHabitSpecificDayFridayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.fridayTimes)
                                    ]
                                    []
                                , input
                                    [ placeholder "saturday"
                                    , onInput OnAddHabitSpecificDaySaturdayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.saturdayTimes)
                                    ]
                                    []
                                , input
                                    [ placeholder "sunday"
                                    , onInput OnAddHabitSpecificDaySundayInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.sundayTimes)
                                    ]
                                    []
                                ]
                            , div
                                [ classList
                                    [ ( "add-habit-form-body-x-times-per-y-days", True )
                                    , ( "display-none", addHabit.frequencyKind /= Habit.EveryXDayFrequencyKind )
                                    ]
                                ]
                                [ input
                                    [ id "add-habit-form-body-times-input"
                                    , placeholder <|
                                        if addHabit.unitNamePlural == "" then
                                            "units"

                                        else
                                            addHabit.unitNamePlural
                                    , onInput OnAddHabitTimesInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.times)
                                    ]
                                    []
                                , input
                                    [ placeholder "days"
                                    , onInput OnAddHabitDaysInput
                                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.days)
                                    ]
                                    []
                                ]
                            , case ( maybeCreateHabitData, isDuplicateHabitName ) of
                                ( Just createHabitData, False ) ->
                                    button
                                        [ class "add-habit-form-submit-button"
                                        , onClick AddHabitFormSubmit
                                        ]
                                        [ text "Create Habit" ]

                                _ ->
                                    Util.hiddenDiv
                            ]
                        ]
                    ]
                , button
                    [ classList [ ( "add-habit-open-form-button", True ), ( "form-is-open", showForm ) ]
                    , onClick <|
                        if showForm then
                            OnExitDialogScreen

                        else
                            OpenAddHabitForm
                    ]
                    [ text <|
                        if showForm then
                            "Cancel"

                        else
                            "Add Habit"
                    ]
                ]

        _ ->
            Util.hiddenDiv


habitActionsDropdownDiv :
    Bool
    -> YmdDate.YmdDate
    -> Habit.Habit
    -> List Habit.SuspendedInterval
    -> Html Msg
habitActionsDropdownDiv dropdown selectedYmd habit suspensions =
    let
        suspensionsArray =
            Array.fromList suspensions

        currentSuspendedIntervalWithIndex : Maybe ( Int, Habit.SuspendedInterval )
        currentSuspendedIntervalWithIndex =
            Util.firstInstanceInArray suspensionsArray
                (\interval -> YmdDate.withinYmdDateInterval interval.startDate interval.endDate selectedYmd)

        currentlySuspended : Bool
        currentlySuspended =
            Maybe.isJust currentSuspendedIntervalWithIndex

        habitRecord =
            Habit.getCommonFields habit
    in
    div [ class "actions-dropdown" ]
        [ div
            [ class <|
                if dropdown then
                    "actions-dropdown-toggler-full"

                else
                    "actions-dropdown-toggler-default"
            , Util.onClickStopPropagation (Just <| ToggleHabitActionsDropdown habitRecord.id)
            ]
            [ text "" ]
        , div
            [ classList
                [ ( "action-buttons", True )
                , ( "display-none", not dropdown )
                ]
            ]
            [ button
                [ class "action-button"
                , onClick <| OpenSuspendOrResumeConfirmationScreen habit
                ]
                [ text <|
                    if currentlySuspended then
                        "Resume"

                    else
                        "Suspend"
                ]
            , button [ class "action-button", onClick <| OpenEditGoalScreen habit ] [ text "Edit Goal" ]
            , button [ class "action-button", onClick <| OpenEditInfoScreen habit ] [ text "Edit Info" ]
            , button
                [ class "action-button"
                , onClick <| OpenAddNoteDialog habit
                ]
                [ text "Add Note" ]
            , button
                [ class "action-button"
                , onClick <| OpenGraphDialogScreen habit
                ]
                [ text "View Graph" ]
            ]
        ]


{-| Renders a habit box with the habit data loaded for that particular date.

Requires 2 event handlers, 1 for handling when data is input into the habit box and 1 for when the user wants to
update the habit data.

-}
renderHabitBox :
    Maybe FrequencyStats.FrequencyStats
    -> YmdDate.YmdDate
    -> List HabitData.HabitData
    -> Dict.Dict String Int
    -> Maybe String
    -> Bool
    -> Habit.Habit
    -> Html Msg
renderHabitBox habitStats selectedYmd habitData editingHabitAmountDict habitActionsDropdown isFirstHabit habit =
    let
        habitRecord =
            Habit.getCommonFields habit

        habitDatum =
            List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == selectedYmd) habitData
                |> List.head
                |> Maybe.map .amount
                |> Maybe.withDefault 0

        editingHabitAmount =
            Dict.get habitRecord.id editingHabitAmountDict

        actionsDropdown =
            habitActionsDropdown == Just habitRecord.id

        isCurrentFragmentSuccessful =
            case habitStats of
                Nothing ->
                    False

                Just stats ->
                    HabitUtil.isHabitCurrentFragmentSuccessful habit stats

        frequencyStatisticDiv str =
            div
                [ class "frequency-statistic" ]
                [ text str ]

        amountInputAttrs =
            [ placeholder <|
                String.fromInt habitDatum
                    ++ " "
                    ++ (if habitDatum == 1 then
                            habitRecord.unitNameSingular

                        else
                            habitRecord.unitNamePlural
                       )
            , onInput <| OnHabitAmountInput habitRecord.id
            , Util.onKeydownStopPropagation
                (\key ->
                    if key == Keyboard.Enter then
                        Just <| SetHabitData selectedYmd habitRecord.id editingHabitAmount

                    else
                        Nothing
                )
            , value <| Maybe.withDefault "" (Maybe.map String.fromInt editingHabitAmount)
            ]

        amountInputAttrsWithPossibleId =
            if isFirstHabit then
                id "first-habit-amount-input" :: tabindex 0 :: amountInputAttrs

            else
                amountInputAttrs
    in
    div
        [ class
            (if isCurrentFragmentSuccessful then
                "habit-success"

             else
                case habit of
                    Habit.GoodHabit _ ->
                        "habit-failure"

                    Habit.BadHabit _ ->
                        case habitStats of
                            Nothing ->
                                "habit-success"

                            Just stats ->
                                if not stats.habitHasStarted then
                                    "habit-success"

                                else
                                    "habit-failure"
            )
        ]
        [ div [ class "habit-name" ] [ text habitRecord.name ]
        , habitActionsDropdownDiv actionsDropdown selectedYmd habit habitRecord.suspensions
        , div [ class "habit-description" ] [ text <| Maybe.withDefault "" habitRecord.description ]
        , case habitStats of
            Nothing ->
                div
                    [ class "frequency-stats-list" ]
                    [ frequencyStatisticDiv "Loading..." ]

            Just stats ->
                if not stats.habitHasStarted then
                    div [ class "start-this-habit-message" ] [ text "Start this habit!" ]

                else
                    div [ class "frequency-stats-list" ]
                        [ div
                            [ class "current-progress" ]
                            [ text <|
                                String.fromInt stats.currentFragmentTotal
                                    ++ " out of "
                                    ++ String.fromInt stats.currentFragmentGoal
                                    ++ " "
                                    ++ habitRecord.unitNamePlural
                            ]
                        , frequencyStatisticDiv ("Days left: " ++ String.fromInt stats.currentFragmentDaysLeft)
                        , frequencyStatisticDiv <|
                            if stats.totalFragments == 0 then
                                "100%"

                            else
                                (String.fromInt <|
                                    round <|
                                        toFloat stats.successfulFragments
                                            * 100
                                            / toFloat stats.totalFragments
                                )
                                    ++ "%"
                        , frequencyStatisticDiv ("Streak: " ++ String.fromInt stats.currentFragmentStreak)
                        , frequencyStatisticDiv ("Best streak: " ++ String.fromInt stats.bestFragmentStreak)
                        , frequencyStatisticDiv ("Total done: " ++ String.fromInt stats.totalDone)
                        ]
        , div
            [ classList
                [ ( "habit-amount-complete", True )
                , ( "editing", Maybe.isJust <| editingHabitAmount )
                ]
            ]
            [ input
                amountInputAttrsWithPossibleId
                []
            , i
                [ classList [ ( "material-icons", True ) ]
                , onClick <| SetHabitData selectedYmd habitRecord.id editingHabitAmount
                ]
                [ text "check_box" ]
            ]
        ]


renderDialogBackgroundScreen : Maybe DialogScreen.DialogScreen -> Html Msg
renderDialogBackgroundScreen activeDialogScreen =
    let
        showBackgroundScreen : Bool
        showBackgroundScreen =
            Maybe.isJust activeDialogScreen
    in
    div
        [ classList
            [ ( "dialog-background-screen", True )
            , ( "display-none", not showBackgroundScreen )
            ]
        , onClick OnExitDialogScreen
        ]
        []


renderAvailableKeyboardShortcutsScreen : Bool -> List KeyboardShortcut.KeyboardShortcut -> Html Msg
renderAvailableKeyboardShortcutsScreen showScreen shortcutList =
    let
        renderKeyboardShortcut : KeyboardShortcut.KeyboardShortcut -> Html Msg
        renderKeyboardShortcut shortcut =
            div
                [ class "shortcut" ]
                [ div [ class "shortcut-keys" ] [ text shortcut.ruleStr ]
                , div [ class "shortcut-desc" ] [ text shortcut.shortcutDesc ]
                ]
    in
    div
        [ classList
            [ ( "available-keyboard-shortcuts-screen", True )
            , ( "display-none", not showScreen )
            ]
        , onClick ToggleAvailableKeyboardShortcutsScreen
        ]
        [ div
            [ class "available-keyboard-shortcuts-screen-body" ]
            [ div [ class "available-keyboard-shortcuts-screen-body-title" ] [ text "Available Keyboard Shortcuts" ]
            , div
                [ class "available-keyboard-shortcuts-screen-body-list" ]
                (List.map renderKeyboardShortcut shortcutList)
            ]
        ]


renderChooseDateDialog : Maybe DialogScreen.DialogScreen -> Maybe YmdDate.YmdDate -> Maybe YmdDate.YmdDate -> Html Msg
renderChooseDateDialog activeDialogScreen maybeChosenYmd maybeActualYmd =
    let
        showDialog : Bool
        showDialog =
            activeDialogScreen == Just DialogScreen.ChooseDateDialogScreen
    in
    case ( maybeChosenYmd, maybeActualYmd ) of
        ( Just chosenYmd, Just actualYmd ) ->
            let
                numDaysInMonth =
                    YmdDate.numDaysInMonth chosenYmd

                numCalendarRows =
                    ceiling <| toFloat numDaysInMonth / 7

                weekdayLetters : List String
                weekdayLetters =
                    List.map
                        (\ymd -> ymd |> YmdDate.prettyPrintWeekday |> String.left 1)
                        (List.map (\day -> { chosenYmd | day = day }) (List.range 1 7))

                renderWeekdayLetter : String -> Html Msg
                renderWeekdayLetter weekdayLetter =
                    div
                        [ class "choose-date-dialog-form-calendar-weekday-box" ]
                        [ text weekdayLetter ]

                calendarRows : List (List Int)
                calendarRows =
                    List.map
                        (\rowIndex -> List.range (rowIndex * 7 + 1) (rowIndex * 7 + 7))
                        (List.range 0 (numCalendarRows - 1))

                renderCalendarDayBox : Int -> Html Msg
                renderCalendarDayBox day =
                    let
                        representedYmd =
                            { chosenYmd | day = day }
                    in
                    div
                        [ classList
                            [ ( "choose-date-dialog-form-calendar-day-box", True )
                            , ( "choose-date-dialog-form-calendar-day-box-today", representedYmd == actualYmd )
                            , ( "choose-date-dialog-form-calendar-day-box-chosen", representedYmd == chosenYmd )
                            , ( "choose-date-dialog-form-calendar-day-box-empty-placeholder", day > numDaysInMonth )
                            ]
                        , onClick <| SetChooseDateDialogChosenYmd representedYmd
                        ]
                        [ text <| String.fromInt day ]

                renderCalendarRow : List Int -> Html Msg
                renderCalendarRow days =
                    div
                        [ class "choose-date-dialog-form-calendar-row" ]
                        (List.map renderCalendarDayBox days)

                yesterday =
                    YmdDate.addDays -1 actualYmd

                tomorrow =
                    YmdDate.addDays 1 actualYmd
            in
            div
                [ classList
                    [ ( "choose-date-dialog", True )
                    , ( "display-none", not showDialog )
                    ]
                ]
                [ div
                    [ class "choose-date-dialog-form"
                    , id "choose-date-dialog-form-id"
                    , tabindex 1
                    ]
                    [ div
                        [ class "choose-date-dialog-form-chosen-ymd-text" ]
                        [ text <| YmdDate.prettyPrintWithWeekday chosenYmd ]
                    , div
                        [ class "choose-date-dialog-form-change-date-buttons" ]
                        [ button
                            [ classList
                                [ ( "choose-date-dialog-form-change-date-button", True )
                                , ( "selected", chosenYmd == yesterday )
                                ]
                            , onClick <| SetChooseDateDialogChosenYmd yesterday
                            ]
                            [ text "Yesterday" ]
                        , button
                            [ classList
                                [ ( "choose-date-dialog-form-change-date-button", True )
                                , ( "selected", chosenYmd == actualYmd )
                                ]
                            , onClick <| SetChooseDateDialogChosenYmd actualYmd
                            ]
                            [ text "Today" ]
                        , button
                            [ classList
                                [ ( "choose-date-dialog-form-change-date-button", True )
                                , ( "selected", chosenYmd == tomorrow )
                                ]
                            , onClick <| SetChooseDateDialogChosenYmd tomorrow
                            ]
                            [ text "Tomorrow" ]
                        ]
                    , div
                        [ class "choose-date-dialog-form-calendar" ]
                        [ div
                            [ class "choose-date-dialog-form-calendar-month-or-day-or-year" ]
                            [ span
                                [ class "choose-date-dialog-form-calendar-month-or-day-or-year-left-arrow"
                                , onClick <| OnChooseDateDialogPreviousMonthClick chosenYmd
                                ]
                                []
                            , span [] [ text <| YmdDate.prettyPrintMonth chosenYmd.month ]
                            , span
                                [ class "choose-date-dialog-form-calendar-month-or-day-or-year-right-arrow"
                                , onClick <| OnChooseDateDialogNextMonthClick chosenYmd
                                ]
                                []
                            ]
                        , div
                            [ class "choose-date-dialog-form-calendar-month-or-day-or-year" ]
                            [ span
                                [ class "choose-date-dialog-form-calendar-month-or-day-or-year-left-arrow"
                                , onClick <| OnChooseDateDialogPreviousDayClick chosenYmd
                                ]
                                []
                            , span [] [ text <| YmdDate.prettyPrintDay chosenYmd.day ]
                            , span
                                [ class "choose-date-dialog-form-calendar-month-or-day-or-year-right-arrow"
                                , onClick <| OnChooseDateDialogNextDayClick chosenYmd
                                ]
                                []
                            ]
                        , div
                            [ class "choose-date-dialog-form-calendar-month-or-day-or-year" ]
                            [ span
                                [ class "choose-date-dialog-form-calendar-month-or-day-or-year-left-arrow"
                                , onClick <| OnChooseDateDialogPreviousYearClick chosenYmd
                                ]
                                []
                            , span [] [ text <| String.fromInt chosenYmd.year ]
                            , span
                                [ class "choose-date-dialog-form-calendar-month-or-day-or-year-right-arrow"
                                , onClick <| OnChooseDateDialogNextYearClick chosenYmd
                                ]
                                []
                            ]
                        , div
                            [ class "choose-date-dialog-form-calendar-weekdays" ]
                            (List.map renderWeekdayLetter weekdayLetters)
                        , div
                            [ class "choose-date-dialog-form-calendar-rows" ]
                            (List.map renderCalendarRow calendarRows)
                        ]
                    , div
                        [ class "choose-date-dialog-form-submit-buttons" ]
                        [ button
                            [ class "choose-date-dialog-form-submit-buttons-submit"
                            , onClick OnChooseDateDialogSubmitClick
                            ]
                            [ text "Submit" ]
                        , button
                            [ class "choose-date-dialog-form-submit-buttons-cancel"
                            , onClick OnExitDialogScreen
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

        _ ->
            div
                [ classList
                    [ ( "choose-date-dialog", True )
                    , ( "display-none", not showDialog )
                    ]
                ]
                [ div
                    [ class "choose-date-dialog-form"
                    , id "choose-date-dialog-form-id"
                    , tabindex 1
                    ]
                    [ text "Loading..." ]
                ]


renderHabitSelectionScreen :
    Bool
    -> String
    -> String
    -> (String -> Msg)
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderHabitSelectionScreen showScreen headerText inputId onInputMsg filterText filteredHabits selectedHabitIndex =
    let
        selectedHabit =
            Array.get selectedHabitIndex filteredHabits

        renderHabitOption habit =
            div
                [ classList
                    [ ( "habit-selection-habits-list-option", True )
                    , ( "selected-habit"
                      , case selectedHabit of
                            Just h ->
                                h == habit

                            _ ->
                                False
                      )
                    ]
                ]
                [ text <| .name <| Habit.getCommonFields habit ]
    in
    div
        [ classList
            [ ( "habit-selection-screen", True )
            , ( "display-none", not showScreen )
            ]
        ]
        [ span
            [ class "habit-selection-screen-header" ]
            [ text headerText ]
        , input
            [ id inputId
            , class "habit-selection-filter-text-input"
            , placeholder "Enter a habit's name..."
            , onInput onInputMsg
            , value filterText
            ]
            []
        , div
            [ classList
                [ ( "habit-selection-habits-list", True )
                , ( "display-none", Array.isEmpty filteredHabits )
                ]
            ]
            (Array.map renderHabitOption filteredHabits |> Array.toList)
        ]


renderSetHabitDataShortcutHabitSelectionScreen :
    Maybe DialogScreen.DialogScreen
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderSetHabitDataShortcutHabitSelectionScreen activeDialogScreen habitSelectionFilterText filteredHabits selectedHabitIndex =
    let
        showScreen =
            activeDialogScreen == Just DialogScreen.SetHabitDataShortcutHabitSelectionScreen
    in
    renderHabitSelectionScreen
        showScreen
        "Set Habit Data"
        "set-habit-data-shortcut-habit-selection-filter-text-input"
        OnSetHabitDataShortcutHabitSelectionFilterTextInput
        habitSelectionFilterText
        filteredHabits
        selectedHabitIndex


renderSetHabitDataShortcutAmountScreen :
    Maybe DialogScreen.DialogScreen
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> Maybe Habit.Habit
    -> Maybe Int
    -> Maybe YmdDate.YmdDate
    -> Html Msg
renderSetHabitDataShortcutAmountScreen activeDialogScreen rdHabitData maybeHabit maybeInputtedAmount maybeSelectedYmd =
    div
        [ classList
            [ ( "set-habit-data-shortcut-amount-screen", True )
            , ( "display-none", activeDialogScreen /= Just DialogScreen.SetHabitDataShortcutAmountScreen )
            ]
        ]
        (case ( maybeHabit, maybeSelectedYmd, rdHabitData ) of
            ( Just habit, Just selectedYmd, RemoteData.Success habitData ) ->
                let
                    habitRecord =
                        Habit.getCommonFields habit

                    habitDatum =
                        List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == selectedYmd) habitData
                            |> List.head
                            |> (\maybeHabitDatum ->
                                    case maybeHabitDatum of
                                        Nothing ->
                                            0

                                        Just { amount } ->
                                            amount
                               )
                in
                [ span
                    [ class "set-habit-data-shortcut-amount-screen-selected-habit-name" ]
                    [ text <| .name habitRecord ]
                , input
                    [ id "set-habit-data-shortcut-amount-screen-input"
                    , class "set-habit-data-shortcut-amount-screen-input"
                    , placeholder <|
                        String.fromInt habitDatum
                            ++ " "
                            ++ (if habitDatum == 1 then
                                    habitRecord.unitNameSingular

                                else
                                    habitRecord.unitNamePlural
                               )
                    , onInput OnSetHabitDataShortcutAmountScreenInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt maybeInputtedAmount)
                    ]
                    []
                ]

            _ ->
                []
        )


renderEditGoalHabitSelectionScreen :
    Maybe DialogScreen.DialogScreen
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderEditGoalHabitSelectionScreen activeDialogScreen habitSelectionFilterText filteredHabits selectedHabitIndex =
    let
        showScreen =
            activeDialogScreen == Just DialogScreen.EditGoalHabitSelectionScreen
    in
    renderHabitSelectionScreen
        showScreen
        "Edit Goal"
        "edit-goal-habit-selection-filter-text-input"
        OnEditGoalHabitSelectionFilterTextInput
        habitSelectionFilterText
        filteredHabits
        selectedHabitIndex


renderEditGoalDialog :
    Maybe DialogScreen.DialogScreen
    -> Maybe Habit.Habit
    -> Maybe ( Int, Habit.FrequencyChangeRecord )
    -> Maybe String
    -> Maybe (List Habit.FrequencyChangeRecord)
    -> Habit.EditGoalInputData
    -> Html Msg
renderEditGoalDialog activeDialogScreen maybeHabit currentFcrWithIndex confirmationMessage newFrequencies editGoal =
    case maybeHabit of
        Just habit ->
            div
                [ classList
                    [ ( "edit-goal-dialog", True )
                    , ( "display-none", activeDialogScreen /= Just DialogScreen.EditGoalScreen )
                    ]
                ]
                (let
                    habitRecord =
                        Habit.getCommonFields habit

                    ( currentGoalTag, currentGoalDesc ) =
                        case currentFcrWithIndex of
                            Just ( currentIndex, currentFcr ) ->
                                ( case currentFcr.newFrequency of
                                    Habit.EveryXDayFrequency f ->
                                        "Y Per X Days"

                                    Habit.TotalWeekFrequency f ->
                                        "X Per Week"

                                    Habit.SpecificDayOfWeekFrequency f ->
                                        "Specific Days of Week"
                                , Habit.prettyPrintFrequency currentFcr.newFrequency habitRecord.unitNameSingular habitRecord.unitNamePlural
                                )

                            Nothing ->
                                ( "N/A", "No current goal." )

                    newGoalDesc : String
                    newGoalDesc =
                        case editGoal.frequencyKind of
                            Habit.TotalWeekFrequencyKind ->
                                Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.timesPerWeek)
                                    ++ " "
                                    ++ (if Maybe.withDefault 0 editGoal.timesPerWeek == 1 then
                                            habitRecord.unitNameSingular

                                        else
                                            habitRecord.unitNamePlural
                                       )
                                    ++ " per week"

                            Habit.SpecificDayOfWeekFrequencyKind ->
                                "Mo "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.mondayTimes)
                                    ++ " Tu "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.tuesdayTimes)
                                    ++ " We "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.wednesdayTimes)
                                    ++ " Th "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.thursdayTimes)
                                    ++ " Fr "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.fridayTimes)
                                    ++ " Sa "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.saturdayTimes)
                                    ++ " Su "
                                    ++ Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.sundayTimes)

                            Habit.EveryXDayFrequencyKind ->
                                Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.times)
                                    ++ " "
                                    ++ (if Maybe.withDefault 0 editGoal.times == 1 then
                                            habitRecord.unitNameSingular

                                        else
                                            habitRecord.unitNamePlural
                                       )
                                    ++ " per "
                                    ++ (if Maybe.withDefault 0 editGoal.days == 1 then
                                            "day"

                                        else
                                            Maybe.withDefault "_" (Maybe.map String.fromInt editGoal.days) ++ " days"
                                       )
                 in
                 [ div
                    [ class "edit-goal-dialog-form" ]
                    [ div
                        [ class "edit-goal-dialog-form-header" ]
                        [ text habitRecord.name ]
                    , div [ class "edit-goal-dialog-form-line-break" ] []
                    , div
                        [ class "edit-goal-dialog-form-current-goal-text" ]
                        [ text "Current Goal" ]
                    , div
                        [ class "edit-goal-dialog-form-current-goal-tag" ]
                        [ button [] [ text currentGoalTag ] ]
                    , div
                        [ class "edit-goal-dialog-form-current-goal-description" ]
                        [ text currentGoalDesc ]
                    , div
                        [ class "edit-goal-dialog-form-new-goal-line-break" ]
                        []
                    , div
                        [ class "edit-goal-dialog-form-new-goal-header" ]
                        [ text "New Goal" ]
                    , div
                        [ class "edit-goal-dialog-form-new-goal-frequency-tags" ]
                        [ button
                            [ classList [ ( "selected", editGoal.frequencyKind == Habit.TotalWeekFrequencyKind ) ]
                            , onClick <| OnEditGoalSelectFrequencyKind Habit.TotalWeekFrequencyKind
                            ]
                            [ text "X Per Week" ]
                        , button
                            [ classList [ ( "selected", editGoal.frequencyKind == Habit.SpecificDayOfWeekFrequencyKind ) ]
                            , onClick <| OnEditGoalSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind
                            ]
                            [ text "Specific Days of Week" ]
                        , button
                            [ classList [ ( "selected", editGoal.frequencyKind == Habit.EveryXDayFrequencyKind ) ]
                            , onClick <| OnEditGoalSelectFrequencyKind Habit.EveryXDayFrequencyKind
                            ]
                            [ text "Y Per X Days" ]
                        ]
                    , div
                        [ class "edit-goal-dialog-form-new-goal-description" ]
                        [ text newGoalDesc ]
                    , div
                        [ class "edit-goal-dialog-form-new-goal-forms" ]
                        [ div
                            [ classList
                                [ ( "edit-goal-dialog-form-new-goal-total-week-frequency-form", True )
                                , ( "display-none", editGoal.frequencyKind /= Habit.TotalWeekFrequencyKind )
                                ]
                            ]
                            [ input
                                [ placeholder <|
                                    if habitRecord.unitNamePlural == "" then
                                        "units"

                                    else
                                        habitRecord.unitNamePlural
                                , id "edit-goal-dialog-x-per-week-input"
                                , onInput OnEditGoalTimesPerWeekInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.timesPerWeek)
                                ]
                                []
                            ]
                        , div
                            [ classList
                                [ ( "edit-goal-dialog-form-new-goal-specific-day-of-week-frequency-form", True )
                                , ( "display-none", editGoal.frequencyKind /= Habit.SpecificDayOfWeekFrequencyKind )
                                ]
                            ]
                            [ input
                                [ placeholder "monday"
                                , id "edit-goal-dialog-monday-input"
                                , onInput OnEditGoalSpecificDayMondayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.mondayTimes)
                                ]
                                []
                            , input
                                [ placeholder "tuesday"
                                , onInput OnEditGoalSpecificDayTuesdayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.tuesdayTimes)
                                ]
                                []
                            , input
                                [ placeholder "wednesday"
                                , onInput OnEditGoalSpecificDayWednesdayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.wednesdayTimes)
                                ]
                                []
                            , input
                                [ placeholder "thursday"
                                , onInput OnEditGoalSpecificDayThursdayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.thursdayTimes)
                                ]
                                []
                            , input
                                [ placeholder "friday"
                                , onInput OnEditGoalSpecificDayFridayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.fridayTimes)
                                ]
                                []
                            , input
                                [ placeholder "saturday"
                                , onInput OnEditGoalSpecificDaySaturdayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.saturdayTimes)
                                ]
                                []
                            , input
                                [ placeholder "sunday"
                                , onInput OnEditGoalSpecificDaySundayInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.sundayTimes)
                                ]
                                []
                            ]
                        , div
                            [ classList
                                [ ( "edit-goal-dialog-form-new-goal-every-x-days-frequency-form", True )
                                , ( "display-none", editGoal.frequencyKind /= Habit.EveryXDayFrequencyKind )
                                ]
                            ]
                            [ input
                                [ placeholder <|
                                    if habitRecord.unitNamePlural == "" then
                                        "units"

                                    else
                                        habitRecord.unitNamePlural
                                , id "edit-goal-dialog-every-x-days-times-input"
                                , onInput OnEditGoalTimesInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.times)
                                ]
                                []
                            , input
                                [ placeholder "days"
                                , onInput OnEditGoalDaysInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.days)
                                ]
                                []
                            ]
                        ]
                    , div
                        [ classList
                            [ ( "edit-goal-dialog-form-confirmation-message-line-break", True )
                            , ( "display-none", not <| Maybe.isJust confirmationMessage )
                            ]
                        ]
                        []
                    , div
                        [ classList
                            [ ( "edit-goal-dialog-form-confirmation-message", True )
                            , ( "display-none", not <| Maybe.isJust confirmationMessage )
                            ]
                        ]
                        [ text <| Maybe.withDefault "" confirmationMessage ]
                    , div
                        [ classList
                            [ ( "edit-goal-dialog-form-buttons", True )
                            , ( "display-none", not <| Maybe.isJust newFrequencies )
                            ]
                        ]
                        [ button
                            [ class "edit-goal-dialog-form-buttons-submit"
                            , onClick OnEditGoalSubmit
                            ]
                            [ text "Submit" ]
                        , button
                            [ class "edit-goal-dialog-form-buttons-cancel"
                            , onClick OnExitDialogScreen
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                 ]
                )

        Nothing ->
            div [] []


renderEditInfoHabitSelectionScreen :
    Maybe DialogScreen.DialogScreen
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderEditInfoHabitSelectionScreen activeDialogScreen habitSelectionFilterText filteredHabits selectedHabitIndex =
    let
        showScreen =
            activeDialogScreen == Just DialogScreen.EditInfoHabitSelectionScreen
    in
    renderHabitSelectionScreen
        showScreen
        "Edit Info"
        "edit-info-habit-selection-filter-text-input"
        OnEditInfoHabitSelectionFilterTextInput
        habitSelectionFilterText
        filteredHabits
        selectedHabitIndex


renderEditInfoDialog :
    Maybe DialogScreen.DialogScreen
    -> Maybe Habit.Habit
    -> Habit.EditInfoInputData
    -> Html Msg
renderEditInfoDialog activeDialogScreen maybeHabit editInfo =
    case maybeHabit of
        Just habit ->
            let
                habitRecord =
                    Habit.getCommonFields habit

                isValidEditInfo =
                    Habit.isValidEditInfo editInfo habit
            in
            div
                [ classList
                    [ ( "edit-info-dialog", True )
                    , ( "display-none", activeDialogScreen /= Just DialogScreen.EditInfoScreen )
                    ]
                ]
                [ div
                    [ class "edit-info-dialog-form" ]
                    [ div [ class "edit-info-dialog-form-header" ] [ text habitRecord.name ]
                    , div [ class "edit-info-dialog-form-line-break" ] []
                    , div
                        [ class "edit-info-dialog-form-name" ]
                        [ div [ class "input-title" ] [ text "Name" ]
                        , input
                            [ id "edit-info-dialog-form-name-input"
                            , placeholder habitRecord.name
                            , value editInfo.name
                            , onInput OnEditInfoNameInput
                            ]
                            []
                        ]
                    , div
                        [ class "edit-info-dialog-form-description" ]
                        [ div [ class "input-title" ] [ text "Description" ]
                        , textarea
                            [ placeholder <| Maybe.withDefault "Description" habitRecord.description
                            , value editInfo.description
                            , onInput OnEditInfoDescriptionInput
                            ]
                            []
                        ]
                    , div
                        [ classList
                            [ ( "edit-info-dialog-form-time-of-day-tags", True )
                            , ( "display-none", not <| Habit.isGoodHabit habit )
                            ]
                        ]
                        [ button
                            [ classList [ ( "habit-time-of-day", True ), ( "selected", editInfo.goodHabitTime == Habit.Anytime ) ]
                            , onClick <| OnSelectEditInfoTimeOfDay Habit.Anytime
                            ]
                            [ text "ANYTIME" ]
                        , button
                            [ classList [ ( "habit-time-of-day", True ), ( "selected", editInfo.goodHabitTime == Habit.Morning ) ]
                            , onClick <| OnSelectEditInfoTimeOfDay Habit.Morning
                            ]
                            [ text "MORNING" ]
                        , button
                            [ classList [ ( "habit-time-of-day", True ), ( "selected", editInfo.goodHabitTime == Habit.Evening ) ]
                            , onClick <| OnSelectEditInfoTimeOfDay Habit.Evening
                            ]
                            [ text "EVENING" ]
                        ]
                    , div
                        [ class "edit-info-dialog-form-unit-name-singular" ]
                        [ div [ class "input-title" ] [ text "Unit Name (Singular)" ]
                        , input
                            [ placeholder habitRecord.unitNameSingular
                            , value editInfo.unitNameSingular
                            , onInput OnEditInfoUnitNameSingularInput
                            ]
                            []
                        ]
                    , div
                        [ class "edit-info-dialog-form-unit-name-plural" ]
                        [ div [ class "input-title" ] [ text "Unit Name (Plural)" ]
                        , input
                            [ placeholder habitRecord.unitNamePlural
                            , value editInfo.unitNamePlural
                            , onInput OnEditInfoUnitNamePluralInput
                            ]
                            []
                        ]
                    , div
                        [ classList
                            [ ( "edit-info-dialog-form-buttons-line-break", True )
                            , ( "display-none", not isValidEditInfo )
                            ]
                        ]
                        []
                    , div
                        [ classList
                            [ ( "edit-info-dialog-form-buttons", True )
                            , ( "display-none", not isValidEditInfo )
                            ]
                        ]
                        [ button
                            [ class "edit-info-dialog-form-buttons-submit"
                            , onClick OnEditInfoSubmitClick
                            ]
                            [ text "Submit" ]
                        , button
                            [ class "edit-info-dialog-form-buttons-cancel"
                            , onClick OnExitDialogScreen
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

        Nothing ->
            Util.hiddenDiv


renderErrorMessage : Maybe String -> Maybe DialogScreen.DialogScreen -> Html Msg
renderErrorMessage errorMessage activeDialogScreen =
    div
        [ classList
            [ ( "error-message", True )
            , ( "display-none", activeDialogScreen /= Just DialogScreen.ErrorMessageScreen )
            ]
        ]
        [ div
            [ class "error-message-text" ]
            (case errorMessage of
                Just errorMessageStr ->
                    [ div [ class "error-message-text-line" ] [ text errorMessageStr ]
                    , div [ class "error-message-text-line" ] [ text "You may want to refresh the page." ]
                    ]

                Nothing ->
                    [ div [ class "error-message-text-line" ] [ text "No errors" ] ]
            )
        ]


renderAddNoteHabitSelectionScreen :
    Maybe DialogScreen.DialogScreen
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderAddNoteHabitSelectionScreen activeDialogScreen habitSelectionFilterText filteredHabits selectedHabitIndex =
    let
        showScreen =
            activeDialogScreen == Just DialogScreen.AddNoteHabitSelectionScreen
    in
    renderHabitSelectionScreen
        showScreen
        "Add Note"
        "add-note-habit-selection-filter-text-input"
        OnAddNoteHabitSelectionFilterTextInput
        habitSelectionFilterText
        filteredHabits
        selectedHabitIndex


renderAddNoteDialog :
    Maybe DialogScreen.DialogScreen
    -> Maybe Habit.Habit
    -> String
    -> Maybe YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List HabitDayNote.HabitDayNote)
    -> Html Msg
renderAddNoteDialog activeDialogScreen addNoteDialogHabit addNoteDialogInput maybeSelectedYmd rdAllHabitDayNotes =
    div
        [ classList
            [ ( "add-note-dialog", True )
            , ( "display-none", activeDialogScreen /= Just DialogScreen.AddNoteScreen )
            ]
        ]
        (case ( addNoteDialogHabit, maybeSelectedYmd, rdAllHabitDayNotes ) of
            ( Just habit, Just selectedYmd, RemoteData.Success allHabitDayNotes ) ->
                let
                    habitRecord =
                        Habit.getCommonFields habit

                    existingHabitDayNoteText : Maybe String
                    existingHabitDayNoteText =
                        List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == selectedYmd) allHabitDayNotes
                            |> List.head
                            |> Maybe.map .note
                in
                [ div
                    [ class "add-note-dialog-form" ]
                    [ div [ class "add-note-dialog-header-habit-name" ] [ text habitRecord.name ]
                    , div [ class "add-note-dialog-header-date" ] [ text <| YmdDate.prettyPrintWithWeekday selectedYmd ]
                    , div
                        [ classList
                            [ ( "add-note-dialog-header-note-added", True )
                            , ( "display-none", existingHabitDayNoteText /= Just addNoteDialogInput )
                            ]
                        ]
                        [ i [ class "material-icons" ] []
                        , text "Note Added"
                        ]
                    , div [ class "add-note-dialog-header-line-break" ] []
                    , textarea
                        [ class "add-note-dialog-input"
                        , placeholder <| Maybe.withDefault "Add a note for today..." existingHabitDayNoteText
                        , onInput OnAddNoteDialogInput
                        , value addNoteDialogInput
                        , id "add-note-dialog-input-id"
                        ]
                        []
                    , div
                        [ classList
                            [ ( "add-note-dialog-form-buttons", True )
                            , ( "display-none", addNoteDialogInput == "" )
                            ]
                        ]
                        [ button
                            [ class "add-note-dialog-form-buttons-submit"
                            , onClick OnAddNoteSubmit
                            ]
                            [ text "Submit" ]
                        , button
                            [ class "add-note-dialog-form-buttons-cancel"
                            , onClick OnExitDialogScreen
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

            _ ->
                []
        )


renderSuspendOrResumeHabitSelectionScreen :
    Maybe DialogScreen.DialogScreen
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderSuspendOrResumeHabitSelectionScreen activeDialogScreen habitSelectionFilterText filteredHabits selectedHabitIndex =
    let
        showScreen =
            activeDialogScreen == Just DialogScreen.SuspendOrResumeHabitSelectionScreen
    in
    renderHabitSelectionScreen
        showScreen
        "Suspend Or Resume Habit"
        "suspend-or-resume-habit-selection-filter-text-input"
        OnSuspendOrResumeHabitSelectionFilterTextInput
        habitSelectionFilterText
        filteredHabits
        selectedHabitIndex


renderSuspendOrResumeConfirmationScreen :
    Maybe DialogScreen.DialogScreen
    -> Maybe Habit.Habit
    -> Maybe YmdDate.YmdDate
    -> String
    -> Maybe (List Habit.SuspendedInterval)
    -> Html Msg
renderSuspendOrResumeConfirmationScreen activeDialogScreen maybeHabit maybeSelectedYmd confirmationMessage maybeNewSuspensions =
    div
        [ classList
            [ ( "suspend-or-resume-confirmation-screen", True )
            , ( "display-none", activeDialogScreen /= Just DialogScreen.SuspendOrResumeConfirmationScreen )
            ]
        ]
        (case ( maybeHabit, maybeSelectedYmd, maybeNewSuspensions ) of
            ( Just habit, Just selectedYmd, Just newSuspensions ) ->
                let
                    habitRecord =
                        Habit.getCommonFields habit
                in
                [ div
                    [ class "suspend-or-resume-confirmation-screen-dialog" ]
                    [ div [ class "suspend-or-resume-confirmation-screen-dialog-header-habit-name" ] [ text habitRecord.name ]
                    , div
                        [ class "suspend-or-resume-confirmation-screen-dialog-header-date" ]
                        [ text <| YmdDate.prettyPrintWithWeekday selectedYmd ]
                    , div [ class "suspend-or-resume-confirmation-screen-dialog-header-line-break" ] []
                    , div [ class "suspend-or-resume-confirmation-screen-dialog-confirmation-message" ] [ text confirmationMessage ]
                    , div
                        [ class "suspend-or-resume-confirmation-screen-dialog-form-buttons" ]
                        [ button
                            [ class "suspend-or-resume-confirmation-screen-dialog-form-buttons-submit"
                            , onClick OnResumeOrSuspendSubmitClick
                            ]
                            [ text "Confirm" ]
                        , button
                            [ class "suspend-or-resume-confirmation-screen-dialog-form-buttons-cancel"
                            , onClick OnExitDialogScreen
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

            _ ->
                []
        )


renderGraphHabitSelectionScreen :
    Maybe DialogScreen.DialogScreen
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Html Msg
renderGraphHabitSelectionScreen activeDialogScreen habitSelectionFilterText filteredHabits selectedHabitIndex =
    let
        showScreen =
            activeDialogScreen == Just DialogScreen.GraphHabitSelectionScreen
    in
    renderHabitSelectionScreen
        showScreen
        "Graph Habit"
        "graph-habit-selection-filter-text-input"
        OnGraphHabitSelectionFilterTextInput
        habitSelectionFilterText
        filteredHabits
        selectedHabitIndex


renderGraphDialogScreen :
    Maybe DialogScreen.DialogScreen
    -> Maybe Habit.Habit
    -> Maybe YmdDate.YmdDate
    -> Graph.NumberOfDaysToShow
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitDayNote.HabitDayNote)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitGoalIntervalList.HabitGoalInterval)
    -> RemoteData.RemoteData ApiError.ApiError (List ( Graph.IntervalSuccessStatus, Graph.GraphData ))
    -> Bool
    -> Maybe Graph.Point
    -> Html Msg
renderGraphDialogScreen activeDialogScreen maybeHabit maybeSelectedYmd numDaysToShow rdHabitData rdNotes rdGoalIntervals rdGraphIntervalsData darkModeOn maybeHoveredPoint =
    div
        [ classList
            [ ( "graph-screen", True )
            , ( "display-none", activeDialogScreen /= Just DialogScreen.GraphDialogScreen )
            ]
        ]
        (case ( maybeHabit, maybeSelectedYmd ) of
            ( Just habit, Just selectedYmd ) ->
                let
                    habitRecord =
                        Habit.getCommonFields habit

                    numDaysButtonText numDays =
                        case numDays of
                            Graph.AllTime ->
                                "All Time"

                            Graph.LastMonth ->
                                "Last Month"

                            Graph.LastThreeMonths ->
                                "Last Three Months"

                            Graph.LastYear ->
                                "Last Year"

                    changeNumDaysToShowButton : Graph.NumberOfDaysToShow -> Html Msg
                    changeNumDaysToShowButton numDays =
                        button
                            [ classList
                                [ ( "graph-screen-dialog-change-numdays-button", True )
                                , ( "selected", numDaysToShow == numDays )
                                ]
                            , onClick <| SetGraphNumDaysToShow numDays
                            ]
                            [ text <| numDaysButtonText numDays ]
                in
                [ div
                    [ class "graph-screen-dialog" ]
                    [ div [ class "graph-screen-dialog-header-habit-name" ] [ text habitRecord.name ]
                    , div
                        [ class "graph-screen-dialog-header-date" ]
                        [ text <| YmdDate.prettyPrintWithWeekday selectedYmd ]
                    , div [ class "graph-screen-dialog-header-line-break" ] []
                    , div
                        [ class "graph-screen-dialog-change-numdays-buttons" ]
                        [ changeNumDaysToShowButton Graph.LastMonth
                        , changeNumDaysToShowButton Graph.LastThreeMonths
                        , changeNumDaysToShowButton Graph.LastYear
                        , changeNumDaysToShowButton Graph.AllTime
                        ]
                    , case ( rdGoalIntervals, rdGraphIntervalsData, ( rdHabitData, rdNotes ) ) of
                        ( RemoteData.Loading, _, _ ) ->
                            div [ class "graph-screen-dialog-graph-container-empty" ] [ text "Loading..." ]

                        ( RemoteData.Success graphGoalIntervals, RemoteData.Success graphIntervalsData, ( RemoteData.Success allData, RemoteData.Success allNotes ) ) ->
                            let
                                graphCustomConfig =
                                    Graph.customConfig graphGoalIntervals darkModeOn allData allNotes habitRecord.id maybeHoveredPoint OnGraphPointHover
                            in
                            div
                                [ class "graph-screen-dialog-graph-container" ]
                                [ LineChart.viewCustom
                                    graphCustomConfig
                                    (List.map
                                        (Graph.intervalGraphDataToLine habit darkModeOn)
                                        graphIntervalsData
                                    )
                                ]

                        _ ->
                            div [ class "graph-screen-dialog-graph-container-empty" ] [ text "Failure..." ]
                    ]
                ]

            _ ->
                []
        )
