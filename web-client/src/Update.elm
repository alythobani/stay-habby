module Update exposing (extractInt, update)

import Api
import Array
import Browser.Dom as Dom
import Date
import DefaultServices.Keyboard as Keyboard
import DefaultServices.Util as Util
import Dict
import Maybe.Extra as Maybe
import Model exposing (Model)
import Models.ApiError as ApiError
import Models.DialogScreen as DialogScreen
import Models.FrequencyStats as FrequencyStats
import Models.Graph as Graph
import Models.Habit as Habit
import Models.HabitGoalIntervalList as HabitGoalIntervalList
import Models.KeyboardShortcut as KeyboardShortcut
import Models.Login as Login
import Models.User as User
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData
import Set
import Task
import TimeZone


{-| When we switch the active screen, we should reset certain model fields, and update
the active keyboard shortcuts list.
-}
switchScreen : Model -> Maybe DialogScreen.DialogScreen -> Model
switchScreen m newScreen =
    let
        newKeyboardShortcuts =
            case newScreen of
                Just DialogScreen.AddNewHabitScreen ->
                    KeyboardShortcut.addNewHabitScreenShortcuts

                Just DialogScreen.EditGoalHabitSelectionScreen ->
                    KeyboardShortcut.editGoalHabitSelectionShortcuts

                Just DialogScreen.EditGoalScreen ->
                    KeyboardShortcut.editGoalScreenShortcuts

                Just DialogScreen.SetHabitDataShortcutHabitSelectionScreen ->
                    KeyboardShortcut.setHabitDataHabitSelectionShortcuts

                Just DialogScreen.SetHabitDataShortcutAmountScreen ->
                    KeyboardShortcut.setHabitDataAmountScreenShortcuts

                Just DialogScreen.ErrorMessageScreen ->
                    KeyboardShortcut.errorMessageScreenShortcuts

                Just DialogScreen.AddNoteHabitSelectionScreen ->
                    KeyboardShortcut.addNoteHabitSelectionShortcuts

                Just DialogScreen.AddNoteScreen ->
                    KeyboardShortcut.addNoteScreenShortcuts

                Just DialogScreen.ChooseDateDialogScreen ->
                    KeyboardShortcut.chooseDateScreenShortcuts

                Just DialogScreen.SuspendOrResumeHabitSelectionScreen ->
                    KeyboardShortcut.suspendOrResumeHabitSelectionShortcuts

                Just DialogScreen.SuspendOrResumeConfirmationScreen ->
                    KeyboardShortcut.suspendOrResumeConfirmationScreenShortcuts

                Just DialogScreen.GraphHabitSelectionScreen ->
                    KeyboardShortcut.graphHabitSelectionShortcuts

                Just DialogScreen.GraphDialogScreen ->
                    KeyboardShortcut.graphScreenShortcuts

                Nothing ->
                    KeyboardShortcut.mainScreenShortcuts

        resettedFilteredHabits =
            case m.allHabits of
                RemoteData.Success habits ->
                    Array.fromList habits

                _ ->
                    Array.empty
    in
    { m
        | activeDialogScreen = newScreen
        , keyboardShortcutsList = newKeyboardShortcuts
        , openTopPanelDateDropdown = False
        , openUserActionsDropdown = False
        , habitActionsDropdown = Nothing
        , setHabitDataShortcutAmountScreenInputInt = Nothing
        , setHabitDataShortcutHabitNameFilterText = ""
        , setHabitDataShortcutFilteredHabits = resettedFilteredHabits
        , setHabitDataShortcutSelectedHabitIndex = 0
        , addNoteHabitSelectionFilterText = ""
        , addNoteHabitSelectionFilteredHabits = resettedFilteredHabits
        , addNoteHabitSelectionSelectedHabitIndex = 0
        , editGoalHabitSelectionFilterText = ""
        , editGoalHabitSelectionFilteredHabits = resettedFilteredHabits
        , editGoalHabitSelectionSelectedHabitIndex = 0
        , graphNumDaysToShow = Graph.LastMonth
        , graphHabitSelectionFilterText = ""
        , graphHabitSelectionFilteredHabits = resettedFilteredHabits
        , graphHabitSelectionSelectedHabitIndex = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddHabit updater =
            { model | addHabit = updater model.addHabit }

        updateEditGoal updater =
            { model | editGoal = updater model.editGoal }

        updateLoginPageFields updater =
            { model | loginPageFields = updater model.loginPageFields }

        updateHabitListsWithNewHabit : Habit.Habit -> Model
        updateHabitListsWithNewHabit habit =
            let
                replaceHabitInList habitList =
                    Util.replaceOrAdd
                        habitList
                        (\oldHabit -> (oldHabit |> Habit.getCommonFields |> .id) == (habit |> Habit.getCommonFields |> .id))
                        habit

                replaceHabitInArray habitArray =
                    habitArray |> Array.toList |> replaceHabitInList |> Array.fromList
            in
            { model
                | allHabits =
                    RemoteData.map
                        replaceHabitInList
                        model.allHabits
                , setHabitDataShortcutFilteredHabits = replaceHabitInArray model.setHabitDataShortcutFilteredHabits
                , editGoalHabitSelectionFilteredHabits = replaceHabitInArray model.editGoalHabitSelectionFilteredHabits
                , addNoteHabitSelectionFilteredHabits = replaceHabitInArray model.addNoteHabitSelectionFilteredHabits
                , suspendOrResumeHabitSelectionFilteredHabits = replaceHabitInArray model.suspendOrResumeHabitSelectionFilteredHabits
            }

        getFrequencyStatsOnDate : User.User -> YmdDate.YmdDate -> List String -> Cmd Msg
        getFrequencyStatsOnDate user ymd habitIds =
            Api.queryFrequencyStats
                user
                ymd
                habitIds
                model.apiBaseUrl
                OnGetFrequencyStatsFailure
                OnGetFrequencyStatsSuccess

        getFrequencyStats : User.User -> List String -> Cmd Msg
        getFrequencyStats user habitIds =
            case model.selectedYmd of
                Just ymd ->
                    getFrequencyStatsOnDate user ymd habitIds

                Nothing ->
                    Cmd.none

        getGraphHabitGoalIntervalList : User.User -> Habit.Habit -> Graph.NumberOfDaysToShow -> YmdDate.YmdDate -> Cmd Msg
        getGraphHabitGoalIntervalList user graphHabit numDaysToShow endYmd =
            let
                habitIds =
                    [ graphHabit |> Habit.getCommonFields |> .id ]

                maybeStartYmd =
                    case numDaysToShow of
                        Graph.AllTime ->
                            Nothing

                        Graph.LastMonth ->
                            Just <| YmdDate.addMonths -1 endYmd

                        Graph.LastThreeMonths ->
                            Just <| YmdDate.addMonths -3 endYmd

                        Graph.LastYear ->
                            Just <| YmdDate.addYears -1 endYmd
            in
            Api.queryHabitGoalIntervalLists
                user
                maybeStartYmd
                endYmd
                habitIds
                model.apiBaseUrl
                OnGetGraphHabitGoalIntervalListFailure
                OnGetGraphHabitGoalIntervalListSuccess

        updateOnHabitSelectionChangeSelectedHabitIndex :
            Array.Array Habit.Habit
            -> Int
            -> (Int -> Model)
            -> ( Model, Cmd Msg )
        updateOnHabitSelectionChangeSelectedHabitIndex filteredHabits newAttemptedIndex modelUpdater =
            let
                filteredHabitsLength =
                    Array.length filteredHabits

                newSelectedHabitIndex =
                    modBy filteredHabitsLength newAttemptedIndex
            in
            ( modelUpdater newSelectedHabitIndex
            , Cmd.none
            )

        updateHabitSelectionFilterTextInput :
            String
            -> Int
            -> Array.Array Habit.Habit
            -> (Array.Array Habit.Habit -> Int -> Model)
            -> ( Model, Cmd Msg )
        updateHabitSelectionFilterTextInput newFilterText oldIndex oldFilteredHabits modelUpdater =
            let
                habitFilter habit =
                    habit |> Habit.getCommonFields |> .name |> String.contains newFilterText

                newFilteredHabits =
                    case model.allHabits of
                        RemoteData.Success habits ->
                            List.filter habitFilter habits

                        _ ->
                            []

                oldSelectedHabit =
                    Array.get oldIndex oldFilteredHabits

                newSelectedHabitIndex =
                    case oldSelectedHabit of
                        Just h ->
                            Maybe.map Tuple.first (Util.firstInstanceInList newFilteredHabits ((==) h))
                                |> Maybe.withDefault 0

                        Nothing ->
                            0

                newFilteredHabitsArray =
                    Array.fromList newFilteredHabits
            in
            ( modelUpdater newFilteredHabitsArray newSelectedHabitIndex
            , Cmd.none
            )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnInitialTimeZoneRetrieval result ->
            -- Called when the app first starts up and we attempt to retrieve the local time zone
            case result of
                Result.Err error ->
                    ( { model | errorMessage = Just <| "Time Zone error: " ++ Debug.toString error }, Cmd.none )

                Ok ( timeZoneName, timeZone ) ->
                    let
                        currentDate : Date.Date
                        currentDate =
                            Date.fromPosix timeZone model.currentPosix

                        currentYmd : YmdDate.YmdDate
                        currentYmd =
                            YmdDate.fromDate currentDate

                        newModel =
                            { model
                                | currentTimeZone = Just timeZone
                                , selectedYmd = Just currentYmd
                                , actualYmd = Just currentYmd
                                , chooseDateDialogChosenYmd = Just currentYmd
                            }
                    in
                    case model.user of
                        Just user ->
                            ( { newModel
                                | allHabits = RemoteData.Loading
                                , allHabitData = RemoteData.Loading
                                , allFrequencyStats = RemoteData.Loading
                                , allHabitDayNotes = RemoteData.Loading
                              }
                            , Api.queryAllRemoteData
                                user
                                currentYmd
                                model.apiBaseUrl
                                OnGetAllRemoteDataFailure
                                OnGetAllRemoteDataSuccess
                            )

                        Nothing ->
                            ( newModel, Cmd.none )

        OnUrlChange url ->
            -- TODO
            ( model, Cmd.none )

        OnUrlRequest urlRequest ->
            -- TODO
            ( model, Cmd.none )

        -- Authentication
        OnClickChooseLoginFormButton ->
            let
                newFormModel =
                    updateLoginPageFields
                        (\loginPageFields ->
                            { loginPageFields | loginOrCreateUserForm = Login.LoginForm }
                        )
            in
            ( { newFormModel | keyboardShortcutsList = KeyboardShortcut.loginFormShortcuts }
            , Dom.focus "login-form-username-input"
                |> Task.attempt FocusResult
            )

        OnClickChooseCreateUserFormButton ->
            let
                newFormModel =
                    updateLoginPageFields
                        (\loginPageFields ->
                            { loginPageFields | loginOrCreateUserForm = Login.CreateUserForm }
                        )
            in
            ( { newFormModel | keyboardShortcutsList = KeyboardShortcut.createUserFormShortcuts }
            , Dom.focus "create-user-form-username-input"
                |> Task.attempt FocusResult
            )

        OnLoginFormUsernameInput newUsernameInput ->
            ( updateLoginPageFields (\loginPageFields -> { loginPageFields | loginFormUsername = newUsernameInput })
            , Cmd.none
            )

        OnLoginFormPasswordInput newPasswordInput ->
            ( updateLoginPageFields (\loginPageFields -> { loginPageFields | loginFormPassword = newPasswordInput })
            , Cmd.none
            )

        OnLoginFormEnterKeydown ->
            case ( model.loginPageFields.loginFormUsername, model.loginPageFields.loginFormPassword ) of
                ( "", _ ) ->
                    -- No username entered, do nothing
                    ( model, Cmd.none )

                ( _, "" ) ->
                    -- No password entered, do nothing
                    ( model, Cmd.none )

                _ ->
                    -- Attempt to login
                    update OnLoginUserClick model

        OnLoginUserClick ->
            ( model
            , Api.queryLoginUser
                model.loginPageFields.loginFormUsername
                model.loginPageFields.loginFormPassword
                model.apiBaseUrl
                OnLoginUserGraphqlFailure
                OnLoginUserGraphqlSuccess
            )

        OnLoginUserGraphqlFailure apiError ->
            ( updateLoginPageFields
                (\loginPageFields ->
                    { loginPageFields
                        | loginErrorMessage =
                            Just <|
                                "Error logging in: "
                                    ++ ApiError.toString apiError
                                    ++ ". You may want to refresh the page."
                    }
                )
            , Cmd.none
            )

        OnLoginUserGraphqlSuccess queriedUser ->
            case queriedUser.maybeUser of
                Just user ->
                    -- Successful login
                    ( { model
                        | user = Just user
                        , loginPageFields = Login.initLoginPageFields
                        , keyboardShortcutsList = KeyboardShortcut.mainScreenShortcuts
                      }
                    , case model.selectedYmd of
                        Just selectedYmd ->
                            Api.queryAllRemoteData
                                user
                                selectedYmd
                                model.apiBaseUrl
                                OnGetAllRemoteDataFailure
                                OnGetAllRemoteDataSuccess

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    -- Didn't pass validation on server side; username or password was incorrect.
                    ( updateLoginPageFields
                        (\loginPageFields ->
                            { loginPageFields | loginErrorMessage = Just "Invalid username or password." }
                        )
                    , Cmd.none
                    )

        OnCreateUserFormUsernameInput newUsernameInput ->
            ( updateLoginPageFields
                (\loginPageFields ->
                    { loginPageFields
                        | createUserFormUsername = newUsernameInput
                        , doesCreateUserFormUsernameHaveAtLeast1Character = not <| String.isEmpty newUsernameInput
                    }
                )
            , Cmd.none
            )

        OnCreateUserFormDisplayNameInput newDisplayNameInput ->
            ( updateLoginPageFields
                (\loginPageFields ->
                    { loginPageFields
                        | createUserFormDisplayName = newDisplayNameInput
                        , doesCreateUserFormDisplayNameHaveAtLeast1Character = not <| String.isEmpty newDisplayNameInput
                    }
                )
            , Cmd.none
            )

        OnCreateUserFormEmailAddressInput newEmailAddressInput ->
            -- https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Input_Validation_Cheat_Sheet.md#Email_Address_Validation
            -- https://emailregex.com/email-validation-summary/ also informative
            let
                lastIndexOfAtSymbolAppearance : Maybe Int
                lastIndexOfAtSymbolAppearance =
                    Util.lastElementOfList <| String.indexes "@" newEmailAddressInput

                ( localPortionOfAddress, domainPortionOfAddress ) =
                    case lastIndexOfAtSymbolAppearance of
                        Just lastIndex ->
                            ( String.left lastIndex newEmailAddressInput
                            , String.right (String.length newEmailAddressInput - lastIndex) newEmailAddressInput
                            )

                        Nothing ->
                            -- Doesn't matter at this point since the email is invalid anyway
                            ( "", "" )

                isNewEmailAddressValid =
                    Maybe.isJust lastIndexOfAtSymbolAppearance
                        && (String.length localPortionOfAddress <= 64)
                        && (String.length domainPortionOfAddress <= 256)
            in
            if String.length newEmailAddressInput > 320 || String.contains "<script" newEmailAddressInput then
                -- Don't allow user to increase length of input past maximum length. And avoid a sketchy email.
                ( model, Cmd.none )

            else
                ( updateLoginPageFields
                    (\loginPageFields ->
                        { loginPageFields
                            | createUserFormEmailAddress = newEmailAddressInput
                            , isCreateUserFormEmailAddressValid = isNewEmailAddressValid
                        }
                    )
                , Cmd.none
                )

        OnCreateUserFormPasswordInput newPasswordInput ->
            ( updateLoginPageFields
                (\loginPageFields ->
                    { loginPageFields
                        | createUserFormPassword = newPasswordInput
                        , doesCreateUserFormPasswordHaveAtLeast10Characters = String.length newPasswordInput >= 10
                        , doesCreateUserFormPasswordHaveAtMost128Characters = String.length newPasswordInput <= 128
                        , doesCreateUserFormPasswordHaveALowerCaseCharacter = String.any Char.isLower newPasswordInput
                        , doesCreateUserFormPasswordHaveAnUpperCaseCharacter = String.any Char.isUpper newPasswordInput
                        , doesCreateUserFormPasswordHaveADigit = String.any Char.isDigit newPasswordInput
                        , doesCreateUserFormRepeatPasswordMatch = newPasswordInput == model.loginPageFields.createUserFormRepeatPassword
                    }
                )
            , Cmd.none
            )

        OnCreateUserFormRepeatPasswordInput newRepeatPasswordInput ->
            ( updateLoginPageFields
                (\loginPageFields ->
                    { loginPageFields
                        | createUserFormRepeatPassword = newRepeatPasswordInput
                        , doesCreateUserFormRepeatPasswordMatch = newRepeatPasswordInput == model.loginPageFields.createUserFormPassword
                    }
                )
            , Cmd.none
            )

        OnCreateUserFormEnterKeydown ->
            case Login.extractCreateUserFields model.loginPageFields of
                Just createUserFields ->
                    update (OnSignUpUserClick createUserFields) model

                _ ->
                    -- User hasn't filled out fields properly yet, do nothing
                    ( model, Cmd.none )

        OnSignUpUserClick createUserFields ->
            ( model
            , Api.mutationAddUser
                createUserFields
                model.apiBaseUrl
                OnSignUpUserGraphqlFailure
                OnSignUpUserGraphqlSuccess
            )

        OnSignUpUserGraphqlFailure apiError ->
            ( updateLoginPageFields
                (\loginPageFields ->
                    { loginPageFields
                        | signUpErrorMessage =
                            Just <|
                                "Error signing up: "
                                    ++ ApiError.toString apiError
                                    ++ ". You may want to refresh the page."
                    }
                )
            , Cmd.none
            )

        OnSignUpUserGraphqlSuccess maybeUser ->
            case maybeUser of
                Just user ->
                    -- Successful sign up
                    ( { model
                        | user = Just user
                        , loginPageFields = Login.initLoginPageFields
                        , keyboardShortcutsList = KeyboardShortcut.mainScreenShortcuts
                      }
                    , case model.selectedYmd of
                        Just selectedYmd ->
                            Api.queryAllRemoteData
                                user
                                selectedYmd
                                model.apiBaseUrl
                                OnGetAllRemoteDataFailure
                                OnGetAllRemoteDataSuccess

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    -- Didn't pass validation on server side; username or email was already taken.
                    ( updateLoginPageFields
                        (\loginPageFields ->
                            { loginPageFields | signUpErrorMessage = Just "Error signing up: username or email was already taken." }
                        )
                    , Cmd.none
                    )

        -- Time / Date
        TickMinute posix ->
            ( { model | currentPosix = posix }
            , Task.attempt OnTimeZoneRetrieval TimeZone.getZone
            )

        OnTimeZoneRetrieval result ->
            -- Called every minute when we update the local time zone
            case result of
                Result.Err error ->
                    ( { model | errorMessage = Just <| "Time Zone error: " ++ Debug.toString error }, Cmd.none )

                Ok ( timeZoneName, timeZone ) ->
                    let
                        currentDate : Date.Date
                        currentDate =
                            Date.fromPosix timeZone model.currentPosix

                        currentYmd : YmdDate.YmdDate
                        currentYmd =
                            YmdDate.fromDate currentDate
                    in
                    ( { model | actualYmd = Just currentYmd }, Cmd.none )

        -- Top Panel Date
        ToggleTopPanelDateDropdown ->
            ( { model
                | openTopPanelDateDropdown = not model.openTopPanelDateDropdown
                , openUserActionsDropdown = False
                , habitActionsDropdown = Nothing
              }
            , Cmd.none
            )

        ChangeSelectedYmd newYmd ->
            if model.selectedYmd == Just newYmd then
                -- Already on desired date, no need to re-query any data
                ( model, Cmd.none )

            else
                let
                    newModel =
                        { model | selectedYmd = Just newYmd }
                in
                case model.user of
                    Just user ->
                        ( { newModel | allFrequencyStats = RemoteData.Loading }
                        , getFrequencyStatsOnDate user newYmd []
                        )

                    Nothing ->
                        ( newModel, Cmd.none )

        SetSelectedDateToXDaysFromToday numDaysToAdd ->
            let
                -- Close the date dropdown
                newDateDropdownModel =
                    { model | openTopPanelDateDropdown = False }
            in
            case model.actualYmd of
                Just today ->
                    let
                        newSelectedYmd =
                            YmdDate.addDays numDaysToAdd today
                    in
                    update (ChangeSelectedYmd newSelectedYmd) newDateDropdownModel

                Nothing ->
                    ( { newDateDropdownModel | errorMessage = Just "Error setting date: current date not available" }, Cmd.none )

        OpenChooseCustomDateDialog ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.ChooseDateDialogScreen)
            in
            ( { newDialogScreenModel
                | chooseDateDialogChosenYmd = model.selectedYmd
              }
            , Dom.focus "choose-date-dialog-form-id"
                |> Task.attempt FocusResult
            )

        OnChooseDateDialogPreviousMonthClick chosenYmd ->
            let
                newYmd =
                    YmdDate.addMonths -1 chosenYmd
            in
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        OnChooseDateDialogNextMonthClick chosenYmd ->
            let
                newYmd =
                    YmdDate.addMonths 1 chosenYmd
            in
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        OnChooseDateDialogPreviousDayClick chosenYmd ->
            let
                newYmd =
                    YmdDate.addDays -1 chosenYmd
            in
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        OnChooseDateDialogNextDayClick chosenYmd ->
            let
                newYmd =
                    YmdDate.addDays 1 chosenYmd
            in
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        OnChooseDateDialogPreviousYearClick chosenYmd ->
            let
                newYmd =
                    YmdDate.addYears -1 chosenYmd
            in
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        OnChooseDateDialogNextYearClick chosenYmd ->
            let
                newYmd =
                    YmdDate.addYears 1 chosenYmd
            in
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        SetChooseDateDialogChosenYmd newYmd ->
            ( { model | chooseDateDialogChosenYmd = Just newYmd }, Cmd.none )

        SetChooseDateDialogChosenYmdToToday ->
            case model.actualYmd of
                Just actualYmd ->
                    update (SetChooseDateDialogChosenYmd actualYmd) model

                Nothing ->
                    ( model, Cmd.none )

        OnChooseDateDialogArrowDown ->
            case model.chooseDateDialogChosenYmd of
                Just chosenYmd ->
                    update (SetChooseDateDialogChosenYmd (YmdDate.addDays 7 chosenYmd)) model

                Nothing ->
                    ( model, Cmd.none )

        OnChooseDateDialogArrowUp ->
            case model.chooseDateDialogChosenYmd of
                Just chosenYmd ->
                    update (SetChooseDateDialogChosenYmd (YmdDate.addDays -7 chosenYmd)) model

                Nothing ->
                    ( model, Cmd.none )

        OnChooseDateDialogArrowLeft ->
            case model.chooseDateDialogChosenYmd of
                Just chosenYmd ->
                    update (SetChooseDateDialogChosenYmd (YmdDate.addDays -1 chosenYmd)) model

                Nothing ->
                    ( model, Cmd.none )

        OnChooseDateDialogArrowRight ->
            case model.chooseDateDialogChosenYmd of
                Just chosenYmd ->
                    update (SetChooseDateDialogChosenYmd (YmdDate.addDays 1 chosenYmd)) model

                Nothing ->
                    ( model, Cmd.none )

        OnChooseDateDialogSubmitClick ->
            case model.chooseDateDialogChosenYmd of
                Just chosenYmd ->
                    let
                        newDialogScreenModel =
                            switchScreen model Nothing
                    in
                    update (ChangeSelectedYmd chosenYmd) newDialogScreenModel

                Nothing ->
                    ( model, Cmd.none )

        -- Top Panel User Actions Dropdown
        ToggleTopPanelUserActionsDropdown ->
            ( { model
                | openUserActionsDropdown = not model.openUserActionsDropdown
                , openTopPanelDateDropdown = False
                , habitActionsDropdown = Nothing
              }
            , Cmd.none
            )

        OnLogoutUserClick ->
            ( { model
                | user = Nothing
                , openUserActionsDropdown = False
                , keyboardShortcutsList = KeyboardShortcut.loginFormShortcuts
              }
            , Dom.focus "login-form-username-input" |> Task.attempt FocusResult
            )

        -- All Habit Data
        OnGetAllRemoteDataFailure apiError ->
            ( { model
                | allHabits = RemoteData.Failure apiError
                , allHabitData = RemoteData.Failure apiError
                , allFrequencyStats = RemoteData.Failure apiError
                , allHabitDayNotes = RemoteData.Failure apiError
                , errorMessage = Just <| "Error retrieving habits / data / stats: " ++ ApiError.toString apiError
              }
            , Cmd.none
            )

        OnGetAllRemoteDataSuccess { habits, habitData, frequencyStatsList, habitDayNotes } ->
            let
                habitsArray =
                    Array.fromList habits
            in
            ( { model
                | allHabits = RemoteData.Success habits
                , allHabitData = RemoteData.Success habitData
                , allFrequencyStats = RemoteData.Success frequencyStatsList
                , allHabitDayNotes = RemoteData.Success habitDayNotes
                , setHabitDataShortcutFilteredHabits = habitsArray
                , editGoalHabitSelectionFilteredHabits = habitsArray
                , addNoteHabitSelectionFilteredHabits = habitsArray
                , suspendOrResumeHabitSelectionFilteredHabits = habitsArray
                , graphHabitSelectionFilteredHabits = habitsArray
              }
            , Cmd.none
            )

        -- Add Habit
        OpenAddHabitForm ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.AddNewHabitScreen)
            in
            case model.allHabits of
                -- Only open form if we have a list of habits to double check the new habit's name against (no duplicates allowed)
                RemoteData.Success _ ->
                    ( newDialogScreenModel
                    , Dom.focus "add-habit-form-body-name-input" |> Task.attempt FocusResult
                    )

                _ ->
                    ( model, Cmd.none )

        OnSelectAddHabitKind habitKind ->
            ( updateAddHabit (\addHabit -> { addHabit | kind = habitKind }), Cmd.none )

        OnAddHabitNameInput habitName ->
            ( updateAddHabit (\addHabit -> { addHabit | name = habitName }), Cmd.none )

        OnAddHabitDescriptionInput habitDescription ->
            ( updateAddHabit (\addHabit -> { addHabit | description = habitDescription }), Cmd.none )

        OnSelectAddGoodHabitTime goodHabitTime ->
            ( updateAddHabit (\addHabit -> { addHabit | goodHabitTime = goodHabitTime }), Cmd.none )

        OnAddHabitUnitNameSingularInput unitNameSingular ->
            ( updateAddHabit (\addHabit -> { addHabit | unitNameSingular = unitNameSingular }), Cmd.none )

        OnAddHabitUnitNamePluralInput unitNamePlural ->
            ( updateAddHabit (\addHabit -> { addHabit | unitNamePlural = unitNamePlural }), Cmd.none )

        OnAddHabitSelectFrequencyKind frequencyKind ->
            ( updateAddHabit (\addHabit -> { addHabit | frequencyKind = frequencyKind }), Cmd.none )

        OnAddHabitTimesPerWeekInput timesPerWeek ->
            ( updateAddHabit (\addHabit -> { addHabit | timesPerWeek = extractInt timesPerWeek addHabit.timesPerWeek })
            , Cmd.none
            )

        OnAddHabitSpecificDayMondayInput mondayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | mondayTimes = extractInt mondayTimes addHabit.mondayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayTuesdayInput tuesdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | tuesdayTimes = extractInt tuesdayTimes addHabit.tuesdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayWednesdayInput wednesdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | wednesdayTimes = extractInt wednesdayTimes addHabit.wednesdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayThursdayInput thursdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | thursdayTimes = extractInt thursdayTimes addHabit.thursdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayFridayInput fridayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | fridayTimes = extractInt fridayTimes addHabit.fridayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDaySaturdayInput saturdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | saturdayTimes = extractInt saturdayTimes addHabit.saturdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDaySundayInput sundayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | sundayTimes = extractInt sundayTimes addHabit.sundayTimes })
            , Cmd.none
            )

        OnAddHabitTimesInput times ->
            ( updateAddHabit (\addHabit -> { addHabit | times = extractInt times addHabit.times })
            , Cmd.none
            )

        OnAddHabitDaysInput days ->
            ( updateAddHabit (\addHabit -> { addHabit | days = extractInt days addHabit.days })
            , Cmd.none
            )

        AddHabitFormSubmit ->
            case ( model.selectedYmd, Habit.extractCreateHabit model.user model.addHabit, model.allHabits ) of
                ( Just selectedYmd, Just createHabitData, RemoteData.Success allHabits ) ->
                    let
                        newDialogScreenModel =
                            switchScreen model Nothing

                        isDuplicateHabitName =
                            List.any
                                (\otherHabit ->
                                    (otherHabit |> Habit.getCommonFields |> .name)
                                        == (createHabitData |> Habit.getCommonCreateFields |> .name)
                                )
                                allHabits
                    in
                    if isDuplicateHabitName then
                        ( model, Cmd.none )

                    else
                        ( { newDialogScreenModel | addHabit = Habit.initAddHabitData }
                        , Api.mutationAddHabit createHabitData selectedYmd model.apiBaseUrl OnAddHabitFailure OnAddHabitSuccess
                        )

                ( Nothing, _, _ ) ->
                    ( { model | errorMessage = Just "Error adding habit: current date not available" }
                    , Cmd.none
                    )

                ( Just _, Nothing, _ ) ->
                    -- User hasn't filled out all the fields yet, do nothing
                    ( model, Cmd.none )

                _ ->
                    -- Can't check habits list for duplicate name, do nothing
                    ( model, Cmd.none )

        OnAddHabitFailure apiError ->
            ( { model | errorMessage = Just <| "Error adding habit: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnAddHabitSuccess habit ->
            let
                habitRecord =
                    Habit.getCommonFields habit

                updatedHabitListsModel =
                    updateHabitListsWithNewHabit habit
            in
            ( { updatedHabitListsModel
                | addHabit = Habit.initAddHabitData
              }
            , case model.user of
                Just user ->
                    getFrequencyStats user [ habitRecord.id ]

                Nothing ->
                    Cmd.none
            )

        -- Set Habit Data
        OnHabitAmountInput habitID newVal ->
            let
                newEditingHabitAmountDict amount =
                    model.editingHabitAmountDict
                        |> Dict.update habitID (always <| amount)
            in
            if String.isEmpty newVal then
                ( { model | editingHabitAmountDict = newEditingHabitAmountDict Nothing }, Cmd.none )

            else
                case String.toInt newVal of
                    Nothing ->
                        ( model, Cmd.none )

                    Just newInt ->
                        ( { model | editingHabitAmountDict = newEditingHabitAmountDict <| Just newInt }, Cmd.none )

        SetHabitData selectedYmd habitId maybeNewVal ->
            case ( maybeNewVal, model.user ) of
                ( Just newVal, Just user ) ->
                    ( model
                    , Api.mutationSetHabitData
                        user
                        selectedYmd
                        habitId
                        newVal
                        model.apiBaseUrl
                        OnSetHabitDataFailure
                        OnSetHabitDataSuccess
                    )

                _ ->
                    ( model, Cmd.none )

        OnSetHabitDataFailure apiError ->
            ( { model | errorMessage = Just <| "Error setting habit data: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnSetHabitDataSuccess updatedHabitDatum ->
            let
                newModel =
                    { model
                        | allHabitData =
                            RemoteData.map
                                (\allHabitData ->
                                    Util.replaceOrAdd allHabitData (.id >> (==) updatedHabitDatum.id) updatedHabitDatum
                                )
                                model.allHabitData
                        , editingHabitAmountDict =
                            Dict.update updatedHabitDatum.habitId (always Nothing) model.editingHabitAmountDict
                    }
            in
            ( newModel
            , case model.user of
                Just user ->
                    getFrequencyStats user [ updatedHabitDatum.habitId ]

                Nothing ->
                    Cmd.none
            )

        OnGetFrequencyStatsFailure apiError ->
            ( { model | errorMessage = Just <| "Error retrieving performance stats: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnGetFrequencyStatsSuccess { frequencyStatsList } ->
            let
                updateAllFrequencyStats : List FrequencyStats.FrequencyStats -> List FrequencyStats.FrequencyStats
                updateAllFrequencyStats allFrequencyStats =
                    List.foldl
                        (\newStats statsList ->
                            Util.replaceOrAdd statsList (\stats -> stats.habitId == newStats.habitId) newStats
                        )
                        allFrequencyStats
                        frequencyStatsList
            in
            ( { model
                | allFrequencyStats =
                    case model.allFrequencyStats of
                        RemoteData.Success frequencyStatsFrequencyStatsList ->
                            RemoteData.map
                                updateAllFrequencyStats
                                model.allFrequencyStats

                        _ ->
                            RemoteData.Success frequencyStatsList
              }
            , Cmd.none
            )

        -- Habit Actions Dropdowns
        ToggleHabitActionsDropdown habitId ->
            let
                updatedHabitActionsDropdown =
                    if model.habitActionsDropdown == Just habitId then
                        Nothing

                    else
                        Just habitId
            in
            ( { model
                | habitActionsDropdown = updatedHabitActionsDropdown
                , openTopPanelDateDropdown = False
                , openUserActionsDropdown = False
              }
            , Cmd.none
            )

        -- Dark Mode
        OnToggleDarkMode ->
            ( { model | darkModeOn = not model.darkModeOn }, Cmd.none )

        -- Keyboard Shortcuts
        KeyboardMsg keyMsg ->
            let
                newKeysDown =
                    Keyboard.update keyMsg model.keysDown

                newModel =
                    { model | keysDown = newKeysDown }
            in
            -- If you want to react to key-presses, call a function here instead
            -- of just updating the model (you should still update the model).
            case keyMsg of
                Keyboard.Down keyCode ->
                    let
                        key =
                            Keyboard.fromCode keyCode
                    in
                    update (AttemptKeyboardShortcut key) newModel

                _ ->
                    ( newModel, Cmd.none )

        AttemptKeyboardShortcut key ->
            let
                modelKeysDownList : List Keyboard.Key
                modelKeysDownList =
                    Set.toList model.keysDown |> List.map Keyboard.fromCode

                maybeKeyShortcutWithIndex : Maybe ( Int, KeyboardShortcut.KeyboardShortcut )
                maybeKeyShortcutWithIndex =
                    Util.firstInstanceInList
                        model.keyboardShortcutsList
                        (\shortcut ->
                            List.all (\shortcutKey -> List.member shortcutKey modelKeysDownList) shortcut.keys
                                && (Util.lastElementOfList shortcut.keys == Just key)
                        )

                maybeKeyShortcutMsg : Maybe Msg
                maybeKeyShortcutMsg =
                    Maybe.map
                        (\( index, shortcut ) -> shortcut.msg)
                        maybeKeyShortcutWithIndex
            in
            case maybeKeyShortcutMsg of
                Just shortcutMsg ->
                    update shortcutMsg model

                Nothing ->
                    ( model, Cmd.none )

        ToggleAvailableKeyboardShortcutsScreen ->
            ( { model | showAvailableKeyboardShortcutsScreen = not model.showAvailableKeyboardShortcutsScreen }
            , Cmd.none
            )

        -- Dom
        FocusResult result ->
            case result of
                Result.Err (Dom.NotFound domId) ->
                    ( { model | errorMessage = Just <| "Error focusing on element: id '" ++ domId ++ "' not found" }
                    , Cmd.none
                    )

                Result.Ok () ->
                    ( model, Cmd.none )

        -- Set Habit Data Shortcut
        OpenSetHabitDataShortcutHabitSelectionScreen ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.SetHabitDataShortcutHabitSelectionScreen)
            in
            ( newDialogScreenModel
            , Dom.focus
                "set-habit-data-shortcut-habit-selection-filter-text-input"
                |> Task.attempt FocusResult
            )

        OnSetHabitDataShortcutHabitSelectionFilterTextInput newFilterText ->
            updateHabitSelectionFilterTextInput
                newFilterText
                model.setHabitDataShortcutSelectedHabitIndex
                model.setHabitDataShortcutFilteredHabits
                (\newFilteredHabitsArray newSelectedHabitIndex ->
                    { model
                        | setHabitDataShortcutHabitNameFilterText = newFilterText
                        , setHabitDataShortcutFilteredHabits = newFilteredHabitsArray
                        , setHabitDataShortcutSelectedHabitIndex = newSelectedHabitIndex
                    }
                )

        OnSetHabitDataShortcutSelectNextHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.setHabitDataShortcutFilteredHabits
                (model.setHabitDataShortcutSelectedHabitIndex + 1)
                (\newIndex -> { model | setHabitDataShortcutSelectedHabitIndex = newIndex })

        OnSetHabitDataShortcutSelectPreviousHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.setHabitDataShortcutFilteredHabits
                (model.setHabitDataShortcutSelectedHabitIndex - 1)
                (\newIndex -> { model | setHabitDataShortcutSelectedHabitIndex = newIndex })

        OpenSetHabitDataShortcutAmountScreen ->
            case Array.get model.setHabitDataShortcutSelectedHabitIndex model.setHabitDataShortcutFilteredHabits of
                Just selectedHabit ->
                    let
                        newDialogScreenModel =
                            switchScreen model (Just DialogScreen.SetHabitDataShortcutAmountScreen)
                    in
                    ( { newDialogScreenModel
                        | setHabitDataShortcutAmountScreenHabit = Just selectedHabit
                      }
                    , Dom.focus
                        "set-habit-data-shortcut-amount-screen-input"
                        |> Task.attempt FocusResult
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnSetHabitDataShortcutAmountScreenInput newInput ->
            let
                newInputInt =
                    extractInt newInput model.setHabitDataShortcutAmountScreenInputInt
            in
            ( { model | setHabitDataShortcutAmountScreenInputInt = newInputInt }
            , Cmd.none
            )

        OnSetHabitDataShortcutAmountScreenSubmit ->
            case
                ( model.setHabitDataShortcutAmountScreenInputInt
                , model.selectedYmd
                , ( model.setHabitDataShortcutAmountScreenHabit, model.user )
                )
            of
                ( Just inputInt, Just selectedYmd, ( Just selectedHabit, Just user ) ) ->
                    let
                        habitId =
                            selectedHabit |> Habit.getCommonFields |> .id

                        newDialogScreenModel =
                            switchScreen model Nothing
                    in
                    ( newDialogScreenModel
                    , Api.mutationSetHabitData
                        user
                        selectedYmd
                        habitId
                        inputInt
                        model.apiBaseUrl
                        OnSetHabitDataFailure
                        OnSetHabitDataSuccess
                    )

                ( _, Nothing, _ ) ->
                    ( { model | errorMessage = Just "Error setting habit data: no selected date" }, Cmd.none )

                ( _, _, ( Nothing, _ ) ) ->
                    ( { model | errorMessage = Just "Error setting habit data: no selected habit" }, Cmd.none )

                ( _, _, ( _, Nothing ) ) ->
                    ( { model | errorMessage = Just "Error setting habit data: not logged in" }, Cmd.none )

                _ ->
                    -- User just hasn't entered a new amount, it's fine, do nothing
                    ( model, Cmd.none )

        -- Edit Goal Habit Selection
        OpenEditGoalHabitSelectionScreen ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.EditGoalHabitSelectionScreen)
            in
            ( newDialogScreenModel
            , Dom.focus "edit-goal-habit-selection-filter-text-input"
                |> Task.attempt FocusResult
            )

        OnEditGoalHabitSelectionFilterTextInput newFilterText ->
            updateHabitSelectionFilterTextInput
                newFilterText
                model.editGoalHabitSelectionSelectedHabitIndex
                model.editGoalHabitSelectionFilteredHabits
                (\newFilteredHabitsArray newSelectedHabitIndex ->
                    { model
                        | editGoalHabitSelectionFilterText = newFilterText
                        , editGoalHabitSelectionFilteredHabits = newFilteredHabitsArray
                        , editGoalHabitSelectionSelectedHabitIndex = newSelectedHabitIndex
                    }
                )

        OnEditGoalHabitSelectionSelectNextHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.editGoalHabitSelectionFilteredHabits
                (model.editGoalHabitSelectionSelectedHabitIndex + 1)
                (\newIndex -> { model | editGoalHabitSelectionSelectedHabitIndex = newIndex })

        OnEditGoalHabitSelectionSelectPreviousHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.editGoalHabitSelectionFilteredHabits
                (model.editGoalHabitSelectionSelectedHabitIndex - 1)
                (\newIndex -> { model | editGoalHabitSelectionSelectedHabitIndex = newIndex })

        OnEditGoalHabitSelectionEnterKeydown ->
            case Array.get model.editGoalHabitSelectionSelectedHabitIndex model.editGoalHabitSelectionFilteredHabits of
                Just selectedHabit ->
                    update (OpenEditGoalScreen selectedHabit) model

                Nothing ->
                    ( model, Cmd.none )

        -- Edit Goal
        OpenEditGoalScreen habit ->
            case model.selectedYmd of
                Just selectedYmd ->
                    let
                        habitRecord =
                            Habit.getCommonFields habit

                        oldFrequencies : List Habit.FrequencyChangeRecord
                        oldFrequencies =
                            case habit of
                                Habit.GoodHabit goodHabit ->
                                    goodHabit.targetFrequencies

                                Habit.BadHabit badHabit ->
                                    badHabit.thresholdFrequencies

                        -- Current `FrequencyChangeRecord`
                        currentFcrWithIndex : Maybe ( Int, Habit.FrequencyChangeRecord )
                        currentFcrWithIndex =
                            Util.firstInstanceInList
                                oldFrequencies
                                (\fcr -> YmdDate.withinYmdDateInterval fcr.startDate fcr.endDate selectedYmd)

                        ( newEditGoalFrequencyKind, idToFocusOn ) =
                            case currentFcrWithIndex of
                                Just ( currentIndex, currentFcr ) ->
                                    case currentFcr.newFrequency of
                                        Habit.TotalWeekFrequency f ->
                                            ( Habit.TotalWeekFrequencyKind
                                            , "edit-goal-dialog-x-per-week-input"
                                            )

                                        Habit.SpecificDayOfWeekFrequency f ->
                                            ( Habit.SpecificDayOfWeekFrequencyKind
                                            , "edit-goal-dialog-monday-input"
                                            )

                                        Habit.EveryXDayFrequency f ->
                                            ( Habit.EveryXDayFrequencyKind
                                            , "edit-goal-dialog-every-x-days-times-input"
                                            )

                                Nothing ->
                                    -- No current goal
                                    ( Habit.TotalWeekFrequencyKind
                                    , "edit-goal-dialog-x-per-week-input"
                                    )

                        newEditGoalModel =
                            updateEditGoal (\editGoal -> { editGoal | frequencyKind = newEditGoalFrequencyKind })

                        newModel =
                            { newEditGoalModel
                                | editGoalDialogHabit = Just habit
                                , editGoalDialogHabitCurrentFcrWithIndex = currentFcrWithIndex
                            }

                        ( newConfirmationMessageModel, newCmdMsg ) =
                            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

                        newDialogScreenModel =
                            switchScreen newConfirmationMessageModel (Just DialogScreen.EditGoalScreen)
                    in
                    ( { newDialogScreenModel
                        | editGoalDialogHabit = Just habit
                        , editGoalDialogHabitCurrentFcrWithIndex = currentFcrWithIndex
                      }
                    , Cmd.batch
                        [ newCmdMsg
                        , Dom.focus idToFocusOn
                            |> Task.attempt FocusResult
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnEditGoalSelectFrequencyKind frequencyKind ->
            let
                newEditGoalModel =
                    updateEditGoal (\editGoal -> { editGoal | frequencyKind = frequencyKind })

                ( newRefreshedModel, newCmdMsg ) =
                    update RefreshEditGoalConfirmationMessageAndNewSuspensions newEditGoalModel
            in
            ( newRefreshedModel
            , Cmd.batch
                [ newCmdMsg
                , Dom.focus
                    (case frequencyKind of
                        Habit.EveryXDayFrequencyKind ->
                            "edit-goal-dialog-every-x-days-times-input"

                        Habit.SpecificDayOfWeekFrequencyKind ->
                            "edit-goal-dialog-monday-input"

                        Habit.TotalWeekFrequencyKind ->
                            "edit-goal-dialog-x-per-week-input"
                    )
                    |> Task.attempt FocusResult
                ]
            )

        OnEditGoalTimesPerWeekInput timesPerWeek ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | timesPerWeek = extractInt timesPerWeek editGoal.timesPerWeek })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDayMondayInput mondayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | mondayTimes = extractInt mondayTimes editGoal.mondayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDayTuesdayInput tuesdayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | tuesdayTimes = extractInt tuesdayTimes editGoal.tuesdayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDayWednesdayInput wednesdayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | wednesdayTimes = extractInt wednesdayTimes editGoal.wednesdayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDayThursdayInput thursdayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | thursdayTimes = extractInt thursdayTimes editGoal.thursdayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDayFridayInput fridayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | fridayTimes = extractInt fridayTimes editGoal.fridayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDaySaturdayInput saturdayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | saturdayTimes = extractInt saturdayTimes editGoal.saturdayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalSpecificDaySundayInput sundayTimes ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | sundayTimes = extractInt sundayTimes editGoal.sundayTimes })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalTimesInput times ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | times = extractInt times editGoal.times })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        OnEditGoalDaysInput days ->
            let
                newModel =
                    updateEditGoal (\editGoal -> { editGoal | days = extractInt days editGoal.days })
            in
            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel

        RefreshEditGoalConfirmationMessageAndNewSuspensions ->
            case ( model.editGoalDialogHabit, Habit.extractNewGoal model.editGoal, model.selectedYmd ) of
                ( Just habit, Just newGoal, Just selectedYmd ) ->
                    let
                        yesterday =
                            YmdDate.addDays -1 selectedYmd

                        habitRecord =
                            Habit.getCommonFields habit

                        ( habitType, oldFrequenciesList ) =
                            case habit of
                                Habit.GoodHabit gh ->
                                    ( "good_habit", gh.targetFrequencies )

                                Habit.BadHabit bh ->
                                    ( "bad_habit", bh.thresholdFrequencies )

                        oldFrequenciesArray =
                            Array.fromList oldFrequenciesList

                        -- First `FrequencyChangeRecord` that starts after today
                        nextFcrWithIndex : Maybe ( Int, Habit.FrequencyChangeRecord )
                        nextFcrWithIndex =
                            Util.firstInstanceInArray oldFrequenciesArray
                                (\fcr -> YmdDate.compareYmds fcr.startDate selectedYmd == GT)

                        -- Last `FrequencyChangeRecord` that ended before today
                        previousFcrWithIndex : Maybe ( Int, Habit.FrequencyChangeRecord )
                        previousFcrWithIndex =
                            Util.lastInstanceInArray oldFrequenciesArray
                                (\fcr ->
                                    case fcr.endDate of
                                        Just endYmd ->
                                            YmdDate.compareYmds endYmd selectedYmd == LT

                                        Nothing ->
                                            False
                                )

                        arrayLength =
                            Array.length oldFrequenciesArray

                        newFrequenciesArray : Array.Array Habit.FrequencyChangeRecord
                        newFrequenciesArray =
                            case model.editGoalDialogHabitCurrentFcrWithIndex of
                                -- There is a current goal
                                Just ( currentIndex, currentFcr ) ->
                                    if currentFcr.newFrequency == newGoal then
                                        -- Current goal is same as new goal already, nothing to change
                                        oldFrequenciesArray

                                    else if currentFcr.startDate == selectedYmd then
                                        -- Current goal started today, we should just replace it
                                        case nextFcrWithIndex of
                                            -- There is a next goal
                                            Just ( nextIndex, nextFcr ) ->
                                                if nextFcr.newFrequency == newGoal then
                                                    -- Next goal is the same as the new one
                                                    case previousFcrWithIndex of
                                                        Just ( previousIndex, previousFcr ) ->
                                                            -- There is a previous goal
                                                            if previousFcr.newFrequency == newGoal then
                                                                -- Previous goal is also the same. Merge it with next goal.
                                                                Array.append
                                                                    (Array.slice
                                                                        0
                                                                        (previousIndex + 1)
                                                                        (Array.set
                                                                            previousIndex
                                                                            { previousFcr | endDate = nextFcr.endDate }
                                                                            oldFrequenciesArray
                                                                        )
                                                                    )
                                                                    (Array.slice
                                                                        (nextIndex + 1)
                                                                        arrayLength
                                                                        oldFrequenciesArray
                                                                    )

                                                            else
                                                                -- Previous goal is different. Merge new and next goal into one.
                                                                Array.append
                                                                    (Array.slice 0 (previousIndex + 1) oldFrequenciesArray)
                                                                    (Array.slice
                                                                        nextIndex
                                                                        arrayLength
                                                                        (Array.set
                                                                            nextIndex
                                                                            { nextFcr | startDate = selectedYmd }
                                                                            oldFrequenciesArray
                                                                        )
                                                                    )

                                                        Nothing ->
                                                            -- There are no previous goals. Merge new and next goal into one.
                                                            oldFrequenciesArray
                                                                |> Array.set nextIndex { nextFcr | startDate = selectedYmd }
                                                                |> Array.slice nextIndex arrayLength

                                                else
                                                    -- Next goal is different
                                                    case previousFcrWithIndex of
                                                        Just ( previousIndex, previousFcr ) ->
                                                            -- There is a previous goal
                                                            if previousFcr.newFrequency == newGoal then
                                                                -- Previous goal is the same, merge it with new one
                                                                Array.append
                                                                    (oldFrequenciesArray
                                                                        |> Array.set
                                                                            previousIndex
                                                                            { previousFcr | endDate = Just <| YmdDate.addDays -1 nextFcr.startDate }
                                                                        |> Array.slice 0 (previousIndex + 1)
                                                                    )
                                                                    (Array.slice nextIndex arrayLength oldFrequenciesArray)

                                                            else
                                                                -- Previous goal is different. Just replace current goal.
                                                                Array.set currentIndex { currentFcr | newFrequency = newGoal } oldFrequenciesArray

                                                        Nothing ->
                                                            -- There are no previous goals. Just replace current goal.
                                                            Array.set currentIndex { currentFcr | newFrequency = newGoal } oldFrequenciesArray

                                            Nothing ->
                                                -- There are no next goals
                                                case previousFcrWithIndex of
                                                    Just ( previousIndex, previousFcr ) ->
                                                        -- There is a previous goal
                                                        if previousFcr.newFrequency == newGoal then
                                                            -- Previous goal is the same, merge it with new one
                                                            oldFrequenciesArray
                                                                |> Array.set previousIndex { previousFcr | endDate = currentFcr.endDate }
                                                                |> Array.slice 0 (previousIndex + 1)

                                                        else
                                                            -- Previous goal is different. Just replace current goal.
                                                            Array.set currentIndex { currentFcr | newFrequency = newGoal } oldFrequenciesArray

                                                    Nothing ->
                                                        -- There are no previous goals. Just replace current goal.
                                                        Array.set currentIndex { currentFcr | newFrequency = newGoal } oldFrequenciesArray

                                    else
                                        -- current goal started yesterday or earlier, we should start a new one
                                        case nextFcrWithIndex of
                                            Just ( nextIndex, nextFcr ) ->
                                                -- There is a next goal
                                                if nextFcr.newFrequency == newGoal then
                                                    -- Next goal is the same, we should just extend it to today
                                                    oldFrequenciesArray
                                                        |> Array.set
                                                            currentIndex
                                                            { currentFcr | endDate = Just yesterday }
                                                        |> Array.set
                                                            nextIndex
                                                            { nextFcr | startDate = selectedYmd }

                                                else
                                                    -- Next goal is different, just make a new goal between the current and next goal
                                                    Array.append
                                                        (Array.slice
                                                            0
                                                            (currentIndex + 1)
                                                            (Array.set
                                                                currentIndex
                                                                { currentFcr | endDate = Just yesterday }
                                                                oldFrequenciesArray
                                                            )
                                                            |> Array.push
                                                                { startDate = selectedYmd
                                                                , endDate = Just <| YmdDate.addDays -1 nextFcr.startDate
                                                                , newFrequency = newGoal
                                                                }
                                                        )
                                                        (Array.slice nextIndex arrayLength oldFrequenciesArray)

                                            Nothing ->
                                                -- There are no next goals. Just add a new one starting today.
                                                oldFrequenciesArray
                                                    |> Array.set currentIndex { currentFcr | endDate = Just yesterday }
                                                    |> Array.slice 0 (currentIndex + 1)
                                                    |> Array.push { startDate = selectedYmd, endDate = Nothing, newFrequency = newGoal }

                                Nothing ->
                                    -- No current goal
                                    case previousFcrWithIndex of
                                        Just ( previousIndex, previousFcr ) ->
                                            -- There's a previous goal but not a current one. This shouldn't happen but we can fix it anyway.
                                            if previousFcr.newFrequency == newGoal then
                                                -- Previous goal is the same
                                                case nextFcrWithIndex of
                                                    Just ( nextIndex, nextFcr ) ->
                                                        -- There is a next goal
                                                        if nextFcr.newFrequency == newGoal then
                                                            -- Next goal is also the same. Merge previous and next goal together.
                                                            Array.append
                                                                (oldFrequenciesArray
                                                                    |> Array.set previousIndex { previousFcr | endDate = nextFcr.endDate }
                                                                    |> Array.slice 0 (previousIndex + 1)
                                                                )
                                                                (Array.slice (nextIndex + 1) arrayLength oldFrequenciesArray)

                                                        else
                                                            -- Next goal is different. Just merge previous and new goal.
                                                            Array.append
                                                                (oldFrequenciesArray
                                                                    |> Array.set
                                                                        previousIndex
                                                                        { previousFcr | endDate = Just <| YmdDate.addDays -1 nextFcr.startDate }
                                                                )
                                                                (Array.slice nextIndex arrayLength oldFrequenciesArray)

                                                    Nothing ->
                                                        -- There are no next goals. Just make previous goal endless.
                                                        oldFrequenciesArray
                                                            |> Array.set previousIndex { previousFcr | endDate = Nothing }
                                                            |> Array.slice 0 (previousIndex + 1)

                                            else
                                                -- Previous goal is different
                                                case nextFcrWithIndex of
                                                    Just ( nextIndex, nextFcr ) ->
                                                        -- There is a next goal
                                                        if nextFcr.newFrequency == newGoal then
                                                            -- Next goal is the same. Extend previous to yesterday and next goal to today.
                                                            Array.append
                                                                (oldFrequenciesArray
                                                                    |> Array.set previousIndex { previousFcr | endDate = Just yesterday }
                                                                    |> Array.slice 0 (previousIndex + 1)
                                                                )
                                                                (oldFrequenciesArray
                                                                    |> Array.set nextIndex { nextFcr | startDate = selectedYmd }
                                                                    |> Array.slice nextIndex arrayLength
                                                                )

                                                        else
                                                            -- Next goal is different. Extend previous to yesterday, start a new goal today.
                                                            Array.append
                                                                (oldFrequenciesArray
                                                                    |> Array.set previousIndex { previousFcr | endDate = Just yesterday }
                                                                    |> Array.slice 0 (previousIndex + 1)
                                                                    |> Array.push
                                                                        { startDate = selectedYmd
                                                                        , endDate = Just <| YmdDate.addDays -1 nextFcr.startDate
                                                                        , newFrequency = newGoal
                                                                        }
                                                                )
                                                                (Array.slice nextIndex arrayLength oldFrequenciesArray)

                                                    Nothing ->
                                                        -- There are no next goals. Extend previous to yesterday and start a new goal today.
                                                        oldFrequenciesArray
                                                            |> Array.set previousIndex { previousFcr | endDate = Just yesterday }
                                                            |> Array.slice 0 (previousIndex + 1)
                                                            |> Array.push { startDate = selectedYmd, endDate = Nothing, newFrequency = newGoal }

                                        Nothing ->
                                            -- There are no previous goals
                                            case nextFcrWithIndex of
                                                Just ( nextIndex, nextFcr ) ->
                                                    -- There is a next goal
                                                    if nextFcr.newFrequency == newGoal then
                                                        -- Next goal is the same, just extend it to today
                                                        oldFrequenciesArray
                                                            |> Array.set nextIndex { nextFcr | startDate = selectedYmd }
                                                            |> Array.slice nextIndex arrayLength

                                                    else
                                                        -- Next goal is different. Create a new one starting today.
                                                        Array.append
                                                            (Array.fromList
                                                                [ { startDate = selectedYmd
                                                                  , endDate = Just <| YmdDate.addDays -1 nextFcr.startDate
                                                                  , newFrequency = newGoal
                                                                  }
                                                                ]
                                                            )
                                                            (Array.slice nextIndex arrayLength oldFrequenciesArray)

                                                Nothing ->
                                                    -- There are no goals at all. Create a new one starting today.
                                                    Array.fromList [ { startDate = selectedYmd, endDate = Nothing, newFrequency = newGoal } ]

                        newFrequenciesList =
                            Array.toList newFrequenciesArray

                        alreadyCurrentGoalMessage =
                            "This is already the current goal."

                        newGoalConfirmationMessage =
                            "The new goal "
                                ++ Habit.prettyPrintFrequency newGoal habitRecord.unitNameSingular habitRecord.unitNamePlural
                                ++ " will officially start today ("
                                ++ YmdDate.prettyPrintWithWeekday selectedYmd
                                ++ ")."

                        confirmationMessage =
                            case model.editGoalDialogHabitCurrentFcrWithIndex of
                                Just ( currentIndex, currentFcr ) ->
                                    if currentFcr.newFrequency == newGoal then
                                        Just alreadyCurrentGoalMessage

                                    else
                                        Just <|
                                            "The previous goal for "
                                                ++ habitRecord.name
                                                ++ " was "
                                                ++ Habit.prettyPrintFrequency currentFcr.newFrequency habitRecord.unitNameSingular habitRecord.unitNamePlural
                                                ++ ". "
                                                ++ newGoalConfirmationMessage

                                Nothing ->
                                    Just <|
                                        habitRecord.name
                                            ++ " does not currently have a goal. "
                                            ++ newGoalConfirmationMessage

                        newEditGoalNewFrequenciesList =
                            if confirmationMessage == Just alreadyCurrentGoalMessage then
                                Nothing

                            else
                                Just newFrequenciesList
                    in
                    ( { model
                        | editGoalConfirmationMessage = confirmationMessage
                        , editGoalNewFrequenciesList = newEditGoalNewFrequenciesList
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | editGoalConfirmationMessage = Nothing, editGoalNewFrequenciesList = Nothing }
                    , Cmd.none
                    )

        OnEditGoalFailure apiError ->
            ( { model | errorMessage = Just <| "Error editing habit goal: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnEditGoalSuccess habit ->
            let
                updatedHabitListsModel =
                    updateHabitListsWithNewHabit habit
            in
            ( { updatedHabitListsModel
                | editGoal = Habit.initEditGoalData
              }
            , case model.user of
                Just user ->
                    getFrequencyStats user [ habit |> Habit.getCommonFields |> .id ]

                Nothing ->
                    Cmd.none
            )

        OnEditGoalSubmit ->
            case ( model.editGoalDialogHabit, model.editGoalNewFrequenciesList, model.user ) of
                ( Just habit, Just newFrequenciesList, Just user ) ->
                    let
                        ( habitId, habitType ) =
                            case habit of
                                Habit.GoodHabit gh ->
                                    ( gh.id, "good_habit" )

                                Habit.BadHabit bh ->
                                    ( bh.id, "bad_habit" )

                        newDialogScreenModel =
                            switchScreen model Nothing
                    in
                    ( newDialogScreenModel
                    , Api.mutationEditHabitGoalFrequencies
                        user
                        habitId
                        newFrequenciesList
                        habitType
                        model.apiBaseUrl
                        OnEditGoalFailure
                        OnEditGoalSuccess
                    )

                ( Nothing, _, _ ) ->
                    -- User shouldn't be able to call `OnEditGoalSubmit` unless the Edit Goal dialog is open
                    ( { model | errorMessage = Just "Error editing habit goal: no habit selected" }
                    , Cmd.none
                    )

                ( _, _, Nothing ) ->
                    ( { model | errorMessage = Just "Error editing habit goal: not logged in" }
                    , Cmd.none
                    )

                ( _, Nothing, _ ) ->
                    -- If the user hasn't filled out all new goal fields properly yet, it's fine, just do nothing
                    ( model, Cmd.none )

        -- Error Messages
        OpenErrorMessageDialogScreen ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.ErrorMessageScreen)
            in
            ( newDialogScreenModel
            , Cmd.none
            )

        -- Full screen dialogs
        OnExitDialogScreen ->
            let
                -- If there is at least one habit, we can focus on its amount input when exiting a dialog form
                shouldAttemptFocus =
                    model.allHabits
                        |> RemoteData.map (not << List.isEmpty)
                        |> (==) (RemoteData.Success True)

                newDialogScreenModel =
                    switchScreen model Nothing
            in
            ( { newDialogScreenModel
                | graphGoalIntervals = RemoteData.NotAsked
                , graphIntervalsData = RemoteData.NotAsked
              }
            , if shouldAttemptFocus then
                Dom.focus "first-habit-amount-input" |> Task.attempt FocusResult

              else
                Cmd.none
            )

        -- Add Note Habit Selection
        OpenAddNoteHabitSelectionDialogScreen ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.AddNoteHabitSelectionScreen)
            in
            ( newDialogScreenModel
            , Dom.focus "add-note-habit-selection-filter-text-input"
                |> Task.attempt FocusResult
            )

        OnAddNoteHabitSelectionFilterTextInput newFilterText ->
            updateHabitSelectionFilterTextInput
                newFilterText
                model.addNoteHabitSelectionSelectedHabitIndex
                model.addNoteHabitSelectionFilteredHabits
                (\newFilteredHabitsArray newSelectedHabitIndex ->
                    { model
                        | addNoteHabitSelectionFilterText = newFilterText
                        , addNoteHabitSelectionFilteredHabits = newFilteredHabitsArray
                        , addNoteHabitSelectionSelectedHabitIndex = newSelectedHabitIndex
                    }
                )

        OnAddNoteHabitSelectionScreenSelectNextHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.addNoteHabitSelectionFilteredHabits
                (model.addNoteHabitSelectionSelectedHabitIndex + 1)
                (\newIndex -> { model | addNoteHabitSelectionSelectedHabitIndex = newIndex })

        OnAddNoteHabitSelectionScreenSelectPreviousHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.addNoteHabitSelectionFilteredHabits
                (model.addNoteHabitSelectionSelectedHabitIndex - 1)
                (\newIndex -> { model | addNoteHabitSelectionSelectedHabitIndex = newIndex })

        OnAddNoteHabitSelectionEnterKeydown ->
            case Array.get model.addNoteHabitSelectionSelectedHabitIndex model.addNoteHabitSelectionFilteredHabits of
                Just selectedHabit ->
                    update (OpenAddNoteDialog selectedHabit) model

                Nothing ->
                    ( model, Cmd.none )

        -- Add Note Dialog
        OpenAddNoteDialog habit ->
            let
                habitRecord =
                    Habit.getCommonFields habit

                existingHabitDayNoteText : Maybe String
                existingHabitDayNoteText =
                    case ( model.allHabitDayNotes, model.selectedYmd ) of
                        ( RemoteData.Success allHabitDayNotes, Just ymd ) ->
                            List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == ymd) allHabitDayNotes
                                |> List.head
                                |> Maybe.map .note

                        _ ->
                            Nothing

                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.AddNoteScreen)
            in
            ( { newDialogScreenModel
                | addNoteDialogHabit = Just habit
                , addNoteDialogInput = Maybe.withDefault "" existingHabitDayNoteText
              }
            , Dom.focus "add-note-dialog-input-id"
                |> Task.attempt FocusResult
            )

        OnAddNoteDialogInput newAddNoteInput ->
            ( { model | addNoteDialogInput = newAddNoteInput }, Cmd.none )

        OnAddNoteSubmit ->
            case ( String.isEmpty model.addNoteDialogInput, model.selectedYmd, ( model.addNoteDialogHabit, model.user ) ) of
                ( False, Just selectedYmd, ( Just habit, Just user ) ) ->
                    let
                        newDialogScreenModel =
                            switchScreen model Nothing
                    in
                    ( newDialogScreenModel
                    , Api.mutationSetHabitDayNote
                        user
                        selectedYmd
                        (habit |> Habit.getCommonFields |> .id)
                        model.addNoteDialogInput
                        model.apiBaseUrl
                        OnAddNoteFailure
                        OnAddNoteSuccess
                    )

                _ ->
                    ( model, Cmd.none )

        OnAddNoteFailure apiError ->
            ( { model | errorMessage = Just <| "Error adding habit day note: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnAddNoteSuccess habitDayNote ->
            ( { model
                | addNoteDialogInput = ""
                , allHabitDayNotes =
                    RemoteData.map
                        (\allHabitDayNotes ->
                            Util.replaceOrAdd allHabitDayNotes (.id >> (==) habitDayNote.id) habitDayNote
                        )
                        model.allHabitDayNotes
              }
            , Cmd.none
            )

        -- Suspend Or Resume Habit Selection Screen
        OpenSuspendOrResumeHabitSelectionScreen ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.SuspendOrResumeHabitSelectionScreen)
            in
            ( newDialogScreenModel
            , Dom.focus "suspend-or-resume-habit-selection-filter-text-input"
                |> Task.attempt FocusResult
            )

        OnSuspendOrResumeHabitSelectionFilterTextInput newFilterText ->
            updateHabitSelectionFilterTextInput
                newFilterText
                model.suspendOrResumeHabitSelectionSelectedHabitIndex
                model.suspendOrResumeHabitSelectionFilteredHabits
                (\newFilteredHabitsArray newSelectedHabitIndex ->
                    { model
                        | suspendOrResumeHabitSelectionFilterText = newFilterText
                        , suspendOrResumeHabitSelectionFilteredHabits = newFilteredHabitsArray
                        , suspendOrResumeHabitSelectionSelectedHabitIndex = newSelectedHabitIndex
                    }
                )

        OnSuspendOrResumeHabitSelectionSelectNextHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.suspendOrResumeHabitSelectionFilteredHabits
                (model.suspendOrResumeHabitSelectionSelectedHabitIndex + 1)
                (\newIndex -> { model | suspendOrResumeHabitSelectionSelectedHabitIndex = newIndex })

        OnSuspendOrResumeHabitSelectionSelectPreviousHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.suspendOrResumeHabitSelectionFilteredHabits
                (model.suspendOrResumeHabitSelectionSelectedHabitIndex - 1)
                (\newIndex -> { model | suspendOrResumeHabitSelectionSelectedHabitIndex = newIndex })

        OnSuspendOrResumeHabitSelectionEnterKeydown ->
            case Array.get model.suspendOrResumeHabitSelectionSelectedHabitIndex model.suspendOrResumeHabitSelectionFilteredHabits of
                Just selectedHabit ->
                    update (OpenSuspendOrResumeConfirmationScreen selectedHabit) model

                Nothing ->
                    ( model, Cmd.none )

        -- Suspend Or Resume Confirmation Screen
        OpenSuspendOrResumeConfirmationScreen habit ->
            case model.selectedYmd of
                Just selectedYmd ->
                    let
                        habitRecord =
                            Habit.getCommonFields habit

                        oldSuspensionsArray =
                            Array.fromList habitRecord.suspensions

                        -- Current suspended interval
                        currentSuspendedIntervalWithIndex : Maybe ( Int, Habit.SuspendedInterval )
                        currentSuspendedIntervalWithIndex =
                            Util.firstInstanceInArray oldSuspensionsArray
                                (\interval -> YmdDate.withinYmdDateInterval interval.startDate interval.endDate selectedYmd)

                        -- First interval that starts after today
                        nextSuspendedIntervalWithIndex : Maybe ( Int, Habit.SuspendedInterval )
                        nextSuspendedIntervalWithIndex =
                            Util.firstInstanceInArray oldSuspensionsArray
                                (\interval -> YmdDate.compareYmds interval.startDate selectedYmd == GT)

                        -- Last interval that ends before today
                        previousSuspendedIntervalWithIndex : Maybe ( Int, Habit.SuspendedInterval )
                        previousSuspendedIntervalWithIndex =
                            Util.lastInstanceInArray oldSuspensionsArray
                                (\interval ->
                                    case interval.endDate of
                                        Just endYmd ->
                                            YmdDate.compareYmds endYmd selectedYmd == LT

                                        Nothing ->
                                            False
                                )

                        habitNameMessage =
                            habitRecord.name ++ " is currently "

                        untilMessage ymd =
                            "until " ++ YmdDate.prettyPrintWithWeekday ymd ++ "."

                        indefinitelyMessage =
                            "indefinitely."

                        resumeMessage =
                            habitNameMessage
                                ++ "suspended. If you resume it now, it will stay active "
                                ++ (case nextSuspendedIntervalWithIndex of
                                        Just ( nextIndex, nextInterval ) ->
                                            untilMessage nextInterval.startDate

                                        Nothing ->
                                            indefinitelyMessage
                                   )

                        suspendMessage =
                            habitNameMessage
                                ++ "active. If you suspend it now, it will stay suspended "
                                ++ (case nextSuspendedIntervalWithIndex of
                                        Just ( nextIndex, nextInterval ) ->
                                            nextInterval.endDate |> Maybe.map untilMessage |> Maybe.withDefault indefinitelyMessage

                                        Nothing ->
                                            indefinitelyMessage
                                   )

                        confirmationMessage : String
                        confirmationMessage =
                            case currentSuspendedIntervalWithIndex of
                                Just ( currentIndex, currentInterval ) ->
                                    -- Habit is currently suspended. User is trying to Resume the Habit.
                                    resumeMessage

                                Nothing ->
                                    suspendMessage

                        newSuspensionsArray : Array.Array Habit.SuspendedInterval
                        newSuspensionsArray =
                            case currentSuspendedIntervalWithIndex of
                                Just ( index, currentInterval ) ->
                                    -- Habit is currently suspended. User is trying to Resume the Habit.
                                    if currentInterval.startDate == selectedYmd then
                                        -- Selected day is the first day of the suspended interval; we can just cancel this suspension.
                                        Array.filter ((/=) currentInterval) oldSuspensionsArray

                                    else
                                        -- Selected day isn't the first day of the suspended interval. We want the habit to remain suspended
                                        -- until the selected day, so we'll set the interval's end date to the day before the selected day.
                                        Array.set
                                            index
                                            { currentInterval | endDate = Just <| YmdDate.addDays -1 selectedYmd }
                                            oldSuspensionsArray

                                Nothing ->
                                    -- Habit is currently active. User is trying to Suspend the Habit.
                                    case nextSuspendedIntervalWithIndex of
                                        Just ( nextIndex, nextInterval ) ->
                                            -- There is a next interval
                                            case previousSuspendedIntervalWithIndex of
                                                Just ( previousIndex, previousInterval ) ->
                                                    -- There is a previous interval
                                                    if previousInterval.endDate == Just (YmdDate.addDays -1 selectedYmd) then
                                                        -- The previous interval ended yesterday anyway, so now we can just remove the next interval
                                                        -- and set the previous interval's end date to that of the next interval.
                                                        oldSuspensionsArray
                                                            |> Array.filter ((/=) nextInterval)
                                                            |> Array.set previousIndex { previousInterval | endDate = nextInterval.endDate }

                                                    else
                                                        -- The previous interval ended before yesterday, so we'll set the next interval's start
                                                        -- date to today.
                                                        Array.set nextIndex { nextInterval | startDate = selectedYmd } oldSuspensionsArray

                                                Nothing ->
                                                    -- No previous interval
                                                    Array.set nextIndex { nextInterval | startDate = selectedYmd } oldSuspensionsArray

                                        Nothing ->
                                            -- There is no next interval
                                            case previousSuspendedIntervalWithIndex of
                                                Just ( previousIndex, previousInterval ) ->
                                                    if previousInterval.endDate == Just (YmdDate.addDays -1 selectedYmd) then
                                                        -- The previous interval ended yesterday anyway, we can just make it endless
                                                        Array.set previousIndex { previousInterval | endDate = Nothing } oldSuspensionsArray

                                                    else
                                                        -- The previous interval ended before yesterday, we'll just start a new one today
                                                        Array.push { startDate = selectedYmd, endDate = Nothing } oldSuspensionsArray

                                                Nothing ->
                                                    -- There is no previous interval either, we can just start a suspended interval today
                                                    Array.push { startDate = selectedYmd, endDate = Nothing } oldSuspensionsArray

                        newDialogScreenModel =
                            switchScreen model (Just DialogScreen.SuspendOrResumeConfirmationScreen)
                    in
                    ( { newDialogScreenModel
                        | suspendOrResumeHabit = Just habit
                        , suspendOrResumeHabitConfirmationMessage = confirmationMessage
                        , suspendOrResumeHabitNewSuspensions = Just <| Array.toList newSuspensionsArray
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnResumeOrSuspendSubmitClick ->
            case ( model.suspendOrResumeHabit, model.suspendOrResumeHabitNewSuspensions, model.user ) of
                ( Just habit, Just newSuspensions, Just user ) ->
                    let
                        habitRecord =
                            Habit.getCommonFields habit

                        newDialogScreenModel =
                            switchScreen model Nothing
                    in
                    ( { newDialogScreenModel
                        | suspendOrResumeHabit = Nothing
                        , suspendOrResumeHabitConfirmationMessage = ""
                        , suspendOrResumeHabitNewSuspensions = Nothing
                      }
                    , Api.mutationEditHabitSuspensions
                        user
                        habitRecord.id
                        newSuspensions
                        model.apiBaseUrl
                        OnResumeOrSuspendHabitFailure
                        OnResumeOrSuspendHabitSuccess
                    )

                ( _, _, Nothing ) ->
                    ( { model | errorMessage = Just "Error suspending/resuming habit: not logged in" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnResumeOrSuspendHabitFailure apiError ->
            ( { model | errorMessage = Just <| "Error suspending/resuming habit: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnResumeOrSuspendHabitSuccess habit ->
            let
                updatedHabitListsModel =
                    updateHabitListsWithNewHabit habit
            in
            ( updatedHabitListsModel
            , case model.user of
                Just user ->
                    getFrequencyStats user [ habit |> Habit.getCommonFields |> .id ]

                Nothing ->
                    Cmd.none
            )

        -- Graph Habit Selection Screen
        OpenGraphHabitSelectionScreen ->
            let
                newDialogScreenModel =
                    switchScreen model (Just DialogScreen.GraphHabitSelectionScreen)
            in
            ( newDialogScreenModel
            , Dom.focus "graph-habit-selection-filter-text-input"
                |> Task.attempt FocusResult
            )

        OnGraphHabitSelectionFilterTextInput newFilterText ->
            updateHabitSelectionFilterTextInput
                newFilterText
                model.graphHabitSelectionSelectedHabitIndex
                model.graphHabitSelectionFilteredHabits
                (\newFilteredHabitsArray newSelectedHabitIndex ->
                    { model
                        | graphHabitSelectionFilterText = newFilterText
                        , graphHabitSelectionFilteredHabits = newFilteredHabitsArray
                        , graphHabitSelectionSelectedHabitIndex = newSelectedHabitIndex
                    }
                )

        OnGraphHabitSelectionSelectNextHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.graphHabitSelectionFilteredHabits
                (model.graphHabitSelectionSelectedHabitIndex + 1)
                (\newIndex -> { model | graphHabitSelectionSelectedHabitIndex = newIndex })

        OnGraphHabitSelectionSelectPreviousHabit ->
            updateOnHabitSelectionChangeSelectedHabitIndex
                model.graphHabitSelectionFilteredHabits
                (model.graphHabitSelectionSelectedHabitIndex - 1)
                (\newIndex -> { model | graphHabitSelectionSelectedHabitIndex = newIndex })

        OnGraphHabitSelectionEnterKeydown ->
            case Array.get model.graphHabitSelectionSelectedHabitIndex model.graphHabitSelectionFilteredHabits of
                Just selectedHabit ->
                    update (OpenGraphDialogScreen selectedHabit) model

                Nothing ->
                    ( model, Cmd.none )

        -- Graph Dialog Screen
        OpenGraphDialogScreen habit ->
            case ( model.selectedYmd, model.user ) of
                ( Just selectedYmd, Just user ) ->
                    let
                        newDialogScreenModel =
                            switchScreen model (Just DialogScreen.GraphDialogScreen)
                    in
                    ( { newDialogScreenModel
                        | graphHabit = Just habit
                        , graphIntervalsData = RemoteData.Loading
                        , graphGoalIntervals = RemoteData.Loading
                      }
                    , getGraphHabitGoalIntervalList user habit newDialogScreenModel.graphNumDaysToShow selectedYmd
                    )

                ( Nothing, _ ) ->
                    ( { model | errorMessage = Just "Error opening habit graph: no date selected" }, Cmd.none )

                ( _, Nothing ) ->
                    ( { model | errorMessage = Just "Error opening habit graph: not logged in" }, Cmd.none )

        SetGraphNumDaysToShow numDaysToShow ->
            case ( model.graphHabit, model.selectedYmd, model.user ) of
                ( Just graphHabit, Just selectedYmd, Just user ) ->
                    if numDaysToShow == model.graphNumDaysToShow then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | graphNumDaysToShow = numDaysToShow
                            , graphIntervalsData = RemoteData.Loading
                            , graphGoalIntervals = RemoteData.Loading
                          }
                        , getGraphHabitGoalIntervalList user graphHabit numDaysToShow selectedYmd
                        )

                ( Nothing, _, _ ) ->
                    ( { model | errorMessage = Just "Error setting graph's number of days to show: no habit selected" }, Cmd.none )

                ( _, Nothing, _ ) ->
                    ( { model | errorMessage = Just "Error setting graph's number of days to show: no date selected" }, Cmd.none )

                ( _, _, Nothing ) ->
                    ( { model | errorMessage = Just "Error setting graph's number of days to show: not logged in" }, Cmd.none )

        OnGetGraphHabitGoalIntervalListFailure apiError ->
            ( { model
                | errorMessage = Just <| "Error retrieving graph data: " ++ ApiError.toString apiError
                , graphIntervalsData = RemoteData.Failure apiError
                , graphGoalIntervals = RemoteData.Failure apiError
              }
            , Cmd.none
            )

        OnGetGraphHabitGoalIntervalListSuccess { habitGoalIntervalLists } ->
            let
                habitGoalIntervalList : Maybe HabitGoalIntervalList.HabitGoalIntervalList
                habitGoalIntervalList =
                    List.head habitGoalIntervalLists
            in
            case ( habitGoalIntervalList, model.graphHabit, ( model.allHabitData, model.allHabitDayNotes ) ) of
                ( Just intervalList, Just graphHabit, ( RemoteData.Success allHabitData, RemoteData.Success allNotes ) ) ->
                    let
                        graphHabitId =
                            graphHabit |> Habit.getCommonFields |> .id
                    in
                    if intervalList.habitId /= graphHabitId then
                        ( { model | errorMessage = Just "Error retrieving graph data: wrong habit" }, Cmd.none )

                    else
                        let
                            graphIntervalsData =
                                Graph.getAllGraphIntervalData intervalList.goalIntervals graphHabit allHabitData allNotes graphHabitId
                        in
                        ( { model
                            | graphIntervalsData = RemoteData.Success graphIntervalsData
                            , graphGoalIntervals = RemoteData.Success intervalList.goalIntervals
                          }
                        , Cmd.none
                        )

                ( Nothing, _, ( _, _ ) ) ->
                    ( { model | errorMessage = Just "Error retrieving graph data: no data received from server" }, Cmd.none )

                ( _, Nothing, ( _, _ ) ) ->
                    ( { model | errorMessage = Just "Error retrieving graph data: no habit selected" }, Cmd.none )

                _ ->
                    ( { model | errorMessage = Just "Error generating graph data: habit data or notes not available" }, Cmd.none )

        OnGraphPointHover maybeHoveredPoint ->
            ( { model | graphHoveredPoint = maybeHoveredPoint }, Cmd.none )


extractInt : String -> Maybe Int -> Maybe Int
extractInt string default =
    let
        maybeInt : Maybe Int
        maybeInt =
            String.toInt string
    in
    if string == "" then
        -- override the default if user inputs an empty string (e.g. by backspacing)
        Nothing

    else
        case maybeInt of
            Just i ->
                maybeInt

            Nothing ->
                default
