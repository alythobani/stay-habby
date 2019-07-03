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
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData
import Set
import Task
import TimeZone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddHabit updater =
            { model | addHabit = updater model.addHabit }

        updateEditGoal updater =
            { model | editGoal = updater model.editGoal }

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

        getFrequencyStatsOnDate : YmdDate.YmdDate -> List String -> Cmd Msg
        getFrequencyStatsOnDate ymd habitIds =
            Api.queryFrequencyStats
                ymd
                habitIds
                model.apiBaseUrl
                OnGetFrequencyStatsFailure
                OnGetFrequencyStatsSuccess

        getFrequencyStats : List String -> Cmd Msg
        getFrequencyStats habitIds =
            case model.selectedYmd of
                Just ymd ->
                    getFrequencyStatsOnDate ymd habitIds

                Nothing ->
                    Cmd.none

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
                    in
                    ( { model
                        | currentTimeZone = Just timeZone
                        , selectedYmd = Just currentYmd
                        , actualYmd = Just currentYmd
                        , chooseDateDialogChosenYmd = Just currentYmd
                        , allHabits = RemoteData.Loading
                        , allHabitData = RemoteData.Loading
                        , allFrequencyStats = RemoteData.Loading
                        , allHabitDayNotes = RemoteData.Loading
                      }
                    , Api.queryAllRemoteData
                        currentYmd
                        model.apiBaseUrl
                        OnGetAllRemoteDataFailure
                        OnGetAllRemoteDataSuccess
                    )

        OnUrlChange url ->
            -- TODO
            ( model, Cmd.none )

        OnUrlRequest urlRequest ->
            -- TODO
            ( model, Cmd.none )

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
            ( { model | openTopPanelDateDropdown = not model.openTopPanelDateDropdown }, Cmd.none )

        ChangeSelectedYmd newYmd ->
            if model.selectedYmd == Just newYmd then
                -- Already on desired date, no need to re-query any data
                ( model, Cmd.none )

            else
                ( { model
                    | selectedYmd = Just newYmd
                    , allFrequencyStats = RemoteData.Loading
                  }
                , getFrequencyStatsOnDate newYmd []
                )

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
            ( { model
                | activeDialogScreen = Just DialogScreen.ChooseDateDialogScreen
                , openTopPanelDateDropdown = False
                , chooseDateDialogChosenYmd = model.selectedYmd
              }
            , Dom.focus "choose-date-dialog-form-id"
                |> Task.attempt FocusResult
            )

        OnChooseDateDialogScreenKeydown key ->
            case ( model.actualYmd, model.chooseDateDialogChosenYmd ) of
                ( Just actualYmd, Just chosenYmd ) ->
                    if key == Keyboard.KeyT then
                        update (SetChooseDateDialogChosenYmd actualYmd) model

                    else if key == Keyboard.ArrowDown then
                        update (SetChooseDateDialogChosenYmd (YmdDate.addDays 7 chosenYmd)) model

                    else if key == Keyboard.ArrowUp then
                        update (SetChooseDateDialogChosenYmd (YmdDate.addDays -7 chosenYmd)) model

                    else if key == Keyboard.ArrowLeft then
                        update (SetChooseDateDialogChosenYmd (YmdDate.addDays -1 chosenYmd)) model

                    else if key == Keyboard.ArrowRight then
                        update (SetChooseDateDialogChosenYmd (YmdDate.addDays 1 chosenYmd)) model

                    else if key == Keyboard.Enter then
                        update (OnChooseDateDialogSubmitClick chosenYmd) model

                    else if key == Keyboard.Escape then
                        update OnExitDialogScreen model

                    else
                        ( model, Cmd.none )

                _ ->
                    if key == Keyboard.Escape then
                        update OnExitDialogScreen model

                    else
                        ( model, Cmd.none )

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

        OnChooseDateDialogSubmitClick chosenYmd ->
            let
                newDialogScreenModel =
                    { model | activeDialogScreen = Nothing }
            in
            update (ChangeSelectedYmd chosenYmd) newDialogScreenModel

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
            ( { model | activeDialogScreen = Just DialogScreen.AddNewHabitScreen }
            , Dom.focus "add-habit-form-body-name-input" |> Task.attempt FocusResult
            )

        OnAddHabitFormKeydown key ->
            if key == Keyboard.Escape then
                update OnExitDialogScreen model

            else if key == Keyboard.Enter then
                case Habit.extractCreateHabit model.addHabit of
                    Just createHabitData ->
                        update (OnAddHabitSubmit createHabitData) model

                    Nothing ->
                        ( model, Cmd.none )

            else
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

        OnAddHabitSubmit createHabitData ->
            case model.actualYmd of
                Just actualYmd ->
                    ( model
                    , Api.mutationAddHabit createHabitData actualYmd model.apiBaseUrl OnAddHabitFailure OnAddHabitSuccess
                    )

                Nothing ->
                    ( { model | errorMessage = Just "Error adding habit: current date not available" }
                    , Cmd.none
                    )

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
            , getFrequencyStats [ habitRecord.id ]
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
            case maybeNewVal of
                Nothing ->
                    ( model, Cmd.none )

                Just newVal ->
                    ( model
                    , Api.mutationSetHabitData
                        selectedYmd
                        habitId
                        newVal
                        model.apiBaseUrl
                        OnSetHabitDataFailure
                        OnSetHabitDataSuccess
                    )

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
            , getFrequencyStats [ updatedHabitDatum.habitId ]
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
            ( { model | habitActionsDropdown = updatedHabitActionsDropdown }, Cmd.none )

        -- Dark Mode
        OnToggleDarkMode ->
            ( { model | darkModeOn = not model.darkModeOn }, Cmd.none )

        -- Keyboard
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
                    case model.activeDialogScreen of
                        Just DialogScreen.SuspendOrResumeConfirmationScreen ->
                            update (OnSuspendOrResumeConfirmationScreenKeydown key) newModel

                        Just DialogScreen.EditGoalScreen ->
                            update (OnEditGoalScreenKeydown key) newModel

                        Just DialogScreen.AddNewHabitScreen ->
                            update (OnAddHabitFormKeydown key) newModel

                        Just DialogScreen.ChooseDateDialogScreen ->
                            update (OnChooseDateDialogScreenKeydown key) newModel

                        Just DialogScreen.AddNoteScreen ->
                            update (OnAddNoteKeydown key) newModel

                        Just screen ->
                            -- A dialog screen is already open
                            if key == Keyboard.Escape then
                                update OnExitDialogScreen newModel

                            else
                                ( newModel, Cmd.none )

                        Nothing ->
                            -- No dialog screen is open (user is on main screen)
                            update (OnMainScreenKeydown key) newModel

                _ ->
                    ( newModel, Cmd.none )

        OnMainScreenKeydown key ->
            if key == Keyboard.KeyA then
                update OpenSetHabitDataShortcutHabitSelectionScreen model

            else if key == Keyboard.KeyD then
                update OnToggleDarkMode model

            else if key == Keyboard.KeyE then
                update OpenEditGoalHabitSelectionScreen model

            else if key == Keyboard.KeyG then
                update OpenGraphHabitSelectionScreen model

            else if key == Keyboard.KeyH then
                update OpenAddHabitForm model

            else if key == Keyboard.KeyN then
                update OpenAddNoteHabitSelectionDialogScreen model

            else if key == Keyboard.KeyC then
                update OpenChooseCustomDateDialog model

            else if key == Keyboard.KeyS then
                update OpenSuspendOrResumeHabitSelectionScreen model

            else
                ( model, Cmd.none )

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
            ( { model | activeDialogScreen = Just DialogScreen.SetHabitDataShortcutHabitSelectionScreen }
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

        OpenSetHabitDataShortcutAmountScreen habit ->
            ( { model
                | activeDialogScreen = Just DialogScreen.SetHabitDataShortcutAmountScreen
                , setHabitDataShortcutAmountScreenHabit = Just habit
              }
            , Dom.focus
                "set-habit-data-shortcut-amount-screen-input"
                |> Task.attempt FocusResult
            )

        OnSetHabitDataShortcutAmountScreenInput newInput ->
            let
                newInputInt =
                    extractInt newInput model.setHabitDataShortcutAmountScreenInputInt
            in
            ( { model | setHabitDataShortcutAmountScreenInputInt = newInputInt }
            , Cmd.none
            )

        OnSetHabitDataShortcutAmountScreenSubmit ymd habitId newVal ->
            ( { model
                | activeDialogScreen = Nothing
                , setHabitDataShortcutAmountScreenInputInt = Nothing
                , setHabitDataShortcutHabitNameFilterText = ""
                , setHabitDataShortcutFilteredHabits =
                    case model.allHabits of
                        RemoteData.Success habits ->
                            Array.fromList habits

                        _ ->
                            Array.empty
                , setHabitDataShortcutSelectedHabitIndex = 0
              }
            , Api.mutationSetHabitData
                ymd
                habitId
                newVal
                model.apiBaseUrl
                OnSetHabitDataFailure
                OnSetHabitDataSuccess
            )

        -- Edit Goal Habit Selection
        OpenEditGoalHabitSelectionScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.EditGoalHabitSelectionScreen }
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
                                | activeDialogScreen = Just DialogScreen.EditGoalScreen
                                , editGoalDialogHabit = Just habit
                                , habitActionsDropdown = Nothing
                                , editGoalDialogHabitCurrentFcrWithIndex = currentFcrWithIndex
                            }

                        ( newConfirmationMessageModel, newCmdMsg ) =
                            update RefreshEditGoalConfirmationMessageAndNewSuspensions newModel
                    in
                    ( { newConfirmationMessageModel
                        | activeDialogScreen = Just DialogScreen.EditGoalScreen
                        , editGoalDialogHabit = Just habit
                        , habitActionsDropdown = Nothing
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

        OnEditGoalScreenKeydown key ->
            if key == Keyboard.Enter then
                update OnEditGoalSubmit model

            else if key == Keyboard.Escape then
                update OnExitDialogScreen model

            else if key == Keyboard.KeyX then
                update (OnEditGoalSelectFrequencyKind Habit.TotalWeekFrequencyKind) model

            else if key == Keyboard.KeyS then
                update (OnEditGoalSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind) model

            else if key == Keyboard.KeyY then
                update (OnEditGoalSelectFrequencyKind Habit.EveryXDayFrequencyKind) model

            else
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
            , getFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
            )

        OnEditGoalSubmit ->
            case ( model.editGoalDialogHabit, model.editGoalNewFrequenciesList ) of
                ( Just habit, Just newFrequenciesList ) ->
                    let
                        ( habitId, habitType ) =
                            case habit of
                                Habit.GoodHabit gh ->
                                    ( gh.id, "good_habit" )

                                Habit.BadHabit bh ->
                                    ( bh.id, "bad_habit" )
                    in
                    ( { model
                        | activeDialogScreen = Nothing
                      }
                    , Api.mutationEditHabitGoalFrequencies
                        habitId
                        newFrequenciesList
                        habitType
                        model.apiBaseUrl
                        OnEditGoalFailure
                        OnEditGoalSuccess
                    )

                ( Nothing, _ ) ->
                    ( { model | errorMessage = Just "Error editing habit goal: no habit selected" }
                    , Cmd.none
                    )

                _ ->
                    ( { model | errorMessage = Just "Error editing habit goal: could not generate new list of goals" }
                    , Cmd.none
                    )

        -- Error Messages
        OpenErrorMessageDialogScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.ErrorMessageScreen }
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
            in
            ( { model | activeDialogScreen = Nothing }
            , if shouldAttemptFocus then
                Dom.focus "first-habit-amount-input" |> Task.attempt FocusResult

              else
                Cmd.none
            )

        -- Add Note Habit Selection
        OpenAddNoteHabitSelectionDialogScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.AddNoteHabitSelectionScreen }
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
            in
            ( { model
                | activeDialogScreen = Just DialogScreen.AddNoteScreen
                , addNoteDialogHabit = Just habit
                , addNoteDialogInput = Maybe.withDefault "" existingHabitDayNoteText
                , habitActionsDropdown = Nothing
              }
            , Dom.focus "add-note-dialog-input-id"
                |> Task.attempt FocusResult
            )

        OnAddNoteKeydown key ->
            let
                modelKeysDownList : List Keyboard.Key
                modelKeysDownList =
                    Set.toList model.keysDown |> List.map Keyboard.fromCode

                specialKeys =
                    [ Keyboard.OSLeft
                    , Keyboard.OSRight
                    , Keyboard.MetaLeft
                    , Keyboard.MetaRight
                    , Keyboard.ControlLeft
                    , Keyboard.ControlRight
                    ]

                specialKeyIsPressed : Bool
                specialKeyIsPressed =
                    List.any (\specialKey -> List.member specialKey modelKeysDownList) specialKeys
            in
            if key == Keyboard.Enter then
                if specialKeyIsPressed then
                    update OnAddNoteSubmit model

                else
                    ( model, Cmd.none )

            else if key == Keyboard.Escape then
                update OnExitDialogScreen model

            else
                ( model, Cmd.none )

        OnAddNoteDialogInput newAddNoteInput ->
            ( { model | addNoteDialogInput = newAddNoteInput }, Cmd.none )

        OnAddNoteSubmit ->
            case ( String.isEmpty model.addNoteDialogInput, model.selectedYmd, model.addNoteDialogHabit ) of
                ( False, Just selectedYmd, Just habit ) ->
                    ( { model
                        | activeDialogScreen = Nothing
                        , addNoteHabitSelectionFilterText = ""
                        , addNoteHabitSelectionFilteredHabits =
                            case model.allHabits of
                                RemoteData.Success habits ->
                                    Array.fromList habits

                                _ ->
                                    Array.empty
                      }
                    , Api.mutationSetHabitDayNote
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
            ( { model | activeDialogScreen = Just DialogScreen.SuspendOrResumeHabitSelectionScreen }
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
                    in
                    ( { model
                        | activeDialogScreen = Just DialogScreen.SuspendOrResumeConfirmationScreen
                        , habitActionsDropdown = Nothing
                        , suspendOrResumeHabit = Just habit
                        , suspendOrResumeHabitConfirmationMessage = confirmationMessage
                        , suspendOrResumeHabitNewSuspensions = Just <| Array.toList newSuspensionsArray
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnSuspendOrResumeConfirmationScreenKeydown key ->
            if key == Keyboard.Enter then
                case ( model.suspendOrResumeHabit, model.suspendOrResumeHabitNewSuspensions ) of
                    ( Just habit, Just newSuspensions ) ->
                        let
                            habitRecord =
                                Habit.getCommonFields habit
                        in
                        update (OnResumeOrSuspendSubmitClick habitRecord.id newSuspensions) model

                    _ ->
                        ( model, Cmd.none )

            else if key == Keyboard.Escape then
                update OnExitDialogScreen model

            else
                ( model, Cmd.none )

        OnResumeOrSuspendSubmitClick habitId newSuspensionsList ->
            ( { model
                | activeDialogScreen = Nothing
                , suspendOrResumeHabit = Nothing
                , suspendOrResumeHabitConfirmationMessage = ""
                , suspendOrResumeHabitNewSuspensions = Nothing
              }
            , Api.mutationEditHabitSuspensions
                habitId
                newSuspensionsList
                model.apiBaseUrl
                OnResumeOrSuspendHabitFailure
                OnResumeOrSuspendHabitSuccess
            )

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
            , getFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
            )

        -- Graph Habit Selection Screen
        OpenGraphHabitSelectionScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.GraphHabitSelectionScreen }
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

        -- Graph Dialog Screen
        OpenGraphDialogScreen habit ->
            ( { model
                | activeDialogScreen = Just DialogScreen.GraphDialogScreen
                , graphHabit = Just habit
                , habitActionsDropdown = Nothing
              }
            , Cmd.none
            )


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
