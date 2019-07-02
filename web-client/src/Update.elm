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
import Task
import TimeZone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddHabit updater =
            { model | addHabit = updater model.addHabit }

        updateEditGoal updater =
            { model | editGoal = updater model.editGoal }

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

        OnChooseCustomDateClick ->
            ( { model
                | activeDialogScreen = Just DialogScreen.ChooseDateDialogScreen
                , openTopPanelDateDropdown = False
                , chooseDateDialogChosenYmd = model.selectedYmd
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
            ( { model
                | allHabits = RemoteData.Success habits
                , allHabitData = RemoteData.Success habitData
                , allFrequencyStats = RemoteData.Success frequencyStatsList
                , allHabitDayNotes = RemoteData.Success habitDayNotes
                , setHabitDataShortcutFilteredHabits = Array.fromList habits
                , editGoalHabitSelectionFilteredHabits = Array.fromList habits
                , addNoteHabitSelectionFilteredHabits = Array.fromList habits
                , suspendOrResumeHabitSelectionFilteredHabits = Array.fromList habits
              }
            , Cmd.none
            )

        -- Add Habit
        OnOpenAddHabit ->
            ( updateAddHabit (\addHabit -> { addHabit | openView = True }), Cmd.none )

        OnCancelAddHabit ->
            ( updateAddHabit (\addHabit -> { addHabit | openView = False }), Cmd.none )

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

        AddHabit createHabitData ->
            case model.actualYmd of
                Just ymd ->
                    ( model
                    , Api.mutationAddHabit createHabitData ymd model.apiBaseUrl OnAddHabitFailure OnAddHabitSuccess
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
            in
            ( { model
                | allHabits = RemoteData.map (\allHabits -> allHabits ++ [ habit ]) model.allHabits
                , addHabit = Habit.initAddHabitData
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

        -- Dropdowns
        ToggleHabitActionsDropdown habitId ->
            let
                updatedHabitActionsDropdown =
                    if model.habitActionsDropdown == Just habitId then
                        Nothing

                    else
                        Just habitId
            in
            ( { model | habitActionsDropdown = updatedHabitActionsDropdown }, Cmd.none )

        OnToggleDarkMode ->
            ( { model | darkModeOn = not model.darkModeOn }, Cmd.none )

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

                        Just screen ->
                            -- A dialog screen is already open
                            if key == Keyboard.Escape then
                                update OnExitDialogScreen newModel

                            else
                                ( newModel, Cmd.none )

                        Nothing ->
                            -- No dialog screen is open, see if the user wants to open one
                            if key == Keyboard.KeyA then
                                update OpenSetHabitDataShortcutHabitSelectionScreen newModel

                            else if key == Keyboard.KeyE then
                                update OpenEditGoalHabitSelectionScreen newModel

                            else if key == Keyboard.KeyN then
                                update OpenAddNoteHabitSelectionDialogScreen newModel

                            else if key == Keyboard.KeyC then
                                update OnChooseCustomDateClick newModel

                            else if key == Keyboard.KeyS then
                                update OpenSuspendOrResumeHabitSelectionScreen newModel

                            else
                                ( newModel, Cmd.none )

                _ ->
                    ( newModel, Cmd.none )

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

                        currentGoal : Maybe Habit.FrequencyChangeRecord
                        currentGoal =
                            case habit of
                                Habit.GoodHabit goodHabit ->
                                    List.head <| List.reverse goodHabit.targetFrequencies

                                Habit.BadHabit badHabit ->
                                    List.head <| List.reverse badHabit.thresholdFrequencies

                        ( newEditGoalFrequencyKind, idToFocusOn ) =
                            case currentGoal of
                                Just fcr ->
                                    case fcr.newFrequency of
                                        Habit.TotalWeekFrequency f ->
                                            ( Just Habit.TotalWeekFrequencyKind
                                            , Just "edit-goal-dialog-x-per-week-input"
                                            )

                                        Habit.SpecificDayOfWeekFrequency f ->
                                            ( Just Habit.SpecificDayOfWeekFrequencyKind
                                            , Just "edit-goal-dialog-monday-input"
                                            )

                                        Habit.EveryXDayFrequency f ->
                                            ( Just Habit.EveryXDayFrequencyKind
                                            , Just "edit-goal-dialog-every-x-days-times-input"
                                            )

                                Nothing ->
                                    ( Nothing, Nothing )

                        newModel =
                            case newEditGoalFrequencyKind of
                                Just fk ->
                                    updateEditGoal (\editGoal -> { editGoal | frequencyKind = fk })

                                Nothing ->
                                    model
                    in
                    ( { newModel
                        | activeDialogScreen = Just DialogScreen.EditGoalScreen
                        , editGoalDialogHabit = Just habit
                        , habitActionsDropdown = Nothing
                      }
                    , case idToFocusOn of
                        Just domId ->
                            Dom.focus domId
                                |> Task.attempt FocusResult

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnEditGoalScreenKeydown key ->
            if key == Keyboard.Enter then
                update OnEditGoalSubmit model

            else if key == Keyboard.Escape then
                update OnExitDialogScreen model

            else if key == Keyboard.ArrowLeft then
                case model.editGoal.frequencyKind of
                    Habit.EveryXDayFrequencyKind ->
                        update (OnEditGoalSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind) model

                    Habit.SpecificDayOfWeekFrequencyKind ->
                        update (OnEditGoalSelectFrequencyKind Habit.TotalWeekFrequencyKind) model

                    Habit.TotalWeekFrequencyKind ->
                        update (OnEditGoalSelectFrequencyKind Habit.EveryXDayFrequencyKind) model

            else if key == Keyboard.ArrowRight then
                case model.editGoal.frequencyKind of
                    Habit.EveryXDayFrequencyKind ->
                        update (OnEditGoalSelectFrequencyKind Habit.TotalWeekFrequencyKind) model

                    Habit.SpecificDayOfWeekFrequencyKind ->
                        update (OnEditGoalSelectFrequencyKind Habit.EveryXDayFrequencyKind) model

                    Habit.TotalWeekFrequencyKind ->
                        update (OnEditGoalSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind) model

            else
                ( model, Cmd.none )

        OnEditGoalSelectFrequencyKind frequencyKind ->
            ( updateEditGoal (\editGoal -> { editGoal | frequencyKind = frequencyKind })
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
            )

        OnEditGoalTimesPerWeekInput timesPerWeek ->
            ( updateEditGoal (\editGoal -> { editGoal | timesPerWeek = extractInt timesPerWeek editGoal.timesPerWeek })
            , Cmd.none
            )

        OnEditGoalSpecificDayMondayInput mondayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | mondayTimes = extractInt mondayTimes editGoal.mondayTimes })
            , Cmd.none
            )

        OnEditGoalSpecificDayTuesdayInput tuesdayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | tuesdayTimes = extractInt tuesdayTimes editGoal.tuesdayTimes })
            , Cmd.none
            )

        OnEditGoalSpecificDayWednesdayInput wednesdayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | wednesdayTimes = extractInt wednesdayTimes editGoal.wednesdayTimes })
            , Cmd.none
            )

        OnEditGoalSpecificDayThursdayInput thursdayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | thursdayTimes = extractInt thursdayTimes editGoal.thursdayTimes })
            , Cmd.none
            )

        OnEditGoalSpecificDayFridayInput fridayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | fridayTimes = extractInt fridayTimes editGoal.fridayTimes })
            , Cmd.none
            )

        OnEditGoalSpecificDaySaturdayInput saturdayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | saturdayTimes = extractInt saturdayTimes editGoal.saturdayTimes })
            , Cmd.none
            )

        OnEditGoalSpecificDaySundayInput sundayTimes ->
            ( updateEditGoal (\editGoal -> { editGoal | sundayTimes = extractInt sundayTimes editGoal.sundayTimes })
            , Cmd.none
            )

        OnEditGoalTimesInput times ->
            ( updateEditGoal (\editGoal -> { editGoal | times = extractInt times editGoal.times })
            , Cmd.none
            )

        OnEditGoalDaysInput days ->
            ( updateEditGoal (\editGoal -> { editGoal | days = extractInt days editGoal.days })
            , Cmd.none
            )

        OnEditGoalFailure apiError ->
            ( { model | errorMessage = Just <| "Error editing habit goal: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnEditGoalSuccess habit ->
            ( { model
                | allHabits =
                    RemoteData.map
                        (\allHabits ->
                            Util.replaceOrAdd
                                allHabits
                                (\oldHabit -> (oldHabit |> Habit.getCommonFields |> .id) == (habit |> Habit.getCommonFields |> .id))
                                habit
                        )
                        model.allHabits
                , editGoal = Habit.initEditGoalData
                , activeDialogScreen = Nothing
              }
            , getFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
            )

        OnEditGoalSubmit ->
            -- case (model.editGoalDialogHabit, Habit.extractNewGoal model.editGoal) of
            --     (Just habit, Just newGoal) ->
            --         let
            --             oldFrequencies : List Habit.FrequencyChangeRecord
            --             oldFrequencies =
            --                 case habit of
            --                     Habit.GoodHabit gh ->
            --                         gh.targetFrequencies
            --
            --                     Habit.BadHabit bh ->
            --                         bh.thresholdFrequencies
            --         in
            --
            --
            --     Nothing ->
            -- TODO
            ( model, Cmd.none )

        -- ( { model
        --     | activeDialogScreen = Nothing
        --   }
        -- , Api.mutationEditHabitGoalFrequencies habitId newFrequencies habitType model.apiBaseUrl OnEditGoalFailure OnEditGoalSuccess
        -- )
        -- Error Messages
        OpenErrorMessageDialogScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.ErrorMessageScreen }
            , Cmd.none
            )

        -- Full screen dialogs
        OnExitDialogScreen ->
            ( { model | activeDialogScreen = Nothing }, Cmd.none )

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
                , addNoteKeysDown = []
                , habitActionsDropdown = Nothing
              }
            , Dom.focus "add-note-dialog-input-id"
                |> Task.attempt FocusResult
            )

        OnAddNoteDialogInput newAddNoteInput ->
            ( { model | addNoteDialogInput = newAddNoteInput }, Cmd.none )

        OnAddNoteKeydown key ymd habitId ->
            let
                newAddNoteKeysDown =
                    Util.addToListIfNotPresent model.addNoteKeysDown key

                -- Keyboard shortcut for Add Note Submit is cmd-enter (mac) or ctrl-enter
                isAddNoteSubmitShortcut : Bool
                isAddNoteSubmitShortcut =
                    (key == Keyboard.Enter)
                        && List.any
                            (\specialKey -> List.member specialKey model.addNoteKeysDown)
                            [ Keyboard.OSLeft
                            , Keyboard.OSRight
                            , Keyboard.MetaLeft
                            , Keyboard.MetaRight
                            , Keyboard.ControlLeft
                            , Keyboard.ControlRight
                            ]

                newModel =
                    { model | addNoteKeysDown = newAddNoteKeysDown }
            in
            if isAddNoteSubmitShortcut && (model.addNoteDialogInput /= "") then
                update (OnAddNoteSubmitClick ymd habitId model.addNoteDialogInput) newModel

            else
                ( newModel, Cmd.none )

        OnAddNoteKeyup key ->
            ( { model | addNoteKeysDown = Util.removeFromListIfPresent model.addNoteKeysDown key }, Cmd.none )

        OnAddNoteSubmitClick ymd habitId note ->
            ( { model
                | activeDialogScreen = Nothing
                , addNoteKeysDown = []
                , addNoteHabitSelectionFilterText = ""
                , addNoteHabitSelectionFilteredHabits =
                    case model.allHabits of
                        RemoteData.Success habits ->
                            Array.fromList habits

                        _ ->
                            Array.empty
              }
            , Api.mutationSetHabitDayNote ymd habitId note model.apiBaseUrl OnAddNoteFailure OnAddNoteSuccess
            )

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
                replaceHabitInList habitList =
                    Util.replaceOrAdd
                        habitList
                        (\oldHabit -> (oldHabit |> Habit.getCommonFields |> .id) == (habit |> Habit.getCommonFields |> .id))
                        habit

                replaceHabitInArray habitArray =
                    habitArray |> Array.toList |> replaceHabitInList |> Array.fromList
            in
            ( { model
                | allHabits =
                    RemoteData.map
                        replaceHabitInList
                        model.allHabits
                , setHabitDataShortcutFilteredHabits = replaceHabitInArray model.setHabitDataShortcutFilteredHabits
                , editGoalHabitSelectionFilteredHabits = replaceHabitInArray model.editGoalHabitSelectionFilteredHabits
                , addNoteHabitSelectionFilteredHabits = replaceHabitInArray model.addNoteHabitSelectionFilteredHabits
                , suspendOrResumeHabitSelectionFilteredHabits = replaceHabitInArray model.suspendOrResumeHabitSelectionFilteredHabits
              }
            , getFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
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
