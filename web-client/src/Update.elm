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

        getTodayViewerFrequencyStats : List String -> Cmd Msg
        getTodayViewerFrequencyStats habitIds =
            case model.ymd of
                Just ymd ->
                    Api.queryPastFrequencyStats
                        ymd
                        habitIds
                        model.apiBaseUrl
                        OnGetTodayFrequencyStatsFailure
                        OnGetTodayFrequencyStatsSuccess

                Nothing ->
                    Cmd.none

        getHistoryViewerFrequencyStats : YmdDate.YmdDate -> List String -> Cmd Msg
        getHistoryViewerFrequencyStats ymd habitIds =
            Api.queryPastFrequencyStats
                ymd
                habitIds
                model.apiBaseUrl
                OnGetPastFrequencyStatsFailure
                OnGetPastFrequencyStatsSuccess
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnTimeZoneRetrieval result ->
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
                    if model.ymd == Just currentYmd then
                        -- Date hasn't changed. No need to re-query things.
                        ( { model | currentTimeZone = Just timeZone }, Cmd.none )

                    else
                        ( { model
                            | currentTimeZone = Just timeZone
                            , ymd = Just currentYmd
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

        TickMinute posix ->
            ( { model | currentPosix = posix }
            , Task.attempt OnTimeZoneRetrieval TimeZone.getZone
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
            ( { model
                | allHabits = RemoteData.Success habits
                , allHabitData = RemoteData.Success habitData
                , allFrequencyStats = RemoteData.Success frequencyStatsList
                , allHabitDayNotes = RemoteData.Success habitDayNotes
                , setHabitDataShortcutFilteredHabits = Array.fromList habits
                , addNoteHabitSelectionFilteredHabits = Array.fromList habits
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
            case model.ymd of
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
            , Cmd.batch
                [ getTodayViewerFrequencyStats [ habitRecord.id ]
                , case model.historyViewerSelectedDate of
                    Just ymd ->
                        getHistoryViewerFrequencyStats ymd [ habitRecord.id ]

                    Nothing ->
                        Cmd.none
                ]
            )

        -- Set Habit Data
        OnHabitDataInput habitID newVal ->
            let
                newEditingTodayHabitAmount amount =
                    model.editingTodayHabitAmount
                        |> Dict.update habitID (always <| amount)
            in
            if String.isEmpty newVal then
                ( { model | editingTodayHabitAmount = newEditingTodayHabitAmount Nothing }, Cmd.none )

            else
                case String.toInt newVal of
                    Nothing ->
                        ( model, Cmd.none )

                    Just newInt ->
                        ( { model | editingTodayHabitAmount = newEditingTodayHabitAmount <| Just newInt }, Cmd.none )

        SetHabitData ymd habitId maybeNewVal ->
            case maybeNewVal of
                Nothing ->
                    ( model, Cmd.none )

                Just newVal ->
                    ( model
                    , Api.mutationSetHabitData
                        ymd
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
                        , editingTodayHabitAmount =
                            Dict.update updatedHabitDatum.habitId (always Nothing) model.editingTodayHabitAmount
                        , editingHistoryHabitAmount =
                            Dict.update
                                (YmdDate.toSimpleString updatedHabitDatum.date)
                                (Maybe.map (Dict.update updatedHabitDatum.habitId (always Nothing)))
                                model.editingHistoryHabitAmount
                    }
            in
            ( newModel
            , Cmd.batch
                [ getTodayViewerFrequencyStats [ updatedHabitDatum.habitId ]
                , case newModel.historyViewerSelectedDate of
                    Just ymd ->
                        getHistoryViewerFrequencyStats ymd [ updatedHabitDatum.habitId ]

                    Nothing ->
                        Cmd.none
                ]
            )

        --
        OnToggleHistoryViewer ->
            ( { model | openHistoryViewer = not model.openHistoryViewer }
            , Cmd.none
            )

        OnToggleTodayViewer ->
            ( { model | openTodayViewer = not model.openTodayViewer }, Cmd.none )

        OnHistoryViewerDateInput newDateInput ->
            ( { model
                | historyViewerDateInput =
                    newDateInput
                        |> String.filter
                            (\char -> List.member char [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '/' ])
              }
            , Cmd.none
            )

        OnHistoryViewerSelectYesterday ->
            case model.ymd of
                Just ymd ->
                    let
                        yesterday : YmdDate.YmdDate
                        yesterday =
                            YmdDate.addDays -1 ymd
                    in
                    update (SetHistoryViewerSelectedDate yesterday) model

                Nothing ->
                    ( { model | errorMessage = Just "Error selecting yesterday: current date not available" }
                    , Cmd.none
                    )

        OnHistoryViewerSelectBeforeYesterday ->
            case model.ymd of
                Just ymd ->
                    let
                        beforeYesterday : YmdDate.YmdDate
                        beforeYesterday =
                            YmdDate.addDays -2 ymd
                    in
                    update (SetHistoryViewerSelectedDate beforeYesterday) model

                Nothing ->
                    ( { model | errorMessage = Just "Error selecting before yesterday: current date not available" }
                    , Cmd.none
                    )

        OnHistoryViewerSelectDateInput ->
            let
                maybeYmd =
                    YmdDate.fromSimpleString model.historyViewerDateInput
            in
            case maybeYmd of
                Just ymd ->
                    update (SetHistoryViewerSelectedDate ymd) model

                Nothing ->
                    ( { model | errorMessage = Just "Error selecting date: invalid input" }
                    , Cmd.none
                    )

        SetHistoryViewerSelectedDate ymd ->
            ( { model | historyViewerSelectedDate = Just ymd }
            , getHistoryViewerFrequencyStats ymd []
            )

        OnGetTodayFrequencyStatsFailure apiError ->
            ( { model | errorMessage = Just <| "Error retrieving performance stats: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnGetTodayFrequencyStatsSuccess { frequencyStatsList } ->
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

        OnGetPastFrequencyStatsFailure apiError ->
            ( { model
                | historyViewerFrequencyStats = RemoteData.Failure apiError
                , errorMessage = Just <| "Error retrieving past performance stats: " ++ ApiError.toString apiError
              }
            , Cmd.none
            )

        OnGetPastFrequencyStatsSuccess { frequencyStatsList } ->
            let
                updateHistoryViewerFrequencyStats : List FrequencyStats.FrequencyStats -> List FrequencyStats.FrequencyStats
                updateHistoryViewerFrequencyStats historyViewerFrequencyStats =
                    List.foldl
                        (\newStats statsList ->
                            Util.replaceOrAdd statsList (\stats -> stats.habitId == newStats.habitId) newStats
                        )
                        historyViewerFrequencyStats
                        frequencyStatsList
            in
            ( { model
                | historyViewerFrequencyStats =
                    case model.historyViewerFrequencyStats of
                        RemoteData.Success frequencyStatsFrequencyStatsList ->
                            RemoteData.map
                                updateHistoryViewerFrequencyStats
                                model.historyViewerFrequencyStats

                        _ ->
                            RemoteData.Success frequencyStatsList
              }
            , Cmd.none
            )

        OnHistoryViewerChangeDate ->
            ( { model | historyViewerSelectedDate = Nothing }, Cmd.none )

        OnHistoryViewerHabitDataInput forDate habitId newInput ->
            let
                editingHabitDataDict =
                    model.editingHistoryHabitAmount
                        |> Dict.get (YmdDate.toSimpleString forDate)
                        |> Maybe.withDefault Dict.empty

                newAmount =
                    extractInt newInput (Dict.get habitId editingHabitDataDict)

                updatedEditingHabitDataDict =
                    editingHabitDataDict |> Dict.update habitId (always newAmount)
            in
            ( { model
                | editingHistoryHabitAmount =
                    model.editingHistoryHabitAmount
                        |> Dict.update
                            (YmdDate.toSimpleString forDate)
                            (always <| Just updatedEditingHabitDataDict)
              }
            , Cmd.none
            )

        -- Dropdowns
        ToggleTodayViewerHabitActionsDropdown habitId ->
            let
                updatedTodayViewerHabitActionsDropdown =
                    if model.todayViewerHabitActionsDropdown == Just habitId then
                        Nothing

                    else
                        Just habitId
            in
            ( { model | todayViewerHabitActionsDropdown = updatedTodayViewerHabitActionsDropdown }, Cmd.none )

        ToggleHistoryViewerHabitActionsDropdown habitId ->
            let
                updatedHistoryViewerHabitActionsDropdown =
                    if model.historyViewerHabitActionsDropdown == Just habitId then
                        Nothing

                    else
                        Just habitId
            in
            ( { model | historyViewerHabitActionsDropdown = updatedHistoryViewerHabitActionsDropdown }, Cmd.none )

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
                    if key == Keyboard.KeyA then
                        if Maybe.isJust model.activeDialogScreen then
                            -- A dialog screen is already open, don't open the set habit data one
                            ( newModel, Cmd.none )

                        else
                            update OpenSetHabitDataShortcutDialogScreen newModel

                    else if key == Keyboard.KeyN then
                        if Maybe.isJust model.activeDialogScreen then
                            -- A dialog screen is already open, don't open the set habit data one
                            ( newModel, Cmd.none )

                        else
                            update OpenAddNoteHabitSelectionDialogScreen newModel

                    else if key == Keyboard.Escape then
                        update OnExitDialogScreen newModel

                    else
                        ( newModel, Cmd.none )

                _ ->
                    ( newModel, Cmd.none )

        FocusResult result ->
            case result of
                Result.Err (Dom.NotFound id) ->
                    ( model, Cmd.none )

                Result.Ok () ->
                    ( model, Cmd.none )

        -- Set Habit Data Shortcut
        OnSetHabitDataShortcutInput habitNameFilterText ->
            let
                habitFilter habit =
                    habit |> Habit.getCommonFields |> .name |> String.contains habitNameFilterText

                newFilteredHabits =
                    case model.allHabits of
                        RemoteData.Success habits ->
                            List.filter habitFilter habits

                        _ ->
                            []

                oldSelectedHabit =
                    Array.get model.setHabitDataShortcutSelectedHabitIndex model.setHabitDataShortcutFilteredHabits

                newSelectedHabitIndex =
                    case oldSelectedHabit of
                        Just h ->
                            Maybe.withDefault 0 (Util.firstIndexInList newFilteredHabits h)

                        Nothing ->
                            0
            in
            ( { model
                | setHabitDataShortcutHabitNameFilterText = habitNameFilterText
                , setHabitDataShortcutFilteredHabits = Array.fromList newFilteredHabits
                , setHabitDataShortcutSelectedHabitIndex = newSelectedHabitIndex
              }
            , Cmd.none
            )

        OnSetHabitDataShortcutSelectNextHabit ->
            let
                filteredHabitsLength =
                    Array.length model.setHabitDataShortcutFilteredHabits

                newSelectedHabitIndex =
                    modBy filteredHabitsLength (model.setHabitDataShortcutSelectedHabitIndex + 1)
            in
            ( { model | setHabitDataShortcutSelectedHabitIndex = newSelectedHabitIndex }
            , Cmd.none
            )

        OnSetHabitDataShortcutSelectPreviousHabit ->
            let
                filteredHabitsLength =
                    Array.length model.setHabitDataShortcutFilteredHabits

                newSelectedHabitIndex =
                    modBy filteredHabitsLength (model.setHabitDataShortcutSelectedHabitIndex - 1)
            in
            ( { model | setHabitDataShortcutSelectedHabitIndex = newSelectedHabitIndex }
            , Cmd.none
            )

        OnToggleShowSetHabitDataShortcutAmountForm ->
            let
                newShowSetHabitDataShortcutAmountForm =
                    not model.showSetHabitDataShortcutAmountForm
            in
            ( { model | showSetHabitDataShortcutAmountForm = newShowSetHabitDataShortcutAmountForm }
            , if newShowSetHabitDataShortcutAmountForm then
                Dom.focus "set-habit-data-shortcut-amount-form-input" |> Task.attempt FocusResult

              else
                Cmd.none
            )

        OnSetHabitDataShortcutAmountFormInput newInput ->
            let
                newSetHabitDataShortcutInputtedAmount =
                    extractInt newInput model.setHabitDataShortcutInputtedAmount
            in
            ( { model | setHabitDataShortcutInputtedAmount = newSetHabitDataShortcutInputtedAmount }
            , Cmd.none
            )

        OnSetHabitDataShortcutAmountFormSubmit ymd habitId maybeNewVal ->
            case maybeNewVal of
                Nothing ->
                    ( model, Cmd.none )

                Just newVal ->
                    ( { model
                        | activeDialogScreen = Nothing
                        , showSetHabitDataShortcutAmountForm = False
                        , setHabitDataShortcutInputtedAmount = Nothing
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

        -- Edit goal
        OnEditGoalClick habitId ->
            let
                habitFilter : Habit.Habit -> Bool
                habitFilter habit =
                    habit |> Habit.getCommonFields |> .id |> (==) habitId

                newEditGoalDialogHabit =
                    case model.allHabits of
                        RemoteData.Success habits ->
                            List.filter habitFilter habits
                                |> List.head

                        _ ->
                            Nothing

                currentGoal : Maybe Habit.FrequencyChangeRecord
                currentGoal =
                    case newEditGoalDialogHabit of
                        Just h ->
                            case h of
                                Habit.GoodHabit gh ->
                                    List.head <| List.reverse gh.targetFrequencies

                                Habit.BadHabit bh ->
                                    List.head <| List.reverse bh.thresholdFrequencies

                        Nothing ->
                            Nothing

                newEditGoalFrequencyKind : Maybe Habit.FrequencyKind
                newEditGoalFrequencyKind =
                    case currentGoal of
                        Just fcr ->
                            case fcr.newFrequency of
                                Habit.TotalWeekFrequency f ->
                                    Just Habit.TotalWeekFrequencyKind

                                Habit.SpecificDayOfWeekFrequency f ->
                                    Just Habit.SpecificDayOfWeekFrequencyKind

                                Habit.EveryXDayFrequency f ->
                                    Just Habit.EveryXDayFrequencyKind

                        Nothing ->
                            Nothing

                newModel =
                    case newEditGoalFrequencyKind of
                        Just fk ->
                            updateEditGoal (\editGoal -> { editGoal | frequencyKind = fk })

                        Nothing ->
                            model
            in
            ( { newModel
                | activeDialogScreen = Just DialogScreen.EditGoalScreen
                , editGoalDialogHabit = newEditGoalDialogHabit
                , todayViewerHabitActionsDropdown = Nothing
                , historyViewerHabitActionsDropdown = Nothing
              }
            , Cmd.none
            )

        CloseEditGoalDialog ->
            ( { model
                | activeDialogScreen = Nothing
              }
            , Cmd.none
            )

        OnEditGoalSelectFrequencyKind frequencyKind ->
            ( updateEditGoal (\editGoal -> { editGoal | frequencyKind = frequencyKind }), Cmd.none )

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
            , Cmd.batch
                [ getTodayViewerFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
                , case model.historyViewerSelectedDate of
                    Just ymd ->
                        getHistoryViewerFrequencyStats ymd [ habit |> Habit.getCommonFields |> .id ]

                    Nothing ->
                        Cmd.none
                ]
            )

        OnEditGoalSubmitClick habitId newFrequencies habitType ->
            ( { model
                | activeDialogScreen = Nothing
              }
            , Api.mutationEditHabitGoalFrequencies habitId newFrequencies habitType model.apiBaseUrl OnEditGoalFailure OnEditGoalSuccess
            )

        -- Suspending Habits
        OnResumeOrSuspendHabitClick habitId currentlySuspended onTodayViewer oldSuspensions ->
            case model.ymd of
                Just ymd ->
                    let
                        newDropdownsModel =
                            -- Close the Habit Actions Dropdown that was used to suspend/resume the habit
                            if onTodayViewer then
                                { model | todayViewerHabitActionsDropdown = Nothing }

                            else
                                { model | historyViewerHabitActionsDropdown = Nothing }

                        newSuspensions : List Habit.SuspendedInterval
                        newSuspensions =
                            if currentlySuspended then
                                -- User is trying to Resume the Habit
                                case List.reverse oldSuspensions of
                                    currSuspendedInterval :: rest ->
                                        -- Check if the last SuspendedInterval has been resumed yet
                                        case currSuspendedInterval.endDate of
                                            Just endDateYmd ->
                                                -- It's already been closed, nothing to do
                                                oldSuspensions

                                            Nothing ->
                                                -- We should close the suspended interval to resume the habit
                                                if YmdDate.compareYmds currSuspendedInterval.startDate ymd == LT then
                                                    -- The suspended interval started earlier than today.
                                                    -- To resume today, we set its endDate to yesterday.
                                                    let
                                                        yesterday =
                                                            YmdDate.addDays -1 ymd

                                                        newCurrSuspendedInterval =
                                                            { currSuspendedInterval | endDate = Just yesterday }
                                                    in
                                                    List.reverse <| newCurrSuspendedInterval :: rest

                                                else
                                                    -- it was suspended today (or later, but that shouldn't be possible), we can just get rid of it
                                                    List.take (List.length oldSuspensions - 1) oldSuspensions

                                    [] ->
                                        -- Habit has never been suspended before, so it's already active. Nothing to do
                                        []

                            else
                                -- User is trying to Suspend the Habit
                                case List.reverse oldSuspensions of
                                    currSuspendedInterval :: rest ->
                                        -- Check if the last SuspendedInterval has still in effect
                                        case currSuspendedInterval.endDate of
                                            Just endDateYmd ->
                                                -- The habit has been resumed.
                                                if YmdDate.compareYmds endDateYmd ymd == LT then
                                                    -- The SuspendedInterval's last day was yesterday or earlier.
                                                    -- To suspend the habit, we should add a new SuspendedInterval that starts today.
                                                    let
                                                        newSuspendedInterval =
                                                            { startDate = ymd, endDate = Nothing }
                                                    in
                                                    List.reverse <| newSuspendedInterval :: currSuspendedInterval :: rest

                                                else
                                                    -- The SuspendedInterval's last day is today (or later, but that shouldn't be possible).
                                                    -- This means the habit is already currently suspended, so there's nothing to do.
                                                    -- (For that reason this branch should never even execute)
                                                    oldSuspensions

                                            Nothing ->
                                                -- The last suspension is still in effect (so the habit is still suspended), nothing to do.
                                                oldSuspensions

                                    [] ->
                                        -- Let's give the Habit its first suspension
                                        [ { startDate = ymd, endDate = Nothing } ]
                    in
                    ( newDropdownsModel
                    , Api.mutationEditHabitSuspensions
                        habitId
                        newSuspensions
                        model.apiBaseUrl
                        OnResumeOrSuspendHabitFailure
                        OnResumeOrSuspendHabitSuccess
                    )

                Nothing ->
                    ( { model | errorMessage = Just "Error suspending/resuming habit: current date not available" }
                    , Cmd.none
                    )

        OnResumeOrSuspendHabitFailure apiError ->
            ( { model | errorMessage = Just <| "Error suspending/resuming habit: " ++ ApiError.toString apiError }
            , Cmd.none
            )

        OnResumeOrSuspendHabitSuccess habit ->
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
              }
            , Cmd.batch
                [ getTodayViewerFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
                , case model.historyViewerSelectedDate of
                    Just ymd ->
                        getHistoryViewerFrequencyStats ymd [ habit |> Habit.getCommonFields |> .id ]

                    Nothing ->
                        Cmd.none
                ]
            )

        -- Error Messages
        OpenErrorMessageDialogScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.ErrorMessageScreen }
            , Cmd.none
            )

        -- Full screen dialogs
        OnExitDialogScreen ->
            ( { model | activeDialogScreen = Nothing }, Cmd.none )

        OpenSetHabitDataShortcutDialogScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.SetHabitDataShortcutScreen }
            , Dom.focus
                (if model.showSetHabitDataShortcutAmountForm then
                    "set-habit-data-shortcut-amount-form-input"

                 else
                    "set-habit-data-shortcut-habit-selection-input"
                )
                |> Task.attempt FocusResult
            )

        OnExitSetHabitDataShortcutAmountFormInput ->
            ( { model
                | activeDialogScreen = Just DialogScreen.SetHabitDataShortcutScreen
                , showSetHabitDataShortcutAmountForm = False
              }
            , Dom.focus "set-habit-data-shortcut-habit-selection-input"
                |> Task.attempt FocusResult
            )

        -- Add Note Habit Selection
        OpenAddNoteHabitSelectionDialogScreen ->
            ( { model | activeDialogScreen = Just DialogScreen.AddNoteHabitSelectionScreen }
            , Dom.focus "add-note-habit-selection-filter-text-input"
                |> Task.attempt FocusResult
            )

        OnAddNoteHabitSelectionFilterTextInput habitNameFilterText ->
            let
                habitFilter habit =
                    habit |> Habit.getCommonFields |> .name |> String.contains habitNameFilterText

                newFilteredHabits =
                    case model.allHabits of
                        RemoteData.Success habits ->
                            List.filter habitFilter habits

                        _ ->
                            []

                oldSelectedHabit =
                    Array.get model.addNoteHabitSelectionSelectedHabitIndex model.addNoteHabitSelectionFilteredHabits

                newSelectedHabitIndex =
                    case oldSelectedHabit of
                        Just h ->
                            Maybe.withDefault 0 (Util.firstIndexInList newFilteredHabits h)

                        Nothing ->
                            0
            in
            ( { model
                | addNoteHabitSelectionFilterText = habitNameFilterText
                , addNoteHabitSelectionFilteredHabits = Array.fromList newFilteredHabits
                , addNoteHabitSelectionSelectedHabitIndex = newSelectedHabitIndex
              }
            , Cmd.none
            )

        OnAddNoteHabitSelectionScreenSelectNextHabit ->
            let
                filteredHabitsLength =
                    Array.length model.addNoteHabitSelectionFilteredHabits

                newSelectedHabitIndex =
                    modBy filteredHabitsLength (model.addNoteHabitSelectionSelectedHabitIndex + 1)
            in
            ( { model | addNoteHabitSelectionSelectedHabitIndex = newSelectedHabitIndex }
            , Cmd.none
            )

        OnAddNoteHabitSelectionScreenSelectPreviousHabit ->
            let
                filteredHabitsLength =
                    Array.length model.addNoteHabitSelectionFilteredHabits

                newSelectedHabitIndex =
                    modBy filteredHabitsLength (model.addNoteHabitSelectionSelectedHabitIndex - 1)
            in
            ( { model | addNoteHabitSelectionSelectedHabitIndex = newSelectedHabitIndex }
            , Cmd.none
            )

        -- Add Note Dialog
        OpenAddNoteDialog habit ->
            let
                habitRecord =
                    Habit.getCommonFields habit

                existingHabitDayNoteText : Maybe String
                existingHabitDayNoteText =
                    case ( model.allHabitDayNotes, model.ymd ) of
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
                , todayViewerHabitActionsDropdown = Nothing
                , historyViewerHabitActionsDropdown = Nothing
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
