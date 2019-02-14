module Update exposing (extractInt, update)

import Api
import Array
import Date
import DefaultServices.Infix exposing (..)
import DefaultServices.Util as Util
import Dict
import Dom
import Keyboard.Extra as KK
import Material
import Model exposing (Model)
import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddHabit updater =
            { model | addHabit = updater model.addHabit }

        updateEditGoal updater =
            { model | editGoal = updater model.editGoal }

        getTodayViewerFrequencyStats : List String -> Cmd Msg
        getTodayViewerFrequencyStats habitIds =
            Api.queryPastFrequencyStats
                model.ymd
                habitIds
                model.apiBaseUrl
                OnGetTodayFrequencyStatsFailure
                OnGetTodayFrequencyStatsSuccess

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

        OnLocationChange location ->
            -- TODO
            ( model, Cmd.none )

        TickMinute time ->
            let
                newYmd =
                    time |> Date.fromTime |> YmdDate.fromDate
            in
            if model.ymd /= newYmd then
                ( { model | ymd = newYmd, currentYmd = newYmd }
                , Api.queryHabitsAndHabitDataAndFrequencyStats
                    newYmd
                    model.apiBaseUrl
                    OnGetHabitsAndHabitDataAndFrequencyStatsFailure
                    OnGetHabitsAndHabitDataAndFrequencyStatsSuccess
                )

            else
                ( model, Cmd.none )

        OnGetHabitsAndHabitDataAndFrequencyStatsFailure apiError ->
            ( { model
                | allHabits = RemoteData.Failure apiError
                , allHabitData = RemoteData.Failure apiError
                , allFrequencyStats = RemoteData.Failure apiError
              }
            , Cmd.none
            )

        OnGetHabitsAndHabitDataAndFrequencyStatsSuccess { habits, habitData, frequencyStatsList } ->
            ( { model
                | allHabits = RemoteData.Success habits
                , allHabitData = RemoteData.Success habitData
                , allFrequencyStats = RemoteData.Success frequencyStatsList
                , setHabitDataShortcutFilteredHabits = Array.fromList habits
              }
            , Cmd.none
            )

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
            ( model
            , Api.mutationAddHabit createHabitData model.currentYmd model.apiBaseUrl OnAddHabitFailure OnAddHabitSuccess
            )

        OnAddHabitFailure apiError ->
            -- TODO
            ( model, Cmd.none )

        OnAddHabitSuccess habit ->
            ( { model
                | allHabits = RemoteData.map (\allHabits -> allHabits ++ [ habit ]) model.allHabits
                , addHabit = Habit.initAddHabitData
              }
            , Cmd.none
            )

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
                    Result.Err _ ->
                        ( model, Cmd.none )

                    Result.Ok newInt ->
                        ( { model | editingTodayHabitAmount = newEditingTodayHabitAmount <| Just newInt }, Cmd.none )

        SetHabitData ymd habitId newVal ->
            case newVal of
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
            -- TODO
            ( model, Cmd.none )

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
            newModel
                ! [ getTodayViewerFrequencyStats [ updatedHabitDatum.habitId ]
                  , case newModel.historyViewerSelectedDate of
                        Just ymd ->
                            getHistoryViewerFrequencyStats ymd [ updatedHabitDatum.habitId ]

                        Nothing ->
                            Cmd.none
                  ]

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
            let
                yesterday =
                    YmdDate.addDays -1 model.ymd
            in
            update (SetHistoryViewerSelectedDate yesterday) model

        OnHistoryViewerSelectBeforeYesterday ->
            let
                beforeYesterday =
                    YmdDate.addDays -2 model.ymd
            in
            update (SetHistoryViewerSelectedDate beforeYesterday) model

        OnHistoryViewerSelectDateInput ->
            let
                ymd =
                    YmdDate.fromSimpleString model.historyViewerDateInput
            in
            case ymd of
                Just ymd ->
                    update (SetHistoryViewerSelectedDate ymd) model

                Nothing ->
                    -- TODO: show error message because of invalid date input
                    ( model, Cmd.none )

        SetHistoryViewerSelectedDate ymd ->
            { model | historyViewerSelectedDate = Just ymd } ! [ getHistoryViewerFrequencyStats ymd [] ]

        OnGetTodayFrequencyStatsFailure apiError ->
            ( model, Cmd.none )

        OnGetTodayFrequencyStatsSuccess { frequencyStatsList } ->
            let
                updateAllFrequencyStats : List FrequencyStats.FrequencyStats -> List FrequencyStats.FrequencyStats
                updateAllFrequencyStats allFrequencyStats =
                    List.foldl
                        (\newStats allFrequencyStats ->
                            Util.replaceOrAdd allFrequencyStats (\stats -> stats.habitId == newStats.habitId) newStats
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
            ( { model | historyViewerFrequencyStats = RemoteData.Failure apiError }, Cmd.none )

        OnGetPastFrequencyStatsSuccess { frequencyStatsList } ->
            let
                updateHistoryViewerFrequencyStats : List FrequencyStats.FrequencyStats -> List FrequencyStats.FrequencyStats
                updateHistoryViewerFrequencyStats historyViewerFrequencyStats =
                    List.foldl
                        (\newStats historyViewerFrequencyStats ->
                            Util.replaceOrAdd historyViewerFrequencyStats (\stats -> stats.habitId == newStats.habitId) newStats
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
                        ?> Dict.empty

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

        ToggleTodayViewerHabitActionsDropdown habitId newState ->
            let
                updatedTodayViewerHabitActionsDropdowns =
                    Dict.update habitId (always <| Just newState) model.todayViewerHabitActionsDropdowns
            in
            ( { model | todayViewerHabitActionsDropdowns = updatedTodayViewerHabitActionsDropdowns }, Cmd.none )

        ToggleHistoryViewerHabitActionsDropdown habitId newState ->
            let
                updatedHistoryViewerHabitActionsDropdowns =
                    Dict.update habitId (always <| Just newState) model.historyViewerHabitActionsDropdowns
            in
            ( { model | historyViewerHabitActionsDropdowns = updatedHistoryViewerHabitActionsDropdowns }, Cmd.none )

        OnToggleDarkMode ->
            ( { model | darkModeOn = not model.darkModeOn }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        OnToggleShowSetHabitDataShortcut ->
            ( { model | showSetHabitDataShortcut = not model.showSetHabitDataShortcut }, Cmd.none )

        KeyboardExtraMsg keyMsg ->
            let
                newKeysDown =
                    KK.update keyMsg model.keysDown

                ( newShowSetHabitDataShortcut, showSetHabitDataShortcutNeedsUpdate ) =
                    case keyMsg of
                        KK.Down keyCode ->
                            if KK.fromCode keyCode == KK.CharA then
                                ( True, model.showSetHabitDataShortcut /= True )

                            else if KK.fromCode keyCode == KK.Escape then
                                ( False, model.showSetHabitDataShortcut /= False )

                            else
                                ( model.showSetHabitDataShortcut, False )

                        _ ->
                            ( model.showSetHabitDataShortcut, False )
            in
            -- If you want to react to key-presses, call a function here instead
            -- of just updating the model (you should still update the model).
            if showSetHabitDataShortcutNeedsUpdate then
                ( { model
                    | keysDown = newKeysDown
                    , showSetHabitDataShortcut = newShowSetHabitDataShortcut
                  }
                , if newShowSetHabitDataShortcut then
                    Dom.focus
                        (if model.showSetHabitDataShortcutAmountForm then
                            "set-habit-data-shortcut-amount-form-input"

                         else
                            "set-habit-data-shortcut-habit-selection-input"
                        )
                        |> Task.attempt FocusResult

                  else
                    Cmd.none
                )

            else
                ( { model | keysDown = newKeysDown }, Cmd.none )

        FocusResult result ->
            case result of
                Result.Err (Dom.NotFound id) ->
                    ( model, Cmd.none )

                Result.Ok () ->
                    ( model, Cmd.none )

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
                            Util.firstIndexInList newFilteredHabits h ?> 0

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
                    (model.setHabitDataShortcutSelectedHabitIndex + 1) % filteredHabitsLength
            in
            ( { model | setHabitDataShortcutSelectedHabitIndex = newSelectedHabitIndex }
            , Cmd.none
            )

        OnSetHabitDataShortcutSelectPreviousHabit ->
            let
                filteredHabitsLength =
                    Array.length model.setHabitDataShortcutFilteredHabits

                newSelectedHabitIndex =
                    (model.setHabitDataShortcutSelectedHabitIndex - 1) % filteredHabitsLength
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

        OnSetHabitDataShortcutAmountFormSubmit ymd habitId newVal ->
            case newVal of
                Nothing ->
                    ( model, Cmd.none )

                Just newVal ->
                    ( { model
                        | showSetHabitDataShortcut = False
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
                | showEditGoalDialog = True
                , editGoalDialogHabit = newEditGoalDialogHabit
              }
            , Cmd.none
            )

        CloseEditGoalDialog ->
            ( { model
                | showEditGoalDialog = False
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
            -- TODO
            ( model, Cmd.none )

        OnEditGoalSuccess habit ->
            { model
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
                , showEditGoalDialog = False
            }
                ! [ getTodayViewerFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
                  , case model.historyViewerSelectedDate of
                        Just ymd ->
                            getHistoryViewerFrequencyStats ymd [ habit |> Habit.getCommonFields |> .id ]

                        Nothing ->
                            Cmd.none
                  ]

        OnEditGoalSubmitClick habitId newFrequencies habitType ->
            ( { model
                | showEditGoalDialog = False
              }
            , Api.mutationEditHabitGoalFrequencies habitId newFrequencies habitType model.apiBaseUrl OnEditGoalFailure OnEditGoalSuccess
            )

        -- Suspending Habits
        OnResumeOrSuspendHabitClick habitId currentlySuspended onTodayViewer oldSuspensions ->
            let
                newDropdownsModel =
                    -- Close the Habit Actions Dropdown that was used to suspend/resume the habit
                    if onTodayViewer then
                        { model
                            | todayViewerHabitActionsDropdowns =
                                Dict.update
                                    habitId
                                    (always <| Just False)
                                    model.todayViewerHabitActionsDropdowns
                        }

                    else
                        { model
                            | historyViewerHabitActionsDropdowns =
                                Dict.update
                                    habitId
                                    (always <| Just False)
                                    model.historyViewerHabitActionsDropdowns
                        }

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
                                        if YmdDate.compareYmds currSuspendedInterval.startDate model.ymd == LT then
                                            -- The suspended interval started earlier than today.
                                            -- To resume today, we set its endDate to yesterday.
                                            let
                                                yesterday =
                                                    YmdDate.addDays -1 model.ymd

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
                                        if YmdDate.compareYmds endDateYmd model.ymd == LT then
                                            -- The SuspendedInterval's last day was yesterday or earlier.
                                            -- To suspend the habit, we should add a new SuspendedInterval that starts today.
                                            let
                                                newSuspendedInterval =
                                                    { startDate = model.ymd, endDate = Nothing }
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
                                [ { startDate = model.ymd, endDate = Nothing } ]
            in
            ( newDropdownsModel
            , Api.mutationEditHabitSuspensions
                habitId
                newSuspensions
                model.apiBaseUrl
                OnResumeOrSuspendHabitFailure
                OnResumeOrSuspendHabitSuccess
            )

        OnResumeOrSuspendHabitFailure apiError ->
            -- TODO
            ( model, Cmd.none )

        OnResumeOrSuspendHabitSuccess habit ->
            { model
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
                ! [ getTodayViewerFrequencyStats [ habit |> Habit.getCommonFields |> .id ]
                  , case model.historyViewerSelectedDate of
                        Just ymd ->
                            getHistoryViewerFrequencyStats ymd [ habit |> Habit.getCommonFields |> .id ]

                        Nothing ->
                            Cmd.none
                  ]

        -- Error Messages
        OnToggleShowErrorMessage ->
            ( { model | showErrorMessage = not model.showErrorMessage }
            , Cmd.none
            )


extractInt : String -> Maybe Int -> Maybe Int
extractInt string default =
    if String.isEmpty string then
        Nothing

    else
        String.toInt string
            |> Result.map Just
            |> Result.withDefault default
