module View exposing (dropdownIcon, habitActionsDropdownDiv, renderHabitBox, renderHistoryViewerPanel, renderSetHabitDataShortcut, renderTodayPanel, view)

import Array
import Browser
import DefaultServices.Keyboard as Keyboard
import DefaultServices.Util as Util
import Dict
import HabitUtil
import Html exposing (Html, button, div, hr, i, input, span, text, textarea)
import Html.Attributes exposing (class, classList, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Maybe.Extra as Maybe
import Model exposing (Model)
import Models.ApiError as ApiError
import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData


view : Model -> Browser.Document Msg
view model =
    { title = "Be Habby"
    , body =
        [ div
            [ classList [ ( "view", True ), ( "dark-mode", model.darkModeOn ) ]
            , Util.onKeydown
                (\key ->
                    if key == Keyboard.Space then
                        Just OnToggleShowSetHabitDataShortcut

                    else
                        Nothing
                )
            ]
            [ renderTodayPanel
                model.ymd
                model.allHabits
                model.allHabitData
                model.allFrequencyStats
                model.addHabit
                model.editingTodayHabitAmount
                model.openTodayViewer
                model.todayViewerHabitActionsDropdowns
                model.darkModeOn
                model.errorMessage
            , renderHistoryViewerPanel
                model.openHistoryViewer
                model.historyViewerDateInput
                model.historyViewerSelectedDate
                model.allHabits
                model.allHabitData
                model.historyViewerFrequencyStats
                model.editingHistoryHabitAmount
                model.historyViewerHabitActionsDropdowns
                model.ymd
            , renderSetHabitDataShortcut
                model.showSetHabitDataShortcut
                model.setHabitDataShortcutHabitNameFilterText
                model.setHabitDataShortcutFilteredHabits
                model.setHabitDataShortcutSelectedHabitIndex
                model.showSetHabitDataShortcutAmountForm
                model.allHabitData
                model.ymd
                model.setHabitDataShortcutInputtedAmount
            , renderEditGoalDialog
                model.showEditGoalDialog
                model.editGoalDialogHabit
                model.editGoal
                model.ymd
            , renderErrorMessage
                model.errorMessage
                model.showErrorMessage
            ]
        ]
    }


renderTodayPanel :
    Maybe YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Habit.AddHabitInputData
    -> Dict.Dict String Int
    -> Bool
    -> Dict.Dict String Bool
    -> Bool
    -> Maybe String
    -> Html Msg
renderTodayPanel ymd rdHabits rdHabitData rdFrequencyStatsList addHabit editingHabitDataDict openView habitActionsDropdowns darkModeOn errorMessage =
    let
        maybeCreateHabitData =
            Habit.extractCreateHabit addHabit
    in
    div
        [ class "today-panel" ]
        [ div [ class "today-panel-title", onClick OnToggleTodayViewer ] [ text "Today's Progress" ]
        , div
            [ classList
                [ ( "error-message-icon", True )
                , ( "display-none", not <| Maybe.isJust errorMessage )
                ]
            , onClick OnToggleShowErrorMessage
            ]
            [ i [ class "material-icons" ] [] ]
        , div [ class "dark-mode-switch" ]
            [ input
                [ type_ "checkbox" ]
                [ text <|
                    if darkModeOn then
                        "Dark Mode"

                    else
                        "Light Mode"
                ]
            ]
        , dropdownIcon openView NoOp
        , div
            [ class "today-panel-date" ]
            [ text <|
                Maybe.withDefault
                    "Today's date not available"
                    (Maybe.map YmdDate.prettyPrint ymd)
            ]
        , case ( rdHabits, rdHabitData ) of
            ( RemoteData.Success habits, RemoteData.Success habitData ) ->
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
                            ymd
                            habitData
                            editingHabitDataDict
                            OnHabitDataInput
                            SetHabitData
                            habitActionsDropdowns
                            ToggleTodayViewerHabitActionsDropdown
                            True
                            habit
                            ymd

                    -- TODO: Redundant, but should be removed during the UI change of only one panel (no History Viewer)
                in
                div []
                    [ div
                        [ classList
                            [ ( "display-none", not openView )
                            , ( "all-habit-lists", True )
                            ]
                        ]
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
                    , button
                        [ class "add-habit"
                        , onClick <|
                            if addHabit.openView then
                                OnCancelAddHabit

                            else
                                OnOpenAddHabit
                        ]
                        [ text <|
                            if addHabit.openView then
                                "Cancel"

                            else
                                "Add Habit"
                        ]
                    ]

            ( RemoteData.Failure apiError, _ ) ->
                span [ class "retrieving-habits-status" ] [ text "Failure..." ]

            ( _, RemoteData.Failure apiError ) ->
                span [ class "retrieving-habits-status" ] [ text "Failure..." ]

            _ ->
                span [ class "retrieving-habits-status" ] [ text "Loading..." ]
        , hr [ classList [ ( "add-habit-line-breaker", True ), ( "visibility-hidden height-0", not addHabit.openView ) ] ] []
        , div
            [ classList [ ( "add-habit-input-form", True ), ( "display-none", not addHabit.openView ) ] ]
            [ div
                [ class "add-habit-input-form-habit-tag-name" ]
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
                [ class "add-habit-input-form-name-and-description" ]
                [ input
                    [ class "add-habit-input-form-name"
                    , placeholder "Name..."
                    , onInput OnAddHabitNameInput
                    , value addHabit.name
                    ]
                    []
                , textarea
                    [ class "add-habit-input-form-description"
                    , placeholder "Short description..."
                    , onInput OnAddHabitDescriptionInput
                    , value addHabit.description
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "add-habit-input-form-time-of-day", True )
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
                [ class "add-habit-input-form-unit-name" ]
                [ input
                    [ class "habit-unit-name-singular"
                    , placeholder "Unit name singular..."
                    , onInput OnAddHabitUnitNameSingularInput
                    , value addHabit.unitNameSingular
                    ]
                    []
                , input
                    [ class "habit-unit-name-plural"
                    , placeholder "Unit name plural..."
                    , onInput OnAddHabitUnitNamePluralInput
                    , value addHabit.unitNamePlural
                    ]
                    []
                ]
            , div
                [ class "add-habit-input-form-frequency-tag-name" ]
                [ button
                    [ classList [ ( "selected", addHabit.frequencyKind == Habit.TotalWeekFrequencyKind ) ]
                    , onClick <| OnAddHabitSelectFrequencyKind Habit.TotalWeekFrequencyKind
                    ]
                    [ text "X Per Week" ]
                , button
                    [ classList [ ( "selected", addHabit.frequencyKind == Habit.SpecificDayOfWeekFrequencyKind ) ]
                    , onClick <| OnAddHabitSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind
                    ]
                    [ text "Specific Days of Week" ]
                , button
                    [ classList [ ( "selected", addHabit.frequencyKind == Habit.EveryXDayFrequencyKind ) ]
                    , onClick <| OnAddHabitSelectFrequencyKind Habit.EveryXDayFrequencyKind
                    ]
                    [ text "Y Per X Days" ]
                ]
            , div
                [ classList
                    [ ( "add-habit-input-form-x-times-per-week", True )
                    , ( "display-none", addHabit.frequencyKind /= Habit.TotalWeekFrequencyKind )
                    ]
                ]
                [ input
                    [ placeholder "X"
                    , onInput OnAddHabitTimesPerWeekInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.timesPerWeek)
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "add-habit-input-form-specific-days-of-week", True )
                    , ( "display-none", addHabit.frequencyKind /= Habit.SpecificDayOfWeekFrequencyKind )
                    ]
                ]
                [ input
                    [ placeholder "Monday"
                    , onInput OnAddHabitSpecificDayMondayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.mondayTimes)
                    ]
                    []
                , input
                    [ placeholder "Tuesday"
                    , onInput OnAddHabitSpecificDayTuesdayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.tuesdayTimes)
                    ]
                    []
                , input
                    [ placeholder "Wednesday"
                    , onInput OnAddHabitSpecificDayWednesdayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.wednesdayTimes)
                    ]
                    []
                , input
                    [ placeholder "Thursday"
                    , onInput OnAddHabitSpecificDayThursdayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.thursdayTimes)
                    ]
                    []
                , input
                    [ placeholder "Friday"
                    , onInput OnAddHabitSpecificDayFridayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.fridayTimes)
                    ]
                    []
                , input
                    [ placeholder "Saturday"
                    , onInput OnAddHabitSpecificDaySaturdayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.saturdayTimes)
                    ]
                    []
                , input
                    [ placeholder "Sunday"
                    , onInput OnAddHabitSpecificDaySundayInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.sundayTimes)
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "add-habit-input-form-x-times-per-y-days", True )
                    , ( "display-none", addHabit.frequencyKind /= Habit.EveryXDayFrequencyKind )
                    ]
                ]
                [ input
                    [ placeholder "Times"
                    , onInput OnAddHabitTimesInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.times)
                    ]
                    []
                , input
                    [ placeholder "Days"
                    , onInput OnAddHabitDaysInput
                    , value <| Maybe.withDefault "" (Maybe.map String.fromInt addHabit.days)
                    ]
                    []
                ]
            , case maybeCreateHabitData of
                Nothing ->
                    Util.hiddenDiv

                Just createHabitData ->
                    button
                        [ class "add-new-habit"
                        , onClick <| AddHabit createHabitData
                        ]
                        [ text "Create Habit" ]
            ]
        ]


renderHistoryViewerPanel :
    Bool
    -> String
    -> Maybe YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Dict.Dict String (Dict.Dict String Int)
    -> Dict.Dict String Bool
    -> Maybe YmdDate.YmdDate
    -> Html Msg
renderHistoryViewerPanel openView dateInput maybeSelectedDate rdHabits rdHabitData rdFrequencyStatsList editingHabitDataDictDict habitActionsDropdowns maybeTodayYmd =
    case ( rdHabits, rdHabitData ) of
        ( RemoteData.Success habits, RemoteData.Success habitData ) ->
            div
                [ class "history-viewer-panel" ]
                [ div [ class "history-viewer-panel-title", onClick OnToggleHistoryViewer ] [ text "Browse and Edit History" ]
                , dropdownIcon openView NoOp
                , if not openView then
                    Util.hiddenDiv

                  else
                    case maybeSelectedDate of
                        Nothing ->
                            div
                                [ classList [ ( "date-entry", True ), ( "display-none", not openView ) ] ]
                                [ span [ class "select-yesterday", onClick OnHistoryViewerSelectYesterday ] [ text "yesterday" ]
                                , span
                                    [ class "before-yesterday", onClick OnHistoryViewerSelectBeforeYesterday ]
                                    [ text "before yesterday" ]
                                , span [ class "separating-text" ] [ text "or exact date" ]
                                , input
                                    [ placeholder "dd/mm/yy"
                                    , onInput OnHistoryViewerDateInput
                                    , value dateInput
                                    , Util.onKeydown
                                        (\key ->
                                            if key == Keyboard.Enter then
                                                Just OnHistoryViewerSelectDateInput

                                            else
                                                Nothing
                                        )
                                    ]
                                    []
                                ]

                        Just selectedDate ->
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

                                editingHabitDataDict =
                                    Dict.get (YmdDate.toSimpleString selectedDate) editingHabitDataDictDict
                                        |> Maybe.withDefault Dict.empty

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
                                        maybeSelectedDate
                                        habitData
                                        editingHabitDataDict
                                        (OnHistoryViewerHabitDataInput selectedDate)
                                        SetHabitData
                                        habitActionsDropdowns
                                        ToggleHistoryViewerHabitActionsDropdown
                                        False
                                        habit
                                        maybeTodayYmd
                            in
                            div
                                []
                                [ span [ class "selected-date-title" ] [ text <| YmdDate.prettyPrint selectedDate ]
                                , span [ class "change-date", onClick OnHistoryViewerChangeDate ] [ text "change date" ]
                                , div
                                    [ class "all-habit-lists" ]
                                    [ div [ class "habit-list good-habits" ] <| List.map renderHabit sortedGoodHabits
                                    , div [ class "habit-list bad-habits" ] <| List.map renderHabit sortedBadHabits
                                    , div [ class "habit-list suspended-habits" ] <| List.map renderHabit sortedSuspendedHabits
                                    ]
                                ]
                ]

        ( RemoteData.Failure apiError, _ ) ->
            span [ class "retrieving-habits-status" ] [ text "Failure..." ]

        ( _, RemoteData.Failure apiError ) ->
            span [ class "retrieving-habits-status" ] [ text "Failure..." ]

        _ ->
            span [ class "retrieving-habits-status" ] [ text "Loading..." ]


dropdownIcon : Bool -> msg -> Html msg
dropdownIcon openView msg =
    i
        [ class "material-icons"
        , onClick msg
        ]
        [ text <|
            if openView then
                "arrow_drop_down"

            else
                "arrow_drop_up"
        ]


habitActionsDropdownDiv :
    Bool
    -> YmdDate.YmdDate
    -> String
    -> Bool
    -> List Habit.SuspendedInterval
    -> Maybe YmdDate.YmdDate
    -> Html Msg
habitActionsDropdownDiv dropdown ymd habitId onTodayViewer suspensions maybeTodayYmd =
    case maybeTodayYmd of
        Just todayYmd ->
            let
                currentlySuspended : Bool
                currentlySuspended =
                    case List.reverse suspensions of
                        currSuspendedInterval :: rest ->
                            case currSuspendedInterval.endDate of
                                Just endDateYmd ->
                                    -- The latest `SuspendedInterval` has been closed already.
                                    -- Assumption: no `SuspendedInterval`s were started or ended after today.
                                    if YmdDate.compareYmds endDateYmd todayYmd == LT then
                                        -- The end date was yesterday or earlier, so the habit is now active.
                                        False

                                    else
                                        -- The end date is today (or later, but that shouldn't be possible).
                                        -- Dates are inclusive, so the habit is currently suspended.
                                        True

                                Nothing ->
                                    -- Suspended interval is endless, habit is currently suspended
                                    True

                        [] ->
                            -- Habit has never been suspended before, so it's active
                            False
            in
            div [ class "actions-dropdown" ]
                [ div
                    [ class <|
                        if dropdown then
                            "actions-dropdown-toggler-full"

                        else
                            "actions-dropdown-toggler-default"
                    ]
                    [ text "" ]
                , div
                    [ class "action-buttons" ]
                    [ button
                        [ class "action-button"
                        , onClick <| OnResumeOrSuspendHabitClick habitId currentlySuspended onTodayViewer suspensions
                        ]
                        [ text <|
                            if currentlySuspended then
                                "Resume"

                            else
                                "Suspend"
                        ]
                    , button
                        [ class "action-button"
                        , onClick <| OnEditGoalClick habitId
                        ]
                        [ text "Edit Goal" ]
                    ]
                ]

        Nothing ->
            div [] []


{-| Renders a habit box with the habit data loaded for that particular date.

Requires 2 event handlers, 1 for handling when data is input into the habit box and 1 for when the user wants to
update the habit data.

-}
renderHabitBox :
    Maybe FrequencyStats.FrequencyStats
    -> Maybe YmdDate.YmdDate
    -> List HabitData.HabitData
    -> Dict.Dict String Int
    -> (String -> String -> Msg)
    -> (YmdDate.YmdDate -> String -> Maybe Int -> Msg)
    -> Dict.Dict String Bool
    -> (String -> Bool -> Msg)
    -> Bool
    -> Habit.Habit
    -> Maybe YmdDate.YmdDate
    -> Html Msg
renderHabitBox habitStats maybeYmd habitData editingHabitDataDict onHabitDataInput setHabitData habitActionsDropdowns toggleHabitActionsDropdown onTodayViewer habit maybeTodayYmd =
    case maybeYmd of
        Just ymd ->
            let
                habitRecord =
                    Habit.getCommonFields habit

                habitDatum =
                    List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == ymd) habitData
                        |> List.head
                        |> (\maybeHabitDatum ->
                                case maybeHabitDatum of
                                    Nothing ->
                                        0

                                    Just { amount } ->
                                        amount
                           )

                editingHabitData =
                    Dict.get habitRecord.id editingHabitDataDict

                actionsDropdown =
                    Dict.get habitRecord.id habitActionsDropdowns |> Maybe.withDefault False

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
            in
            div
                [ class
                    (if isCurrentFragmentSuccessful then
                        "habit-success"

                     else
                        "habit-failure"
                    )
                ]
                [ div [ class "habit-name" ] [ text habitRecord.name ]
                , habitActionsDropdownDiv actionsDropdown ymd habitRecord.id onTodayViewer habitRecord.suspensions maybeTodayYmd
                , case habitStats of
                    Nothing ->
                        frequencyStatisticDiv "Error retriving performance stats"

                    Just stats ->
                        if not stats.habitHasStarted then
                            div [ class "current-progress" ] [ text "Start this habit!" ]

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
                                , frequencyStatisticDiv
                                    ((String.fromInt <|
                                        round <|
                                            toFloat stats.successfulFragments
                                                * 100
                                                / toFloat stats.totalFragments
                                     )
                                        ++ "%"
                                    )
                                , frequencyStatisticDiv ("Streak: " ++ String.fromInt stats.currentFragmentStreak)
                                , frequencyStatisticDiv ("Best streak: " ++ String.fromInt stats.bestFragmentStreak)
                                , frequencyStatisticDiv ("Total done: " ++ String.fromInt stats.totalDone)
                                ]
                , div
                    [ classList
                        [ ( "habit-amount-complete", True )
                        , ( "editing", Maybe.isJust <| editingHabitData )
                        ]
                    ]
                    [ input
                        [ placeholder <|
                            String.fromInt habitDatum
                                ++ " "
                                ++ (if habitDatum == 1 then
                                        habitRecord.unitNameSingular

                                    else
                                        habitRecord.unitNamePlural
                                   )
                        , onInput <| onHabitDataInput habitRecord.id
                        , Util.onKeydown
                            (\key ->
                                if key == Keyboard.Enter then
                                    Just <| setHabitData ymd habitRecord.id editingHabitData

                                else
                                    Nothing
                            )
                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editingHabitData)
                        ]
                        []
                    , i
                        [ classList [ ( "material-icons", True ) ]
                        , onClick <| setHabitData ymd habitRecord.id editingHabitData
                        ]
                        [ text "check_box" ]
                    ]
                ]

        Nothing ->
            div [] []


renderSetHabitDataShortcut :
    Bool
    -> String
    -> Array.Array Habit.Habit
    -> Int
    -> Bool
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> Maybe YmdDate.YmdDate
    -> Maybe Int
    -> Html Msg
renderSetHabitDataShortcut showSetHabitDataShortcut setHabitDataShortcutHabitNameFilterText filteredHabits selectedHabitIndex showAmountForm rdHabitData maybeYmd inputtedAmount =
    case maybeYmd of
        Just ymd ->
            let
                selectedHabit =
                    Array.get selectedHabitIndex filteredHabits

                readyToEnterHabit =
                    Maybe.isJust selectedHabit

                renderHabitOption habit =
                    div
                        [ classList
                            [ ( "set-habit-data-shortcut-habits-selection-habits-list-habit-name", True )
                            , ( "set-habit-data-shortcut-habits-selection-habits-list-selected-habit"
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
                    [ ( "set-habit-data-shortcut", True )
                    , ( "display-none", not showSetHabitDataShortcut )
                    ]
                ]
                [ div
                    [ classList
                        [ ( "set-habit-data-shortcut-background", True )
                        , ( "display-none", not showSetHabitDataShortcut )
                        ]
                    , onClick OnToggleShowSetHabitDataShortcut
                    ]
                    []
                , div
                    [ classList
                        [ ( "set-habit-data-shortcut-habit-selection", True )
                        , ( "display-none", showAmountForm )
                        ]
                    ]
                    [ input
                        [ id "set-habit-data-shortcut-habit-selection-input"
                        , class "set-habit-data-shortcut-habit-selection-input"
                        , placeholder "Enter a habit's name..."
                        , onInput <| OnSetHabitDataShortcutInput
                        , value setHabitDataShortcutHabitNameFilterText
                        , Util.onKeydownPreventDefault
                            (\key ->
                                if key == Keyboard.ArrowDown then
                                    Just OnSetHabitDataShortcutSelectNextHabit

                                else if key == Keyboard.ArrowUp then
                                    Just OnSetHabitDataShortcutSelectPreviousHabit

                                else if key == Keyboard.Enter && readyToEnterHabit then
                                    Just OnToggleShowSetHabitDataShortcutAmountForm

                                else
                                    Nothing
                            )
                        ]
                        []
                    , div
                        [ classList
                            [ ( "set-habit-data-shortcut-habits-selection-habits-list", True )
                            , ( "display-none", Array.isEmpty filteredHabits )
                            ]
                        ]
                        (Array.map renderHabitOption filteredHabits |> Array.toList)
                    ]
                , div
                    [ classList
                        [ ( "set-habit-data-shortcut-amount-form", True )
                        , ( "display-none", not showAmountForm )
                        ]
                    ]
                    (case selectedHabit of
                        Just habit ->
                            let
                                habitRecord =
                                    Habit.getCommonFields habit

                                habitDatum =
                                    case rdHabitData of
                                        RemoteData.Success habitData ->
                                            List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == ymd) habitData
                                                |> List.head
                                                |> (\maybeHabitDatum ->
                                                        case maybeHabitDatum of
                                                            Nothing ->
                                                                0

                                                            Just { amount } ->
                                                                amount
                                                   )

                                        _ ->
                                            0
                            in
                            [ span
                                [ class "set-habit-data-shortcut-amount-form-selected-habit-name" ]
                                [ text <| .name habitRecord ]
                            , input
                                [ id "set-habit-data-shortcut-amount-form-input"
                                , class "set-habit-data-shortcut-amount-form-input"
                                , placeholder <|
                                    String.fromInt habitDatum
                                        ++ " "
                                        ++ (if habitDatum == 1 then
                                                habitRecord.unitNameSingular

                                            else
                                                habitRecord.unitNamePlural
                                           )
                                , onInput OnSetHabitDataShortcutAmountFormInput
                                , value <| Maybe.withDefault "" (Maybe.map String.fromInt inputtedAmount)
                                , Util.onKeydownPreventDefault
                                    (\key ->
                                        if key == Keyboard.Escape then
                                            Just OnToggleShowSetHabitDataShortcutAmountForm

                                        else if key == Keyboard.Enter then
                                            Just <| OnSetHabitDataShortcutAmountFormSubmit ymd habitRecord.id inputtedAmount

                                        else
                                            Nothing
                                    )
                                ]
                                []
                            ]

                        Nothing ->
                            []
                    )
                ]

        Nothing ->
            div [] []


renderEditGoalDialog : Bool -> Maybe Habit.Habit -> Habit.EditGoalInputData -> Maybe YmdDate.YmdDate -> Html Msg
renderEditGoalDialog showEditGoalDialog habit editGoal maybeTodayYmd =
    case maybeTodayYmd of
        Just todayYmd ->
            div
                [ classList
                    [ ( "edit-goal-dialog", True )
                    , ( "display-none", not showEditGoalDialog )
                    ]
                ]
                (case habit of
                    Just h ->
                        let
                            habitRecord =
                                Habit.getCommonFields h

                            isGoodHabit =
                                case h of
                                    Habit.GoodHabit _ ->
                                        True

                                    _ ->
                                        False

                            currentGoal : Maybe Habit.FrequencyChangeRecord
                            currentGoal =
                                case h of
                                    Habit.GoodHabit gh ->
                                        List.head <| List.reverse gh.targetFrequencies

                                    Habit.BadHabit bh ->
                                        List.head <| List.reverse bh.thresholdFrequencies

                            ( currentGoalTag, currentGoalDesc ) =
                                case currentGoal of
                                    Just fcr ->
                                        ( case fcr.newFrequency of
                                            Habit.EveryXDayFrequency f ->
                                                "Y Per X Days"

                                            Habit.TotalWeekFrequency f ->
                                                "X Per Week"

                                            Habit.SpecificDayOfWeekFrequency f ->
                                                "Specific Days of Week"
                                        , Habit.prettyPrintFrequency fcr.newFrequency habitRecord.unitNameSingular habitRecord.unitNamePlural
                                        )

                                    Nothing ->
                                        ( "N/A", "N/A" )

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

                            oldFrequencies : List Habit.FrequencyChangeRecord
                            oldFrequencies =
                                case h of
                                    Habit.GoodHabit gh ->
                                        gh.targetFrequencies

                                    Habit.BadHabit bh ->
                                        bh.thresholdFrequencies

                            newGoal =
                                Habit.extractNewGoal editGoal

                            isWeeklyNewGoal =
                                editGoal.frequencyKind == Habit.TotalWeekFrequencyKind

                            newStartDate : YmdDate.YmdDate
                            newStartDate =
                                if isWeeklyNewGoal then
                                    YmdDate.getFirstMondayAfterDate todayYmd

                                else
                                    todayYmd

                            newFrequencies =
                                case newGoal of
                                    Just newFrequency ->
                                        case List.reverse oldFrequencies of
                                            currFcr :: rest ->
                                                if List.member (YmdDate.compareYmds currFcr.startDate todayYmd) [ EQ, GT ] then
                                                    -- The current goal started today or later, we should overwrite it.
                                                    -- (At this point there should only possibly be one goal, the current one,
                                                    -- that started today or later.)
                                                    case rest of
                                                        secondLastFcr :: restTwo ->
                                                            Just <|
                                                                List.reverse <|
                                                                    { startDate = newStartDate
                                                                    , endDate = Nothing
                                                                    , newFrequency = newFrequency
                                                                    }
                                                                        :: { secondLastFcr | endDate = Just <| YmdDate.addDays -1 newStartDate }
                                                                        :: restTwo

                                                        [] ->
                                                            -- `currFcr` was the only goal and we are replacing it
                                                            Just [ { startDate = newStartDate, endDate = Nothing, newFrequency = newFrequency } ]

                                                else
                                                    Just <|
                                                        List.reverse <|
                                                            { startDate = newStartDate, endDate = Nothing, newFrequency = newFrequency }
                                                                :: { currFcr | endDate = Just <| YmdDate.addDays -1 newStartDate }
                                                                :: rest

                                            [] ->
                                                -- there are no existing goals (this shouldn't happen though)
                                                Just [ { startDate = newStartDate, endDate = Nothing, newFrequency = newFrequency } ]

                                    Nothing ->
                                        -- User has not fully filled out form, we don't need to compute `newFrequencies` yet
                                        Nothing

                            confirmationMessage : String
                            confirmationMessage =
                                case newGoal of
                                    Just newFrequency ->
                                        case currentGoal of
                                            Just fcr ->
                                                "The previous goal for "
                                                    ++ habitRecord.name
                                                    ++ " was "
                                                    ++ Habit.prettyPrintFrequency fcr.newFrequency habitRecord.unitNameSingular habitRecord.unitNamePlural
                                                    ++ ". The new goal "
                                                    ++ Habit.prettyPrintFrequency newFrequency habitRecord.unitNameSingular habitRecord.unitNamePlural
                                                    ++ " will officially start "
                                                    ++ (if newStartDate == todayYmd then
                                                            "today (" ++ YmdDate.prettyPrintWithWeekday newStartDate ++ ")."

                                                        else
                                                            "on " ++ YmdDate.prettyPrintWithWeekday newStartDate ++ "."
                                                       )

                                            Nothing ->
                                                "The new goal "
                                                    ++ newGoalDesc
                                                    ++ " will officially start on "
                                                    ++ YmdDate.prettyPrintWithWeekday newStartDate
                                                    ++ "."

                                    Nothing ->
                                        ""
                        in
                        [ div
                            [ class "edit-goal-dialog-background"
                            , onClick CloseEditGoalDialog
                            ]
                            []
                        , div
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
                                        [ placeholder "X"
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
                                        [ placeholder "Monday"
                                        , onInput OnEditGoalSpecificDayMondayInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.mondayTimes)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Tuesday"
                                        , onInput OnEditGoalSpecificDayTuesdayInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.tuesdayTimes)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Wednesday"
                                        , onInput OnEditGoalSpecificDayWednesdayInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.wednesdayTimes)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Thursday"
                                        , onInput OnEditGoalSpecificDayThursdayInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.thursdayTimes)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Friday"
                                        , onInput OnEditGoalSpecificDayFridayInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.fridayTimes)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Saturday"
                                        , onInput OnEditGoalSpecificDaySaturdayInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.saturdayTimes)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Sunday"
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
                                        [ placeholder "Times"
                                        , onInput OnEditGoalTimesInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.times)
                                        ]
                                        []
                                    , input
                                        [ placeholder "Days"
                                        , onInput OnEditGoalDaysInput
                                        , value <| Maybe.withDefault "" (Maybe.map String.fromInt editGoal.days)
                                        ]
                                        []
                                    ]
                                ]
                            , div
                                [ classList
                                    [ ( "edit-goal-dialog-form-confirmation-message-line-break", True )
                                    , ( "display-none", not <| Maybe.isJust newGoal )
                                    ]
                                ]
                                []
                            , div
                                [ classList
                                    [ ( "edit-goal-dialog-form-confirmation-message", True )
                                    , ( "display-none", not <| Maybe.isJust newGoal )
                                    ]
                                ]
                                [ text confirmationMessage ]
                            , div
                                [ classList
                                    [ ( "edit-goal-dialog-form-submit-button-line-break", True )
                                    , ( "display-none", not <| Maybe.isJust newGoal )
                                    ]
                                ]
                                []
                            , div
                                [ classList
                                    [ ( "edit-goal-dialog-form-buttons", True )
                                    , ( "display-none", not <| Maybe.isJust newGoal )
                                    ]
                                ]
                                [ button
                                    [ class "edit-goal-dialog-form-buttons-submit"
                                    , onClick <|
                                        case newFrequencies of
                                            Just fcrs ->
                                                OnEditGoalSubmitClick
                                                    habitRecord.id
                                                    fcrs
                                                    (if isGoodHabit then
                                                        "good_habit"

                                                     else
                                                        "bad_habit"
                                                    )

                                            Nothing ->
                                                NoOp
                                    ]
                                    [ text "Submit" ]
                                , button
                                    [ class "edit-goal-dialog-form-buttons-cancel"
                                    , onClick CloseEditGoalDialog
                                    ]
                                    [ text "Cancel" ]
                                ]
                            ]
                        ]

                    Nothing ->
                        []
                )

        Nothing ->
            div [] []


renderErrorMessage : Maybe String -> Bool -> Html Msg
renderErrorMessage errorMessage showErrorMessage =
    div
        [ classList
            [ ( "error-message", True )
            , ( "display-none", not showErrorMessage )
            ]
        ]
        [ div
            [ class "error-message-background"
            , onClick OnToggleShowErrorMessage
            ]
            [ div
                [ class "error-message-text" ]
                [ text <|
                    Maybe.withDefault
                        "No errors"
                        (Maybe.map (\em -> em ++ ". You may want to refresh the page.") errorMessage)
                ]
            ]
        ]
