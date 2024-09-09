module HabitUtil exposing (Comparator, HabitStatsPair, compareByAll, compareHabitsByCompletion, compareHabitsByCurrentGoalProgress, compareHabitsByCurrentGoalRemaining, compareHabitsByDaysLeft, compareHabitsByHabitHasStarted, findFrequencyStatsForHabit, isHabitCurrentFragmentSuccessful, isHabitCurrentlySuspended, sortHabitsByCurrentFragment, splitHabitsByCurrentlySuspended, doesHabitCurrentlyHaveFailedStreak)

{-| Module for useful Habit operations
-}

import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit


isHabitCurrentFragmentSuccessful : Habit.Habit -> FrequencyStats.FrequencyStats -> Bool
isHabitCurrentFragmentSuccessful habit frequencyStats =
    if not frequencyStats.habitHasStarted then
        False

    else
        case habit of
            Habit.GoodHabit _ ->
                frequencyStats.currentFragmentTotal >= frequencyStats.currentFragmentGoal

            Habit.BadHabit _ ->
                frequencyStats.currentFragmentTotal <= frequencyStats.currentFragmentGoal


{- Returns whether the habit currently has a failed streak, i.e. a streak of 0 unless the habit has just started or is suspended. -}
doesHabitCurrentlyHaveFailedStreak : FrequencyStats.FrequencyStats -> Bool
doesHabitCurrentlyHaveFailedStreak frequencyStats =
    if not frequencyStats.habitHasStarted || frequencyStats.totalFragments == 0 || frequencyStats.currentlySuspended  then
        False

    else
        frequencyStats.currentFragmentStreak == 0

findFrequencyStatsForHabit : Habit.Habit -> List FrequencyStats.FrequencyStats -> Maybe FrequencyStats.FrequencyStats
findFrequencyStatsForHabit habit frequencyStats =
    List.filter (\stats -> stats.habitId == (habit |> Habit.getCommonFields |> .id)) frequencyStats
        |> List.head


type alias HabitStatsPair =
    ( Habit.Habit, FrequencyStats.FrequencyStats )


{-| For comparing 2 instances of `t`.
-}
type alias Comparator t =
    t -> t -> Order


{-| To be able to sort by multiple comparators (for breaking ties).
Inspired by <https://github.com/amilner42/code-tidbit/blob/a8a8b3b169eb7a4c929788095ae43353d6559752/frontend/src/DefaultServices/Sort.elm#L24>
-}
compareByAll : List (Comparator t) -> Comparator t
compareByAll comparators tOne tTwo =
    case comparators of
        [] ->
            EQ

        comparator :: restOfComparators ->
            case comparator tOne tTwo of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compareByAll restOfComparators tOne tTwo


{-| Compares two habits by whether or not they have been started by the user.
-}
compareHabitsByHabitHasStarted : Comparator HabitStatsPair
compareHabitsByHabitHasStarted ( _, statsOne ) ( _, statsTwo ) =
    if statsOne.habitHasStarted == statsTwo.habitHasStarted then
        EQ

    else if statsOne.habitHasStarted then
        GT

    else
        LT


{-| Compares two habits by whether or not their current fragment goals have already been achieved. Prioritizes incomplete habits.
-}
compareHabitsByCompletion : Comparator HabitStatsPair
compareHabitsByCompletion ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    let
        isHabitOneGoalCompleted =
            isHabitCurrentFragmentSuccessful habitOne statsOne

        isHabitTwoGoalCompleted =
            isHabitCurrentFragmentSuccessful habitTwo statsTwo
    in
    if isHabitOneGoalCompleted == isHabitTwoGoalCompleted then
        EQ

    else if isHabitOneGoalCompleted then
        -- habit one is already complete, habit two is not
        GT

    else
        -- habit two is already complete, habit one is not
        LT


{-| Compares habits by urgency (days left in the current fragment). Prioritizes urgent habits.
-}
compareHabitsByDaysLeft : Comparator HabitStatsPair
compareHabitsByDaysLeft ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    compare statsOne.currentFragmentDaysLeft statsTwo.currentFragmentDaysLeft


{-| Compares two habits by their `timeOfDay` fields. Prioritizes `Morning` over `Anytime` over
`Evening` habits. If they're not both good habits, just returns `EQ`.
-}
compareHabitsByTimeOfDay : Comparator HabitStatsPair
compareHabitsByTimeOfDay ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    case ( habitOne, habitTwo ) of
        ( Habit.GoodHabit goodHabitOne, Habit.GoodHabit goodHabitTwo ) ->
            if goodHabitOne.timeOfDay == goodHabitTwo.timeOfDay then
                EQ

            else
                -- They are different times of day
                case goodHabitOne.timeOfDay of
                    Habit.Morning ->
                        -- `goodHabitTwo.timeOfDay` must be `Anytime` or `Evening`
                        LT

                    Habit.Anytime ->
                        if goodHabitTwo.timeOfDay == Habit.Morning then
                            GT

                        else
                            -- `goodHabitTwo.timeOfDay` must be `Evening`
                            LT

                    Habit.Evening ->
                        -- `goodHabitTwo.timeOfDay` must be `Morning` or `Anytime`
                        GT

        _ ->
            EQ


{-| Compares habits by progress proportional to the current fragment goal. Prioritizes further-behind habits.
-}
compareHabitsByCurrentGoalProgress : Comparator HabitStatsPair
compareHabitsByCurrentGoalProgress ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    let
        -- Progress proportional to goal, e.g. 0.3 if the user has done 30% of the goal
        getCurrentGoalProgressFraction : FrequencyStats.FrequencyStats -> Float
        getCurrentGoalProgressFraction stats =
            toFloat stats.currentFragmentTotal
                / toFloat stats.currentFragmentGoal

        ( habitOneGoalProgressFraction, habitTwoGoalProgressFraction ) =
            ( getCurrentGoalProgressFraction statsOne, getCurrentGoalProgressFraction statsTwo )
    in
    case ( habitOne, habitTwo ) of
        ( Habit.GoodHabit _, Habit.GoodHabit _ ) ->
            -- The higher the fraction, the better the user is doing
            compare habitOneGoalProgressFraction habitTwoGoalProgressFraction

        ( Habit.BadHabit _, Habit.BadHabit _ ) ->
            -- The higher the fraction, the worse the user is doing
            compare habitTwoGoalProgressFraction habitOneGoalProgressFraction

        ( Habit.GoodHabit _, _ ) ->
            -- We probably shouldn't be sorting good habits and bad habits together,
            -- but if we are, we should display good habits first.
            LT

        _ ->
            GT


{-| Compares habits by how much of the current goal remains to be done. Prioritizes further-behind habits.
-}
compareHabitsByCurrentGoalRemaining : Comparator HabitStatsPair
compareHabitsByCurrentGoalRemaining ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    let
        getCurrentGoalRemaining stats =
            stats.currentFragmentGoal - stats.currentFragmentTotal

        ( habitOneGoalRemaining, habitTwoGoalRemaining ) =
            ( getCurrentGoalRemaining statsOne, getCurrentGoalRemaining statsTwo )
    in
    case ( habitOne, habitTwo ) of
        ( Habit.GoodHabit _, Habit.GoodHabit _ ) ->
            -- The more there is left to do for habit one, the worse it is doing
            compare habitTwoGoalRemaining habitOneGoalRemaining

        ( Habit.BadHabit _, Habit.BadHabit _ ) ->
            -- The bigger the difference between the goal and the total, the better
            -- the user is doing, the further down the list we should display the habit
            compare habitOneGoalRemaining habitTwoGoalRemaining

        ( Habit.GoodHabit _, _ ) ->
            -- We probably shouldn't be sorting good habits and bad habits together,
            -- but if we are, we should display good habits first.
            LT

        _ ->
            GT


{-| Returns the habits sorted by their progress in the current fragment.
Sorts first by completion, then by urgency, then by if it's a morning/anytime/evening habit,
then by progress proportional to the current goal, and finally by how much (absolute, not
proportional) of the current goal remains to be done.
-}
sortHabitsByCurrentFragment : List FrequencyStats.FrequencyStats -> List Habit.Habit -> List Habit.Habit
sortHabitsByCurrentFragment frequencyStatsList habits =
    let
        compareHabitsByCurrentFragment : Comparator Habit.Habit
        compareHabitsByCurrentFragment habitOne habitTwo =
            let
                habitOneFrequencyStats =
                    findFrequencyStatsForHabit habitOne frequencyStatsList

                habitTwoFrequencyStats =
                    findFrequencyStatsForHabit habitTwo frequencyStatsList
            in
            case ( habitOneFrequencyStats, habitTwoFrequencyStats ) of
                ( Nothing, Nothing ) ->
                    EQ

                ( Nothing, Just _ ) ->
                    GT

                ( Just _, Nothing ) ->
                    LT

                ( Just statsOne, Just statsTwo ) ->
                    compareByAll
                        [ compareHabitsByHabitHasStarted
                        , compareHabitsByCompletion
                        , compareHabitsByDaysLeft
                        , compareHabitsByTimeOfDay
                        , compareHabitsByCurrentGoalProgress
                        , compareHabitsByCurrentGoalRemaining
                        ]
                        ( habitOne, statsOne )
                        ( habitTwo, statsTwo )
    in
    List.sortWith compareHabitsByCurrentFragment habits


{-| Returns True iff the habit is currently suspended. Returns False if we can't find the habit's
frequency stats for some reason.
-}
isHabitCurrentlySuspended : List FrequencyStats.FrequencyStats -> Habit.Habit -> Bool
isHabitCurrentlySuspended frequencyStatsList habit =
    let
        habitFrequencyStats =
            findFrequencyStatsForHabit habit frequencyStatsList
    in
    case habitFrequencyStats of
        Nothing ->
            False

        Just stats ->
            stats.currentlySuspended


{-| Returns the habits split by good/bad/suspended: (good habits, bad habits, suspended habits).
-}
splitHabitsByCurrentlySuspended :
    List FrequencyStats.FrequencyStats
    -> List Habit.Habit
    -> List Habit.Habit
    -> ( List Habit.Habit, List Habit.Habit, List Habit.Habit )
splitHabitsByCurrentlySuspended frequencyStatsList goodHabits badHabits =
    let
        ( goodSuspendedHabits, goodActiveHabits ) =
            List.partition (isHabitCurrentlySuspended frequencyStatsList) goodHabits

        ( badSuspendedHabits, badActiveHabits ) =
            List.partition (isHabitCurrentlySuspended frequencyStatsList) badHabits
    in
    ( goodActiveHabits, badActiveHabits, List.append goodSuspendedHabits badSuspendedHabits )
