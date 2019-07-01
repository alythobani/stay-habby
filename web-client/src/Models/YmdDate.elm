module Models.YmdDate exposing
    ( YmdDate
    , addDays
    , addMonths
    , addYears
    , compareYmds
    , decodeYmdDate
    , encodeYmdDate
    , fromDate
    , fromSimpleString
    , getFirstMondayAfterDate
    , numDaysInMonth
    , prettyPrint
    , prettyPrintDay
    , prettyPrintMonth
    , prettyPrintWeekday
    , prettyPrintWithWeekday
    , toDate
    , toSimpleString
    , withinYmdDateInterval
    )

import Date
import DefaultServices.Util as Util
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias YmdDate =
    { day : Int, month : Int, year : Int }


compareYmds : YmdDate -> YmdDate -> Order
compareYmds ymdOne ymdTwo =
    let
        yearOrder =
            compare ymdOne.year ymdTwo.year

        monthOrder =
            compare ymdOne.month ymdTwo.month

        dayOrder =
            compare ymdOne.day ymdTwo.day
    in
    if List.member yearOrder [ LT, GT ] then
        yearOrder

    else if List.member monthOrder [ LT, GT ] then
        monthOrder

    else if List.member dayOrder [ LT, GT ] then
        dayOrder

    else
        EQ


{-| Returns True iff `ymd` falls within the inclusive date interval from `startDate` to `endDate`
(or endless interval if `endDate` is Nothing).
-}
withinYmdDateInterval : YmdDate -> Maybe YmdDate -> YmdDate -> Bool
withinYmdDateInterval startDate maybeEndDate ymd =
    let
        compareStartDateOrder =
            compareYmds startDate ymd
    in
    case maybeEndDate of
        Just endDate ->
            let
                compareEndDateOrder =
                    compareYmds endDate ymd
            in
            List.member compareStartDateOrder [ LT, EQ ]
                && List.member compareEndDateOrder [ EQ, GT ]

        Nothing ->
            -- Never ending interval starting at `startDate`
            List.member compareStartDateOrder [ LT, EQ ]


numDaysInMonth : YmdDate -> Int
numDaysInMonth { month, year } =
    let
        isLeapYear =
            if modBy 4 year == 0 then
                if modBy 100 year == 0 && not (modBy 400 year == 0) then
                    -- Century years are not leap years unless they're divisible by 400
                    False

                else
                    True

            else
                False
    in
    if List.member month [ 1, 3, 5, 7, 8, 10, 12 ] then
        31

    else if month == 2 then
        if isLeapYear then
            29

        else
            28

    else
        30


prettyPrintMonth : Int -> String
prettyPrintMonth month =
    case month of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        12 ->
            "December"

        _ ->
            "Invalid Month Number"


prettyPrintDay : Int -> String
prettyPrintDay day =
    String.fromInt day
        ++ (if List.member day [ 1, 21, 31 ] then
                "st"

            else if List.member day [ 2, 22 ] then
                "nd"

            else if List.member day [ 3, 23 ] then
                "rd"

            else
                "th"
           )


prettyPrint : YmdDate -> String
prettyPrint ymd =
    prettyPrintMonth ymd.month ++ " " ++ prettyPrintDay ymd.day ++ ", " ++ String.fromInt ymd.year


prettyPrintWithWeekday : YmdDate -> String
prettyPrintWithWeekday ymd =
    ymd |> toDate |> Date.format "EEEE, MMMM ddd, y"


prettyPrintWeekday : YmdDate -> String
prettyPrintWeekday ymd =
    let
        weekdayNumberToString weekdayNumber =
            case weekdayNumber of
                1 ->
                    "Monday"

                2 ->
                    "Tuesday"

                3 ->
                    "Wednesay"

                4 ->
                    "Thursday"

                5 ->
                    "Friday"

                6 ->
                    "Saturday"

                7 ->
                    "Sunday"

                _ ->
                    "Invalid Weekday Number"
    in
    ymd |> toDate |> Date.weekdayNumber |> weekdayNumberToString


{-| Add days to a date to get a new date that many days away, you can add negative days to go back in time.
-}
addDays : Int -> YmdDate -> YmdDate
addDays dayDelta ymd =
    toDate ymd
        |> Date.add Date.Days dayDelta
        |> fromDate


{-| Add months to a date to get a new date that many months away, you can add negative months to go back in time.
Day values are clamped to the end of the month (e.g. Jan 31 -> Feb 28) if necessary.
-}
addMonths : Int -> YmdDate -> YmdDate
addMonths monthDelta ymd =
    toDate ymd
        |> Date.add Date.Months monthDelta
        |> fromDate


{-| Add years to a date to get a new date that many years away, you can add negative years to go back in time.
Day values are clamped to the end of the month (e.g. Jan 31 -> Feb 28) if necessary.
-}
addYears : Int -> YmdDate -> YmdDate
addYears yearDelta ymd =
    toDate ymd
        |> Date.add Date.Years yearDelta
        |> fromDate


toDate : YmdDate -> Date.Date
toDate ymd =
    Date.fromCalendarDate ymd.year (Date.numberToMonth ymd.month) ymd.day


fromDate : Date.Date -> YmdDate
fromDate date =
    { year = Date.year date, month = Date.monthNumber date, day = Date.day date }


getFirstMondayAfterDate : YmdDate -> YmdDate
getFirstMondayAfterDate ymd =
    let
        -- Days are numbered 1 (Monday) to 7 (Sunday)
        ymdDayOfWeekNumber =
            toDate ymd |> Date.weekdayNumber

        -- Monday: 0, Tuesday: 6, Wednesday: 5, ..., Sunday: 1
        numDaysToAdd =
            modBy 7 (8 - ymdDayOfWeekNumber)
    in
    addDays numDaysToAdd ymd


{-| From format "dd/mm/yy" where it's not required that dd or mm be 2 characters.

@refer toSimpleString

-}
fromSimpleString : String -> Maybe YmdDate
fromSimpleString date =
    String.split "/" date
        |> (\dateComponents ->
                case dateComponents of
                    [ dayStr, monthNumberStr, shortenedYearStr ] ->
                        case ( String.toInt dayStr, String.toInt monthNumberStr, String.toInt <| "20" ++ shortenedYearStr ) of
                            ( Just day, Just monthNumber, Just year ) ->
                                Date.fromCalendarDate year (Date.numberToMonth monthNumber) day
                                    |> fromDate
                                    |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing
           )


{-| To format "dd/mm/yy", where dd and mm can be 1-char, they are not zero-padded.

@refer fromSimpleString

-}
toSimpleString : YmdDate -> String
toSimpleString { year, month, day } =
    String.fromInt day ++ "/" ++ String.fromInt month ++ "/" ++ (String.dropLeft 2 <| String.fromInt year)


{-| Encode a `YmdDate` as JSON that can be inputed into a graphQL query / mutation.
-}
encodeYmdDate : YmdDate -> String
encodeYmdDate ymd =
    let
        templateDict =
            Dict.fromList
                [ ( "day", Util.encodeInt ymd.day )
                , ( "month", Util.encodeInt ymd.month )
                , ( "year", Util.encodeInt ymd.year )
                ]

        graphQLString =
            """{
              day: {{day}},
              month: {{month}},
              year: {{year}}
            }"""
    in
    Util.templater templateDict graphQLString


decodeYmdDate : Decode.Decoder YmdDate
decodeYmdDate =
    Decode.succeed YmdDate
        |> required "day" Decode.int
        |> required "month" Decode.int
        |> required "year" Decode.int
