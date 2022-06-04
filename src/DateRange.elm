module DateRange exposing
    ( dateRange
    , endOfUnit
    , format
    , intToOrdinal
    , numberOfUnitsPerYear
    , timeDifference
    , timeDifferenceAsOrdinal
    , timeDifferenceAsString
    , unitFromString
    , unitToString
    , unitToStringSingular
    )

import Date exposing (Date, Interval(..), Unit(..))


dateRange : Unit -> Int -> Date -> Date -> List Date
dateRange unit count startDate endDate =
    let
        buildRange : Date -> List Date -> List Date
        buildRange currentDate accumulatedDates =
            if Date.compare currentDate endDate == LT then
                buildRange
                    (Date.add unit count currentDate)
                    (currentDate :: accumulatedDates)

            else
                accumulatedDates
    in
    buildRange startDate [] |> List.reverse


numberOfUnitsPerYear : Unit -> Int
numberOfUnitsPerYear unit =
    case unit of
        Years ->
            1

        Months ->
            12

        Weeks ->
            52

        Days ->
            365


unitToString : Unit -> String
unitToString unit =
    case unit of
        Years ->
            "years"

        Months ->
            "months"

        Weeks ->
            "weeks"

        Days ->
            "days"


unitToStringSingular : Unit -> String
unitToStringSingular unit =
    case unit of
        Years ->
            "year"

        Months ->
            "month"

        Weeks ->
            "week"

        Days ->
            "day"


unitFromString : String -> Maybe Unit
unitFromString s =
    case s of
        "years" ->
            Just Years

        "months" ->
            Just Months

        "weeks" ->
            Just Weeks

        "days" ->
            Just Days

        _ ->
            Nothing


timeDifference : Date -> Date -> ( Unit, Int )
timeDifference date1 date2 =
    let
        years =
            Date.diff Years date1 date2

        months =
            Date.diff Months date1 date2

        weeks =
            Date.diff Weeks date1 date2

        days =
            Date.diff Days date1 date2
    in
    if weeks < 1 then
        ( Days, days )

    else if months < 1 then
        ( Weeks, weeks )

    else if years < 1 then
        ( Months, months )

    else
        ( Years, years )


timeDifferenceAsString : Date -> Date -> String
timeDifferenceAsString date1 date2 =
    let
        ( unit, num ) =
            timeDifference date1 date2

        pluralS i =
            if i == 1 then
                ""

            else
                "s"
    in
    String.fromInt num ++ " " ++ unitToStringSingular unit ++ pluralS num


timeDifferenceAsOrdinal : Date -> Date -> String
timeDifferenceAsOrdinal date1 date2 =
    let
        ( unit, num ) =
            timeDifference date1 date2

        ordinal =
            intToOrdinal (num + 1)
    in
    ordinal ++ " " ++ unitToStringSingular unit


intToOrdinal : Int -> String
intToOrdinal i =
    let
        s =
            String.fromInt i
    in
    if String.endsWith "11" s then
        s ++ "th"

    else if String.endsWith "12" s then
        s ++ "th"

    else if String.endsWith "13" s then
        s ++ "th"

    else if String.endsWith "1" s then
        s ++ "st"

    else if String.endsWith "2" s then
        s ++ "nd"

    else if String.endsWith "3" s then
        s ++ "rd"

    else
        s ++ "th"


endOfUnit : Unit -> Date -> Date
endOfUnit unit startOfUnit =
    startOfUnit
        |> Date.add unit 1
        |> Date.add Days -1


format : Date -> Date -> String
format startDate endDate =
    let
        dateFormat : String
        dateFormat =
            "MMMM ddd, y"
    in
    Date.format dateFormat startDate
        ++ " - "
        ++ Date.format dateFormat endDate
