module DateRange exposing
    ( dateRange
    , numberOfUnitsPerYear
    , stringToUnit
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


stringToUnit : String -> Maybe Unit
stringToUnit s =
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
