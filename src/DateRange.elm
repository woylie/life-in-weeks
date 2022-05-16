module DateRange exposing (dateRange)

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
