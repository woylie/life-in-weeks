module Colors exposing (categoryColor, getColor)

import Color exposing (Color, rgb255)
import Types exposing (Category(..), Phase(..), State(..))


defaultColor : Color
defaultColor =
    rgb255 84 222 253


retirementColor : Color
retirementColor =
    rgb255 139 215 210


todayColor : Color
todayColor =
    rgb255 73 198 229


categoryColor : Category -> Color
categoryColor category =
    case category of
        Education ->
            -- cyber yellow
            rgb255 255 207 0

        Hobby ->
            -- spanish-viridian
            rgb255 12 124 89

        Membership ->
            -- burnt-orange
            rgb255 214 81 8

        Other ->
            -- dark-sky-blue
            rgb255 131 181 209

        Relationship ->
            -- red-purple
            rgb255 220 0 115

        Residence ->
            -- orange-red-crayola
            rgb255 254 95 85

        Work ->
            -- magic-mint
            rgb255 174 246 199


getColor : State -> Phase -> ( Color, Color )
getColor state phase =
    let
        phaseColor =
            case phase of
                Default ->
                    defaultColor

                Retirement ->
                    retirementColor

                Phase period ->
                    categoryColor period.category
    in
    -- (backgroundColor, borderColor)
    case state of
        Past ->
            ( phaseColor, phaseColor )

        Present ->
            ( todayColor, todayColor )

        Future ->
            ( Color.white, phaseColor )
