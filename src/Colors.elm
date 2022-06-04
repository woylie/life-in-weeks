module Colors exposing (categoryColor, getColor, invertColor, selectedColor)

import Color exposing (Color, rgb255)
import Color.Manipulate exposing (darken, lighten)
import Types exposing (Category(..), Phase(..), State(..))


defaultColor : Color
defaultColor =
    rgb255 84 222 253


retirementColor : Color
retirementColor =
    rgb255 210 198 207


pastLifeExpectancyColor : Color
pastLifeExpectancyColor =
    rgb255 20 149 204


todayColor : Color
todayColor =
    rgb255 73 198 229


selectedColor : Color
selectedColor =
    rgb255 60 73 63


invertColor : Color
invertColor =
    rgb255 141 107 148


categoryColor : Category -> Color
categoryColor category =
    case category of
        Education ->
            rgb255 255 207 0

        Activity ->
            rgb255 206 207 222

        Membership ->
            rgb255 249 181 172

        Other ->
            rgb255 131 181 209

        Relationship ->
            rgb255 250 163 129

        Residence ->
            rgb255 120 220 227

        Work ->
            rgb255 35 231 103


getColor : State -> Phase -> ( Color, Color )
getColor state phase =
    let
        phaseColor =
            case phase of
                Default ->
                    defaultColor

                Retirement ->
                    retirementColor

                PastLifeExpectancy ->
                    pastLifeExpectancyColor

                Phase period ->
                    period.color
    in
    -- (backgroundColor, borderColor)
    case state of
        Past ->
            ( phaseColor, phaseColor )

        Present ->
            ( todayColor, darken 0.15 todayColor )

        Future ->
            ( lighten 0.6 phaseColor, phaseColor )

        Selected ->
            ( selectedColor, selectedColor )
