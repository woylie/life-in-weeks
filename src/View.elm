module View exposing (view)

import Color
import Colors
import Components
import Css
    exposing
        ( alignItems
        , alignSelf
        , block
        , border
        , borderRadius
        , borderStyle
        , borderWidth
        , center
        , display
        , displayFlex
        , flex3
        , flexBasis
        , flexDirection
        , flexGrow
        , flexShrink
        , flexStart
        , flexWrap
        , fontSize
        , height
        , int
        , margin
        , padding
        , pct
        , property
        , px
        , rem
        , solid
        , textAlign
        , wrap
        )
import Date exposing (Date, Interval(..), Unit(..))
import DateRange exposing (dateRange, numberOfUnitsPerYear)
import Html.Styled
    exposing
        ( Html
        , div
        , fieldset
        , p
        , text
        )
import Html.Styled.Attributes exposing (css)
import Time exposing (Month(..))
import Types
    exposing
        ( Category(..)
        , Dates
        , Model
        , Msg(..)
        , Period
        , PeriodField(..)
        , Phase(..)
        , State(..)
        )


gapSize : String
gapSize =
    "2px"


squareSize : Float
squareSize =
    6


view : Model -> Html Msg
view model =
    Components.container
        [ div
            []
            [ grid model
            , settings model
            ]
        ]


settings : Model -> Html Msg
settings model =
    div
        []
        [ Components.fieldset "Display"
            [ Components.field "liw-field-unit"
                "Time unit"
                [ Components.select "liw-field-unit"
                    (DateRange.unitToString model.unit)
                    SetUnit
                    [ ( "weeks", "weeks" ), ( "months", "months" ) ]
                ]
            ]
        , Components.fieldset "Base variables"
            [ Components.field
                "liw-field-birthdate"
                "Birthdate"
                [ Components.dateInput
                    "liw-field-birthdate"
                    (Just model.birthdate)
                    SetBirthdate
                ]
            , Components.field
                "liw-field-life-expectancy"
                "Life expectancy"
                [ Components.numberInput
                    "liw-field-life-expectancy"
                    model.lifeExpectancy
                    SetLifeExpectancy
                    (Just 0)
                    (Just 150)
                ]
            , Components.field
                "liw-field-retirement-age"
                "Retirement age"
                [ Components.numberInput
                    "liw-field-retirement-age"
                    model.retirementAge
                    SetRetirementAge
                    (Just 0)
                    (Just 100)
                ]
            , p
                [ css
                    [ fontSize (rem 0.75)
                    , margin (px 0)
                    , flexGrow (int 1)
                    , flexShrink (int 1)
                    , flexBasis (pct 100)
                    ]
                ]
                [ text "You can find the life expectancy for your country and gender on "
                , Components.link
                    "https://www.worldometers.info/demographics/life-expectancy/"
                    "worldometer.com"
                , text "."
                ]
            ]
        , Components.fieldset "Education"
            [ periodFieldsets Education (filterPeriods Education model.periods) ]
        , Components.fieldset "Work"
            [ periodFieldsets Work (filterPeriods Work model.periods) ]
        , Components.fieldset "Hobbies"
            [ periodFieldsets Hobby (filterPeriods Hobby model.periods) ]
        , Components.fieldset "Memberships"
            [ periodFieldsets Membership (filterPeriods Membership model.periods) ]
        , Components.fieldset "Relationships"
            [ periodFieldsets Relationship (filterPeriods Relationship model.periods) ]
        , Components.fieldset "Places of residence"
            [ periodFieldsets Residence (filterPeriods Residence model.periods) ]
        , Components.fieldset "Other periods"
            [ periodFieldsets Other (filterPeriods Other model.periods) ]
        ]


filterPeriods : Category -> List Period -> List Period
filterPeriods category periods =
    List.filter (\p -> p.category == category) periods


periodFieldsets : Category -> List Period -> Html Msg
periodFieldsets category periods =
    div [] <|
        List.map periodFieldset periods
            ++ [ div
                    [ css [ flexBasis (pct 100), flexShrink (int 0) ] ]
                    [ Components.button "add" (AddPeriod category) ]
               ]


periodFieldset : Period -> Html Msg
periodFieldset period =
    fieldset
        [ css
            [ border (px 0)
            , margin (px 0)
            , padding (px 0)
            , displayFlex
            , alignItems flexStart
            , flexWrap wrap
            , flexBasis (pct 100)
            , flexShrink (int 0)
            , padding (rem 0)
            , property "gap" "0.375rem 0.75rem"
            ]
        ]
        (periodFields period)


periodFields : Period -> List (Html Msg)
periodFields period =
    let
        inputIdPrefix =
            "liw-field-period-" ++ String.fromInt period.id ++ "-"
    in
    [ Components.field
        (inputIdPrefix ++ "name")
        "Description"
        [ Components.textInput
            (inputIdPrefix ++ "name")
            period.name
            (UpdatePeriod period.id Name)
        ]
    , Components.field
        (inputIdPrefix ++ "startDate")
        "Start date"
        [ Components.dateInput
            (inputIdPrefix ++ "startDate")
            (Just period.startDate)
            (UpdatePeriod period.id StartDate)
        ]
    , Components.field
        (inputIdPrefix ++ "endDate")
        "End date"
        [ Components.dateInput
            (inputIdPrefix ++ "endDate")
            period.endDate
            (UpdatePeriod period.id EndDate)
        ]
    , Components.button "remove" (RemovePeriod period.id)
    ]


grid : Model -> Html msg
grid model =
    let
        unitsPerYear =
            numberOfUnitsPerYear model.unit

        dates =
            getDates model unitsPerYear

        years =
            dateRange
                model.unit
                unitsPerYear
                model.birthdate
                (Date.max dates.death model.today)

        periods =
            List.sortWith
                (\p1 p2 -> Date.compare p1.startDate p2.startDate)
                model.periods
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "auto 1fr"
            , property "grid-template-rows" "auto 1fr"
            ]
        ]
        [ div [] []
        , div [ css [ textAlign center ] ] [ text (horizontalAxis model.unit) ]
        , div
            [ css
                [ property "text-orientation" "mixed"
                , property
                    "writing-mode"
                    "vertical-rl"
                , alignSelf center
                ]
            ]
            [ text "Years →" ]
        , div
            [ css
                [ displayFlex
                , flexDirection Css.column
                , property "gap" gapSize
                ]
            ]
            (List.indexedMap
                (\_ startOfYear ->
                    row model dates periods unitsPerYear startOfYear
                )
                years
            )
        ]


horizontalAxis : Unit -> String
horizontalAxis unit =
    case unit of
        Days ->
            "Days →"

        Weeks ->
            "Weeks →"

        Months ->
            "Months →"

        Years ->
            "Years →"


row : Model -> Dates -> List Period -> Int -> Date -> Html msg
row model dates periods unitsPerYear startOfYear =
    let
        oneYearLater =
            Date.add model.unit unitsPerYear startOfYear

        units =
            dateRange model.unit 1 startOfYear oneYearLater
    in
    div
        [ css [ displayFlex, property "gap" gapSize ] ]
    <|
        List.indexedMap
            (\_ startOfUnit -> column model dates periods startOfUnit)
            units


column : Model -> Dates -> List Period -> Date -> Html msg
column model dates periods startOfUnit =
    let
        endOfUnit =
            startOfUnit
                |> Date.add model.unit 1
                |> Date.add Days -1

        state =
            getState model.today startOfUnit endOfUnit

        phase =
            getPhase dates periods startOfUnit endOfUnit

        ( boxColor, borderColor ) =
            Colors.getColor state phase
    in
    div
        [ css
            [ flex3 (int 1) (int 1) (pct 100)
            , height (px squareSize)
            , borderWidth (px 1)
            , borderStyle solid
            , borderRadius (px 2)
            , display block
            , property "background-color" (Color.toCssString boxColor)
            , property "border-color" (Color.toCssString borderColor)
            ]
        ]
        []


getDates : Model -> Int -> Dates
getDates model unitsPerYear =
    { death =
        Date.add
            model.unit
            (unitsPerYear * model.lifeExpectancy)
            model.birthdate
    , retirement =
        Date.add
            model.unit
            (unitsPerYear * model.retirementAge)
            model.birthdate
    }


getState : Date -> Date -> Date -> State
getState today startOfUnit endOfUnit =
    if Date.isBetween startOfUnit endOfUnit today then
        Present

    else if Date.compare startOfUnit today == LT then
        Past

    else
        Future


getPhase : Dates -> List Period -> Date -> Date -> Phase
getPhase dates periods startOfUnit endOfUnit =
    let
        matchingPeriod =
            periods
                |> filterMatchingPeriods startOfUnit endOfUnit
                |> List.head

        retirement =
            Date.compare startOfUnit dates.retirement /= LT

        pastLifeExpectancy =
            Date.compare startOfUnit dates.death /= LT
    in
    case matchingPeriod of
        Just period ->
            case period.category of
                Work ->
                    if pastLifeExpectancy then
                        PastLifeExpectancy

                    else if retirement then
                        Retirement

                    else
                        Phase period

                _ ->
                    Phase period

        Nothing ->
            if pastLifeExpectancy then
                PastLifeExpectancy

            else if retirement then
                Retirement

            else
                Default


filterMatchingPeriods : Date -> Date -> List Period -> List Period
filterMatchingPeriods startOfUnit endOfUnit periods =
    let
        filterCondition p =
            case p.endDate of
                Just endDate ->
                    Date.compare p.startDate endOfUnit
                        /= GT
                        && Date.compare endDate startOfUnit
                        /= LT

                Nothing ->
                    Date.compare p.startDate endOfUnit /= GT
    in
    List.filter filterCondition periods
