module View exposing (view)

import Color exposing (Color)
import Color.Blending
import Colors
import Components exposing (defaultFieldOpts)
import Css
    exposing
        ( absolute
        , alignItems
        , alignSelf
        , block
        , border
        , borderRadius
        , borderStyle
        , borderWidth
        , boxSizing
        , calc
        , center
        , color
        , contentBox
        , cursor
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
        , hex
        , int
        , left
        , listStyleType
        , margin
        , margin4
        , minus
        , none
        , padding
        , pct
        , pointer
        , position
        , property
        , px
        , relative
        , rem
        , solid
        , textAlign
        , top
        , width
        , wrap
        )
import Date exposing (Date, Interval(..), Unit(..))
import DateRange exposing (dateRange, numberOfUnitsPerYear)
import Html.Styled
    exposing
        ( Html
        , div
        , fieldset
        , li
        , p
        , text
        , ul
        )
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy3, lazy5)
import List.Extra as List
import Time exposing (Month(..))
import Types
    exposing
        ( Category(..)
        , Dates
        , Event
        , EventField(..)
        , Model
        , Msg(..)
        , Period
        , PeriodColor
        , PeriodField(..)
        , Phase(..)
        , Settings
        , State(..)
        , categoryToString
        )


gapSize : String
gapSize =
    "2px"


squareSize : Float
squareSize =
    6


dotSize : Float
dotSize =
    4


view : Model -> Html Msg
view ({ form, settings } as model) =
    Components.container
        [ div
            []
            [ lazy5 grid
                model.categories
                model.selectedDate
                settings
                model.today
                model.unit
            , lazy3 details
                model.selectedDate
                settings
                model.unit
            , lazy3 settingsForm
                model.categories
                form
                model.unit
            , actionButtons
            ]
        ]


cutOffWorkAtRetirement : Date -> List Period -> List Period
cutOffWorkAtRetirement retirementDate periods =
    let
        maybeSetEndDate : Period -> Period
        maybeSetEndDate period =
            case period.endDate of
                Just _ ->
                    period

                Nothing ->
                    { period | endDate = Just (Date.add Days -1 retirementDate) }
    in
    List.map
        (\period ->
            case period.category of
                Work ->
                    maybeSetEndDate period

                _ ->
                    period
        )
        periods


periodsForGrid : List Category -> List Period -> List PeriodColor
periodsForGrid categories periods =
    periods
        |> List.filter (\p -> List.member p.category categories)
        |> List.map
            (\period ->
                { color = period.color
                , startDate = period.startDate
                , endDate = period.endDate
                }
            )


getYears : Unit -> Int -> Date -> Date -> Dates -> List Date
getYears unit unitsPerYear today birthdate dates =
    let
        maxYear : Date
        maxYear =
            Date.add Years 150 birthdate

        lastYear =
            today
                |> Date.max dates.death
                |> Date.min maxYear
    in
    dateRange
        unit
        unitsPerYear
        birthdate
        lastYear


settingsForm : List Category -> Settings -> Unit -> Html Msg
settingsForm categories form unit =
    let
        categoryToCheckboxOption : Category -> ( String, Bool )
        categoryToCheckboxOption category =
            let
                categoryAsString =
                    categoryToString category
            in
            ( categoryAsString, List.member category categories )
    in
    div
        []
        [ Components.fieldset "Display"
            [ Components.field "liw-field-unit"
                "Time unit"
                [ Components.select "liw-field-unit"
                    (DateRange.unitToString unit)
                    SetUnit
                    [ ( "weeks", "weeks" ), ( "months", "months" ) ]
                ]
            , Components.checkboxes "Show or hide categories"
                ToggleCategory
                (List.map categoryToCheckboxOption Types.categories)
            ]
        , Components.fieldset "Base variables"
            [ Components.field
                "liw-field-birthdate"
                "Birthdate"
                [ Components.dateInput
                    "liw-field-birthdate"
                    (Just form.birthdate)
                    SetBirthdate
                    { defaultFieldOpts | required = True }
                ]
            , Components.field
                "liw-field-life-expectancy"
                "Life expectancy"
                [ Components.numberInput
                    "liw-field-life-expectancy"
                    form.lifeExpectancy
                    SetLifeExpectancy
                    { defaultFieldOpts
                        | min = Just 0
                        , max = Just 150
                        , required = True
                    }
                ]
            , Components.field
                "liw-field-retirement-age"
                "Retirement age"
                [ Components.numberInput
                    "liw-field-retirement-age"
                    form.retirementAge
                    SetRetirementAge
                    { defaultFieldOpts
                        | min = Just 0
                        , max = Just 100
                        , required = True
                    }
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
            [ periodFieldsets Education (filterPeriods Education form.periods) ]
        , Components.fieldset "Work"
            [ periodFieldsets Work (filterPeriods Work form.periods) ]
        , Components.fieldset "Activities"
            [ periodFieldsets Activity (filterPeriods Activity form.periods) ]
        , Components.fieldset "Memberships"
            [ periodFieldsets Membership (filterPeriods Membership form.periods) ]
        , Components.fieldset "Relationships"
            [ periodFieldsets Relationship (filterPeriods Relationship form.periods) ]
        , Components.fieldset "Places of residence"
            [ periodFieldsets Residence (filterPeriods Residence form.periods) ]
        , Components.fieldset "Other periods"
            [ periodFieldsets Other (filterPeriods Other form.periods) ]
        , Components.fieldset "Singular events"
            [ eventFieldsets form.events ]
        ]


actionButtons : Html Msg
actionButtons =
    Components.fieldset "Import/Export"
        [ Components.button "export JSON" Export
        , Components.button "import JSON" JsonRequested
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
            (UpdatePeriod period.id PeriodName)
            { defaultFieldOpts | required = True }
        ]
    , Components.field
        (inputIdPrefix ++ "startDate")
        "Start date"
        [ Components.dateInput
            (inputIdPrefix ++ "startDate")
            (Just period.startDate)
            (UpdatePeriod period.id PeriodStartDate)
            { defaultFieldOpts | required = True }
        ]
    , Components.field
        (inputIdPrefix ++ "endDate")
        "End date"
        [ Components.dateInput
            (inputIdPrefix ++ "endDate")
            period.endDate
            (UpdatePeriod period.id PeriodEndDate)
            { defaultFieldOpts | required = False }
        ]
    , Components.button "remove" (RemovePeriod period.id)
    ]


eventFieldsets : List Event -> Html Msg
eventFieldsets events =
    div [] <|
        List.map eventFieldset events
            ++ [ div
                    [ css [ flexBasis (pct 100), flexShrink (int 0) ] ]
                    [ Components.button "add" AddEvent ]
               ]


eventFieldset : Event -> Html Msg
eventFieldset event =
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
        (eventFields event)


eventFields : Event -> List (Html Msg)
eventFields event =
    let
        inputIdPrefix =
            "liw-field-event-" ++ String.fromInt event.id ++ "-"
    in
    [ Components.field
        (inputIdPrefix ++ "name")
        "Name"
        [ Components.textInput
            (inputIdPrefix ++ "name")
            event.name
            (UpdateEvent event.id EventName)
            { defaultFieldOpts | required = True }
        ]
    , Components.field
        (inputIdPrefix ++ "date")
        "Date"
        [ Components.dateInput
            (inputIdPrefix ++ "date")
            (Just event.date)
            (UpdateEvent event.id EventDate)
            { defaultFieldOpts | required = True }
        ]
    , Components.button "remove" (RemoveEvent event.id)
    ]


grid : List Category -> Maybe Date -> Settings -> Date -> Unit -> Html Msg
grid categories selectedDate settings today unit =
    let
        unitsPerYear : Int
        unitsPerYear =
            numberOfUnitsPerYear unit

        dates : Dates
        dates =
            getDates
                { birthdate = settings.birthdate
                , lifeExpectancy = settings.lifeExpectancy
                , retirementAge = settings.retirementAge
                , unit = unit
                , unitsPerYear = unitsPerYear
                }

        years : List Date
        years =
            getYears
                unit
                unitsPerYear
                today
                settings.birthdate
                dates

        cutOffPeriods : List Period
        cutOffPeriods =
            cutOffWorkAtRetirement dates.retirement settings.periods

        periods : List PeriodColor
        periods =
            periodsForGrid categories cutOffPeriods

        rowPeriods : Date -> Date -> List PeriodColor
        rowPeriods year oneYearLater =
            filterMatchingPeriods year oneYearLater periods

        renderRow : Date -> Html Msg
        renderRow year =
            let
                oneYearLater =
                    Date.add unit unitsPerYear year
            in
            row
                { dates = dates
                , events = filterMatchingEvents year oneYearLater settings.events
                , periods = rowPeriods year oneYearLater
                , selectedDate = selectedDate
                , today = today
                , unit = unit
                , units = dateRange unit 1 year oneYearLater
                }
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "auto 1fr"
            , property "grid-template-rows" "auto 1fr"
            ]
        ]
        [ div
            [ css [ textAlign center, property "grid-column-start" "2" ] ]
            [ text (horizontalAxisTitle unit) ]
        , div
            [ css
                [ property "text-orientation" "mixed"
                , property "writing-mode" "vertical-rl"
                , alignSelf center
                ]
            ]
            [ text "Years →" ]
        , ul
            [ css
                [ displayFlex
                , flexDirection Css.column
                , property "gap" gapSize
                , listStyleType none
                , padding (px 0)
                , margin (px 0)
                ]
            ]
            (List.map renderRow years)
        ]


horizontalAxisTitle : Unit -> String
horizontalAxisTitle unit =
    case unit of
        Days ->
            "Days →"

        Weeks ->
            "Weeks →"

        Months ->
            "Months →"

        Years ->
            "Years →"


row :
    { dates : Dates
    , events : List Event
    , periods : List PeriodColor
    , selectedDate : Maybe Date
    , today : Date
    , unit : Unit
    , units : List Date
    }
    -> Html Msg
row { dates, events, periods, selectedDate, today, unit, units } =
    let
        renderColumn startOfUnit =
            let
                endOfUnit =
                    DateRange.endOfUnit unit startOfUnit
            in
            column
                { endOfUnit = endOfUnit
                , periodColors = getPeriodColors startOfUnit endOfUnit periods
                , phase = getPhase dates startOfUnit
                , showEventDot = hasEvents startOfUnit endOfUnit events
                , startOfUnit = startOfUnit
                , state = getState today selectedDate startOfUnit endOfUnit
                }
    in
    li
        []
        [ ul
            [ css
                [ displayFlex
                , property "gap" gapSize
                , listStyleType none
                , padding (px 0)
                , margin (px 0)
                ]
            ]
            (List.map renderColumn units)
        ]


getPeriodColors : Date -> Date -> List PeriodColor -> List Color
getPeriodColors startDate endDate periods =
    periods
        |> filterMatchingPeriods startDate endDate
        |> List.map .color


column :
    { endOfUnit : Date
    , periodColors : List Color
    , phase : Phase
    , showEventDot : Bool
    , startOfUnit : Date
    , state : State
    }
    -> Html Msg
column { endOfUnit, periodColors, phase, showEventDot, startOfUnit, state } =
    let
        ( boxColor, borderColor ) =
            Colors.getColor state phase

        dotColor : Color
        dotColor =
            Color.Blending.exclusion boxColor Color.white

        periodDiv : Color -> Html msg
        periodDiv color =
            div
                [ css
                    [ property "background-color" (Color.toCssString color)
                    , flex3 (int 1) (int 1) (pct 100)
                    ]
                ]
                []

        eventDot : Html msg
        eventDot =
            div
                [ css
                    [ width (px dotSize)
                    , height (px dotSize)
                    , borderRadius (px 142191)
                    , property "background-color" (Color.toCssString dotColor)
                    , position absolute
                    , left (calc (pct 50) minus (px (dotSize / 2)))
                    , top (calc (pct 50) minus (px (dotSize / 2)))
                    ]
                ]
                []

        periodOverlays : List (Html Msg)
        periodOverlays =
            if state == Selected || state == Present then
                []

            else
                List.map periodDiv periodColors
    in
    li
        [ css
            [ flex3 (int 1) (int 1) (pct 100)
            , height (px squareSize)
            , borderWidth (px 1)
            , borderStyle solid
            , borderRadius (px 2)
            , display block
            , property "background-color" (Color.toCssString boxColor)
            , property "border-color" (Color.toCssString borderColor)
            , cursor pointer
            , displayFlex
            , flexDirection Css.column
            , position relative
            , boxSizing contentBox
            ]
        , onClick <| SelectDate (Just startOfUnit)
        , title <| DateRange.format startOfUnit endOfUnit
        ]
        (Components.showIf showEventDot eventDot
            :: periodOverlays
        )


getDates :
    { birthdate : Date
    , lifeExpectancy : Int
    , retirementAge : Int
    , unit : Unit
    , unitsPerYear : Int
    }
    -> Dates
getDates { birthdate, lifeExpectancy, retirementAge, unit, unitsPerYear } =
    { death = Date.add unit (unitsPerYear * lifeExpectancy) birthdate
    , retirement = Date.add unit (unitsPerYear * retirementAge) birthdate
    }


getState : Date -> Maybe Date -> Date -> Date -> State
getState today selectedDate startOfUnit endOfUnit =
    if Just startOfUnit == selectedDate then
        Selected

    else if Date.isBetween startOfUnit endOfUnit today then
        Present

    else if Date.compare startOfUnit today == LT then
        Past

    else
        Future


getPhase : Dates -> Date -> Phase
getPhase dates startOfUnit =
    if Date.compare startOfUnit dates.death /= LT then
        PastLifeExpectancy

    else if Date.compare startOfUnit dates.retirement /= LT then
        Retirement

    else
        Default


filterMatchingPeriods :
    Date
    -> Date
    -> List { a | startDate : Date, endDate : Maybe Date }
    -> List { a | startDate : Date, endDate : Maybe Date }
filterMatchingPeriods startOfUnit endOfUnit periods =
    let
        periodFilter : { a | startDate : Date, endDate : Maybe Date } -> Bool
        periodFilter p =
            case p.endDate of
                Just endDate ->
                    Date.compare p.startDate endOfUnit
                        /= GT
                        && Date.compare endDate startOfUnit
                        /= LT

                Nothing ->
                    Date.compare p.startDate endOfUnit /= GT
    in
    List.filter periodFilter periods


eventFilter : Date -> Date -> Event -> Bool
eventFilter startDate endDate { date } =
    Date.compare date startDate /= LT && Date.compare date endDate /= GT


hasEvents : Date -> Date -> List Event -> Bool
hasEvents startDate endDate events =
    List.any (eventFilter startDate endDate) events


filterMatchingEvents : Date -> Date -> List Event -> List Event
filterMatchingEvents startDate endDate events =
    List.filter (eventFilter startDate endDate) events


details : Maybe Date -> Settings -> Unit -> Html Msg
details selectedDate settings unit =
    let
        dates : Dates
        dates =
            getDates
                { birthdate = settings.birthdate
                , lifeExpectancy = settings.lifeExpectancy
                , retirementAge = settings.retirementAge
                , unit = unit
                , unitsPerYear = numberOfUnitsPerYear unit
                }

        periods : List Period
        periods =
            cutOffWorkAtRetirement dates.retirement settings.periods
    in
    div
        [ css
            [ padding (rem 0.75)
            , borderRadius (px 4)
            , margin4 (rem 0.75) (rem 0) (rem 0.375) (rem 0)
            , property
                "background-color"
                (Color.toCssString Colors.invertColor)
            , color (hex "FFFFFF")
            , fontSize (rem 0.75)
            ]
        ]
        [ case selectedDate of
            Just date ->
                detailsForDate
                    { birthdate = settings.birthdate
                    , date = date
                    , dates = dates
                    , events = settings.events
                    , periods = periods
                    , unit = unit
                    }

            Nothing ->
                text <|
                    "Select "
                        ++ DateRange.unitToStringSingular unit
                        ++ " to show details"
        ]


detailsForDate :
    { birthdate : Date
    , date : Date
    , dates : Dates
    , events : List Event
    , periods : List Period
    , unit : Unit
    }
    -> Html Msg
detailsForDate { birthdate, date, dates, events, periods, unit } =
    let
        endOfUnit =
            DateRange.endOfUnit unit date

        dateFormat =
            "MMMM ddd, y"

        periodText =
            Date.format dateFormat date
                ++ " - "
                ++ Date.format dateFormat endOfUnit

        selectedPeriod =
            "Selected "
                ++ DateRange.unitToStringSingular unit
                ++ ": "
                ++ periodText

        age =
            "Age: "
                ++ DateRange.timeDifferenceAsString
                    birthdate
                    endOfUnit

        pastRetirement =
            justIf
                (Date.compare endOfUnit dates.retirement == GT)
                (DateRange.timeDifferenceAsOrdinal dates.retirement date
                    ++ " of retirement"
                )

        pastLifeExpectancy =
            justIf (Date.compare date dates.death == GT)
                (DateRange.timeDifferenceAsString dates.death endOfUnit
                    ++ " past life expectancy"
                )

        defaultItems =
            List.filterMap identity
                [ Just <| selectedPeriod
                , Just <| age
                , pastRetirement
                , pastLifeExpectancy
                ]

        joinPeriodNames : List Period -> String
        joinPeriodNames periodList =
            periodList
                |> List.map .name
                |> String.join ", "

        periodItems =
            periods
                |> filterMatchingPeriods date endOfUnit
                |> List.gatherEqualsBy .category
                |> List.map
                    (\( head, tail ) ->
                        categoryToString head.category
                            ++ ": "
                            ++ joinPeriodNames (head :: tail)
                    )

        eventItems =
            events
                |> filterMatchingEvents date endOfUnit
                |> List.map
                    (\event ->
                        Date.format "MMMM ddd" event.date ++ ": " ++ event.name
                    )
    in
    ul
        [ css [ margin (rem 0), padding (rem 0), listStyleType none ] ]
        (List.map
            (\item -> li [] [ text item ])
            (defaultItems ++ periodItems ++ eventItems)
        )


justIf : Bool -> a -> Maybe a
justIf condition value =
    if condition then
        Just value

    else
        Nothing
