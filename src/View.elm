module View exposing (view)

import Color
import Color.Blending
import Colors
import Components exposing (defaultFieldOpts)
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
        , color
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
        , justifyContent
        , listStyleType
        , margin
        , margin4
        , none
        , padding
        , pct
        , pointer
        , property
        , px
        , rem
        , solid
        , textAlign
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
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
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
        , PeriodField(..)
        , Phase(..)
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
view model =
    let
        unitsPerYear =
            numberOfUnitsPerYear model.unit

        dates =
            getDates model unitsPerYear
    in
    Components.container
        [ div
            []
            [ grid model dates unitsPerYear
            , details model dates
            , settings model
            , actionButtons
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
                    { defaultFieldOpts | required = True }
                ]
            , Components.field
                "liw-field-life-expectancy"
                "Life expectancy"
                [ Components.numberInput
                    "liw-field-life-expectancy"
                    model.lifeExpectancy
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
                    model.retirementAge
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
            [ periodFieldsets Education (filterPeriods Education model.periods) ]
        , Components.fieldset "Work"
            [ periodFieldsets Work (filterPeriods Work model.periods) ]
        , Components.fieldset "Activities"
            [ periodFieldsets Activity (filterPeriods Activity model.periods) ]
        , Components.fieldset "Memberships"
            [ periodFieldsets Membership (filterPeriods Membership model.periods) ]
        , Components.fieldset "Relationships"
            [ periodFieldsets Relationship (filterPeriods Relationship model.periods) ]
        , Components.fieldset "Places of residence"
            [ periodFieldsets Residence (filterPeriods Residence model.periods) ]
        , Components.fieldset "Other periods"
            [ periodFieldsets Other (filterPeriods Other model.periods) ]
        , Components.fieldset "Singular events"
            [ eventFieldsets model.events ]
        ]


actionButtons : Html Msg
actionButtons =
    Components.fieldset "Import/Export"
        [ Components.button "export JSON" Export
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


grid : Model -> Dates -> Int -> Html Msg
grid model dates unitsPerYear =
    let
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


row : Model -> Dates -> List Period -> Int -> Date -> Html Msg
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


column : Model -> Dates -> List Period -> Date -> Html Msg
column model dates periods startOfUnit =
    let
        endOfUnit =
            DateRange.endOfUnit model.unit startOfUnit

        state =
            getState model.today model.selectedDate startOfUnit endOfUnit

        phase =
            getPhase dates periods startOfUnit endOfUnit

        events =
            filterMatchingEvents startOfUnit endOfUnit model.events

        ( boxColor, borderColor ) =
            Colors.getColor state phase

        dotColor =
            Color.Blending.exclusion boxColor Color.white
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
            , cursor pointer
            , displayFlex
            , alignItems center
            , justifyContent center
            ]
        , onClick (SelectDate (Just startOfUnit))
        ]
        [ Components.showIf (events /= [])
            (div
                [ css
                    [ width (px dotSize)
                    , height (px dotSize)
                    , borderRadius (px 142191)
                    , property "background-color" (Color.toCssString dotColor)
                    ]
                ]
                []
            )
        ]


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

        phaseWithDefault default =
            if pastLifeExpectancy then
                PastLifeExpectancy

            else if retirement then
                Retirement

            else
                default
    in
    case matchingPeriod of
        Just period ->
            case period.category of
                Work ->
                    phaseWithDefault (Phase period)

                _ ->
                    Phase period

        Nothing ->
            phaseWithDefault Default


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


filterMatchingEvents : Date -> Date -> List Event -> List Event
filterMatchingEvents startOfUnit endOfUnit events =
    let
        filterCondition e =
            Date.compare e.date startOfUnit
                /= LT
                && Date.compare e.date endOfUnit
                /= GT
    in
    List.filter filterCondition events


details : Model -> Dates -> Html Msg
details model dates =
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
        [ case model.selectedDate of
            Just date ->
                detailsForDate model dates date

            Nothing ->
                text <|
                    "Select "
                        ++ DateRange.unitToStringSingular model.unit
                        ++ " to show details"
        ]


detailsForDate : Model -> Dates -> Date -> Html Msg
detailsForDate model dates date =
    let
        endOfUnit =
            DateRange.endOfUnit model.unit date

        dateFormat =
            "MMMM ddd, y"

        periodText =
            Date.format dateFormat date
                ++ " - "
                ++ Date.format dateFormat endOfUnit

        selectedPeriod =
            "Selected period: " ++ periodText

        age =
            "Age: "
                ++ DateRange.timeDifferenceAsString
                    model.birthdate
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
        joinPeriodNames periods =
            periods
                |> List.map .name
                |> String.join ", "

        periodItems =
            model.periods
                |> filterMatchingPeriods date endOfUnit
                |> List.gatherEqualsBy .category
                |> List.map
                    (\( head, tail ) ->
                        categoryToString head.category
                            ++ ": "
                            ++ joinPeriodNames (head :: tail)
                    )

        eventItems =
            model.events
                |> filterMatchingEvents date endOfUnit
                |> List.sortWith (\e1 e2 -> Date.compare e1.date e2.date)
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
