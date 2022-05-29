module View exposing (view)

import Components
import Css
    exposing
        ( auto
        , backgroundColor
        , block
        , border3
        , borderRadius
        , display
        , displayFlex
        , flex
        , flex3
        , flexBasis
        , flexDirection
        , flexGrow
        , flexShrink
        , fontSize
        , height
        , hex
        , int
        , margin
        , pct
        , px
        , rem
        , solid
        , width
        )
import Date exposing (Date, Interval(..), Unit(..))
import DateRange exposing (dateRange, numberOfUnitsPerYear)
import Html.Styled
    exposing
        ( Html
        , a
        , div
        , p
        , text
        )
import Html.Styled.Attributes exposing (css, href, style, target)
import Time exposing (Month(..))
import Types exposing (Dates, Model, Msg(..), Phase(..), State(..))


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
                    model.birthdate
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
    in
    div
        [ css [ displayFlex, flexDirection Css.column ], style "gap" gapSize ]
    <|
        List.indexedMap
            (\_ startOfYear -> row model dates unitsPerYear startOfYear)
            years


row : Model -> Dates -> Int -> Date -> Html msg
row model dates unitsPerYear startOfYear =
    let
        oneYearLater =
            Date.add model.unit unitsPerYear startOfYear

        units =
            dateRange model.unit 1 startOfYear oneYearLater
    in
    div
        [ css [ displayFlex ], style "gap" gapSize ]
    <|
        List.indexedMap
            (\_ startOfUnit -> column model dates startOfUnit)
            units


column : Model -> Dates -> Date -> Html msg
column model dates startOfUnit =
    let
        endOfUnit =
            startOfUnit
                |> Date.add model.unit 1
                |> Date.add Days -1

        state =
            getState model.today startOfUnit endOfUnit

        phase =
            getPhase dates startOfUnit endOfUnit

        ( boxColor, borderColor ) =
            getColor state phase
    in
    div
        [ css
            [ width (px squareSize)
            , height (px squareSize)
            , backgroundColor (hex boxColor)
            , border3 (px 1) solid (hex borderColor)
            , borderRadius (px 2)
            , display block
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


getPhase : Dates -> Date -> Date -> Phase
getPhase dates startOfUnit endOfUnit =
    if Date.compare startOfUnit dates.retirement /= LT then
        Retirement

    else
        Default


getColor : State -> Phase -> ( String, String )
getColor state phase =
    -- (boxColor, borderColor)
    case ( state, phase ) of
        ( Past, _ ) ->
            ( "54DEFD", "54DEFD" )

        ( Present, _ ) ->
            ( "49C6E5", "49C6E5" )

        ( Future, Default ) ->
            ( "FFFBFA", "8BD7D2" )

        ( Future, Retirement ) ->
            ( "8BD7D2", "8BD7D2" )
