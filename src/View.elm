module View exposing (view)

import Components
import Css
    exposing
        ( backgroundColor
        , block
        , border3
        , display
        , displayFlex
        , flexDirection
        , height
        , hex
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
        , div
        )
import Html.Styled.Attributes exposing (css, style)
import Time exposing (Month(..))
import Types exposing (Model, Msg(..), State(..))


gapSize : String
gapSize =
    "2px"


view : Model -> Html Msg
view model =
    let
        unitsPerYear =
            numberOfUnitsPerYear model.unit

        deathdate =
            Date.add
                model.unit
                (unitsPerYear * model.lifeExpectancy)
                model.birthdate

        years =
            dateRange model.unit unitsPerYear model.birthdate deathdate
    in
    Components.container
        [ div
            [ css [ displayFlex ], style "gap" "2rem" ]
            [ sidepanel model
            , grid model.today model.unit unitsPerYear years
            ]
        ]


sidepanel : Model -> Html Msg
sidepanel model =
    div
        [ css [ width (rem 20) ] ]
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
            ]
        ]


grid : Date -> Unit -> Int -> List Date -> Html msg
grid today unit unitsPerYear years =
    div
        [ css [ displayFlex, flexDirection Css.column ], style "gap" gapSize ]
    <|
        List.indexedMap
            (\_ startOfYear ->
                let
                    oneYearLater =
                        Date.add unit unitsPerYear startOfYear

                    units =
                        dateRange unit 1 startOfYear oneYearLater
                in
                row <|
                    List.indexedMap
                        (\_ startOfUnit ->
                            let
                                endOfUnit =
                                    Date.add unit 1 startOfUnit
                                        |> Date.add Days -1

                                state =
                                    if Date.isBetween startOfUnit endOfUnit today then
                                        Present

                                    else if Date.compare startOfUnit today == LT then
                                        Past

                                    else
                                        Future
                            in
                            column state
                        )
                        units
            )
            years


row : List (Html msg) -> Html msg
row content =
    div
        [ css [ displayFlex ], style "gap" gapSize ]
        content


column : State -> Html msg
column state =
    let
        ( boxColor, borderColor ) =
            case state of
                Past ->
                    ( "BBBBBB", "BBBBBB" )

                Present ->
                    ( "24b373", "24b373" )

                Future ->
                    ( "FFFFFF", "BBBBBB" )
    in
    div
        [ css
            [ width (px 6)
            , height (px 6)
            , backgroundColor (hex boxColor)
            , border3 (px 1) solid (hex borderColor)
            , display block
            ]
        ]
        []
