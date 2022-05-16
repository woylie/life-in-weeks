module View exposing (view)

import Components
import Css
    exposing
        ( backgroundColor
        , block
        , display
        , displayFlex
        , flexDirection
        , height
        , hex
        , px
        , rem
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
import Types exposing (Model, Msg(..))


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
            [ css [ displayFlex ] ]
            [ sidepanel model
            , grid model.unit unitsPerYear years
            ]
        ]


grid : Unit -> Int -> List Date -> Html msg
grid unit unitsPerYear years =
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
                    List.indexedMap (\_ _ -> column) units
            )
            years


sidepanel : Model -> Html Msg
sidepanel model =
    div
        [ css [ width (rem 20) ] ]
        [ Components.field "liw-field-unit"
            "Time unit"
            [ Components.select "liw-field-unit"
                (DateRange.unitToString model.unit)
                SetUnit
                [ ( "weeks", "weeks" ), ( "months", "months" ) ]
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


row : List (Html msg) -> Html msg
row content =
    div
        [ css [ displayFlex ], style "gap" gapSize ]
        content


column : Html msg
column =
    div
        [ css
            [ width (px 8)
            , height (px 8)
            , backgroundColor (hex "71819c")
            , display block
            ]
        ]
        []
