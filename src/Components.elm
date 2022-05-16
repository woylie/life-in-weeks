module Components exposing (grid)

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
        , width
        )
import Date exposing (Date, Interval(..), Unit(..))
import DateRange exposing (dateRange)
import Html.Styled
    exposing
        ( Html
        , div
        )
import Html.Styled.Attributes exposing (css, style)
import Time exposing (Month(..))


gapSize : String
gapSize =
    "2px"


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
