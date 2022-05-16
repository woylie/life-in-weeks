module Main exposing (Msg(..), main, update, view)

import Browser
import Css
    exposing
        ( backgroundColor
        , block
        , column
        , display
        , displayFlex
        , flexDirection
        , height
        , hex
        , px
        , width
        )
import Date exposing (Date, Interval(..), Unit(..))
import Html.Styled
    exposing
        ( Html
        , div
        , toUnstyled
        )
import Html.Styled.Attributes exposing (css, style)
import Time exposing (Month(..))


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }


type alias Model =
    { birthdate : Date
    , lifeExpectancy : Int
    , unit : Unit
    }


type Msg
    = NoOp


init : Model
init =
    { birthdate = Date.fromCalendarDate 1991 Apr 13
    , lifeExpectancy = 72
    , unit = Weeks
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


numberOfUnitsPerYear : Unit -> Int
numberOfUnitsPerYear unit =
    case unit of
        Years ->
            1

        Months ->
            12

        Weeks ->
            52

        Days ->
            365


view : Model -> Html Msg
view model =
    let
        unitsPerYear =
            numberOfUnitsPerYear model.unit

        deathdate =
            Date.add model.unit (unitsPerYear * model.lifeExpectancy) model.birthdate

        years =
            dateRange model.unit unitsPerYear model.birthdate deathdate
    in
    div
        [ css [ displayFlex, flexDirection column ], style "gap" "2px" ]
    <|
        List.indexedMap
            (\_ startOfYear ->
                let
                    oneYearLater =
                        Date.add model.unit unitsPerYear startOfYear

                    units =
                        dateRange model.unit 1 startOfYear oneYearLater
                in
                yearBox <| List.indexedMap (\_ _ -> weekBox) units
            )
            years


yearBox : List (Html Msg) -> Html Msg
yearBox content =
    div
        [ css [ displayFlex ], style "gap" "2px" ]
        content


weekBox : Html Msg
weekBox =
    div
        [ css
            [ width (px 8)
            , height (px 8)
            , backgroundColor (hex "71819c")
            , display block
            ]
        ]
        []


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
