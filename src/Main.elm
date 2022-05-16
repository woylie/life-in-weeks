module Main exposing (Msg(..), main, update, view)

import Browser
import Components
import Date exposing (Date, Interval(..), Unit(..))
import DateRange exposing (dateRange)
import Html.Styled
    exposing
        ( Html
        , toUnstyled
        )
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
    Components.grid model.unit unitsPerYear years


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
