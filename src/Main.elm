module Main exposing (main)

import Browser
import Date exposing (Interval(..), Unit(..))
import DateRange
import Html.Styled exposing (toUnstyled)
import Time exposing (Month(..))
import Types exposing (Model, Msg(..))
import View exposing (view)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }


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

        SetLifeExpectancy s ->
            case String.toInt s of
                Just i ->
                    { model | lifeExpectancy = i }

                Nothing ->
                    model

        SetUnit s ->
            case DateRange.stringToUnit s of
                Just unit ->
                    { model | unit = unit }

                Nothing ->
                    model
