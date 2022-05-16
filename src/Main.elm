module Main exposing (main)

import Browser
import Date exposing (Interval(..), Unit(..))
import DateRange
import Html.Styled exposing (toUnstyled)
import Task
import Time exposing (Month(..))
import Types exposing (Model, Msg(..))
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { birthdate = Date.fromCalendarDate 1991 Apr 13
      , lifeExpectancy = 72
      , today = Date.fromCalendarDate 2000 Jan 1
      , unit = Weeks
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceiveDate date ->
            ( { model | today = date }, Cmd.none )

        SetLifeExpectancy s ->
            case String.toInt s of
                Just i ->
                    ( { model | lifeExpectancy = i }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetUnit s ->
            case DateRange.stringToUnit s of
                Just unit ->
                    ( { model | unit = unit }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
