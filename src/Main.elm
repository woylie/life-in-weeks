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
    ( { birthdate = Date.fromCalendarDate 1990 Jan 1
      , lifeExpectancy = 73
      , retirementAge = 65
      , today = Date.fromCalendarDate 2000 Jan 1
      , unit = Weeks
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveDate date ->
            ( { model | today = date }, Cmd.none )

        SetBirthdate s ->
            ( { model | birthdate = s |> Date.fromIsoString |> Result.withDefault model.birthdate }
            , Cmd.none
            )

        SetLifeExpectancy s ->
            ( { model | lifeExpectancy = toIntWithDefault model.lifeExpectancy s }
            , Cmd.none
            )

        SetRetirementAge s ->
            ( { model | retirementAge = toIntWithDefault model.retirementAge s }
            , Cmd.none
            )

        SetUnit s ->
            ( { model | unit = s |> DateRange.stringToUnit |> Maybe.withDefault model.unit }
            , Cmd.none
            )


toIntWithDefault : Int -> String -> Int
toIntWithDefault default s =
    s
        |> String.toInt
        |> Maybe.withDefault default


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
