module Main exposing (main)

import Browser
import Date exposing (Interval(..), Unit(..))
import DateRange
import Html.Styled exposing (toUnstyled)
import Task
import Time exposing (Month(..))
import Types exposing (Category(..), Model, Msg(..), Period, PeriodField(..))
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
      , periods =
            [ { id = 0
              , name = "University of Oxford"
              , startDate = Date.fromCalendarDate 2008 Sep 1
              , endDate = Just <| Date.fromCalendarDate 2014 Jun 30
              , category = Education
              }
            ]
      , retirementAge = 65
      , today = Date.fromCalendarDate 2000 Jan 1
      , unit = Weeks
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPeriod category ->
            ( { model | periods = addPeriod category model.periods }, Cmd.none )

        ReceiveDate date ->
            ( { model | today = date }, Cmd.none )

        RemovePeriod id ->
            ( { model
                | periods =
                    List.filter (\period -> period.id /= id) model.periods
              }
            , Cmd.none
            )

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

        UpdatePeriod id field value ->
            ( { model | periods = updatePeriods id field value model.periods }
            , Cmd.none
            )


addPeriod : Category -> List Period -> List Period
addPeriod category periods =
    let
        maxId =
            periods
                |> List.map .id
                |> List.maximum
                |> Maybe.withDefault -1
    in
    { id = maxId + 1
    , name = defaultPeriodName category
    , startDate = Date.fromCalendarDate 2000 Jan 1
    , endDate = Just <| Date.fromCalendarDate 2005 Jan 1
    , category = category
    }
        :: List.reverse periods
        |> List.reverse


defaultPeriodName : Category -> String
defaultPeriodName category =
    case category of
        Education ->
            "University of Oxford"

        Hobby ->
            "Painting"

        Membership ->
            "Amnesty International"

        Relationship ->
            "Mary"

        Residence ->
            "London"

        Other ->
            "World trip"

        Work ->
            "Acme Corporation"


updatePeriods : Int -> PeriodField -> String -> List Period -> List Period
updatePeriods id field value periods =
    List.map
        (\period ->
            if period.id == id then
                updatePeriod field value period

            else
                period
        )
        periods


updatePeriod : PeriodField -> String -> Period -> Period
updatePeriod field value period =
    case field of
        Name ->
            { period | name = value }

        StartDate ->
            { period
                | startDate =
                    value
                        |> Date.fromIsoString
                        |> Result.withDefault period.startDate
            }

        EndDate ->
            { period
                | endDate =
                    value
                        |> Date.fromIsoString
                        |> Result.toMaybe
            }


toIntWithDefault : Int -> String -> Int
toIntWithDefault default s =
    s
        |> String.toInt
        |> Maybe.withDefault default


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
