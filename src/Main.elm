module Main exposing (main)

import Browser
import Color.Manipulate exposing (lighten)
import Colors
import Date exposing (Interval(..), Unit(..))
import DateRange
import Html.Styled exposing (toUnstyled)
import Task
import Time exposing (Month(..))
import Types
    exposing
        ( Category(..)
        , Event
        , EventField(..)
        , Model
        , Msg(..)
        , Period
        , PeriodField(..)
        )
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
      , events =
            [ { id = 0
              , name = "Graduation"
              , date = Date.fromCalendarDate 2014 Jun 30
              }
            ]
      , lifeExpectancy = 73
      , periods =
            [ { id = 0
              , name = "University of Oxford"
              , startDate = Date.fromCalendarDate 2008 Sep 1
              , endDate = Just <| Date.fromCalendarDate 2014 Jun 30
              , category = Education
              , color = Colors.categoryColor Education
              }
            ]
      , retirementAge = 65
      , selectedDate = Nothing
      , today = Date.fromCalendarDate 2000 Jan 1
      , unit = Weeks
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEvent ->
            ( { model | events = addEvent model.events }, Cmd.none )

        AddPeriod category ->
            ( { model | periods = addPeriod category model.periods }, Cmd.none )

        ReceiveDate date ->
            ( { model | today = date }, Cmd.none )

        RemoveEvent id ->
            ( { model
                | events =
                    List.filter (\event -> event.id /= id) model.events
              }
            , Cmd.none
            )

        RemovePeriod id ->
            ( { model
                | periods =
                    List.filter (\period -> period.id /= id) model.periods
              }
            , Cmd.none
            )

        SelectDate date ->
            ( { model | selectedDate = date }, Cmd.none )

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
            ( { model
                | selectedDate = Nothing
                , unit = s |> DateRange.stringToUnit |> Maybe.withDefault model.unit
              }
            , Cmd.none
            )

        UpdateEvent id field value ->
            ( { model | events = updateEvents id field value model.events }
            , Cmd.none
            )

        UpdatePeriod id field value ->
            ( { model | periods = updatePeriods id field value model.periods }
            , Cmd.none
            )


addEvent : List Event -> List Event
addEvent events =
    let
        maxId =
            events
                |> List.map .id
                |> List.maximum
                |> Maybe.withDefault -1
    in
    { id = maxId + 1
    , name = "Wedding"
    , date = Date.fromCalendarDate 2000 Jan 1
    }
        :: List.reverse events
        |> List.reverse


addPeriod : Category -> List Period -> List Period
addPeriod category periods =
    let
        maxId =
            periods
                |> List.map .id
                |> List.maximum
                |> Maybe.withDefault -1

        lastColor =
            periods
                |> List.filter (\p -> p.category == category)
                |> List.sortBy .id
                |> List.reverse
                |> List.head
                |> Maybe.map .color

        color =
            case lastColor of
                Just someColor ->
                    lighten 0.15 someColor

                Nothing ->
                    Colors.categoryColor category
    in
    { id = maxId + 1
    , name = defaultPeriodName category
    , startDate = Date.fromCalendarDate 2000 Jan 1
    , endDate = Just <| Date.fromCalendarDate 2005 Jan 1
    , category = category
    , color = color
    }
        :: List.reverse periods
        |> List.reverse


defaultPeriodName : Category -> String
defaultPeriodName category =
    case category of
        Education ->
            "University of Oxford"

        Activity ->
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


updateEvents : Int -> EventField -> String -> List Event -> List Event
updateEvents id field value events =
    List.map
        (\event ->
            if event.id == id then
                updateEvent field value event

            else
                event
        )
        events


updateEvent : EventField -> String -> Event -> Event
updateEvent field value event =
    case field of
        EventName ->
            { event | name = value }

        EventDate ->
            { event
                | date =
                    value
                        |> Date.fromIsoString
                        |> Result.withDefault event.date
            }


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
        PeriodName ->
            { period | name = value }

        PeriodStartDate ->
            { period
                | startDate =
                    value
                        |> Date.fromIsoString
                        |> Result.withDefault period.startDate
            }

        PeriodEndDate ->
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
