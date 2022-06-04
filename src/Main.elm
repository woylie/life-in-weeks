module Main exposing (main)

import Browser
import Color exposing (Color)
import Color.Manipulate exposing (lighten)
import Colors
import Date exposing (Interval(..), Unit(..))
import DateRange
import Decoder
import Encoder
import File
import File.Download
import File.Select
import Html.Styled exposing (toUnstyled)
import Json.Decode
import Json.Encode
import Ports
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
        , categories
        , categoryFromString
        )
import View exposing (view)


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


init : Maybe String -> ( Model, Cmd Msg )
init maybeStoredState =
    let
        model =
            maybeStoredState
                |> Maybe.andThen
                    (Json.Decode.decodeString Decoder.decoder
                        >> Result.toMaybe
                    )
                |> Maybe.withDefault initialModel
    in
    ( model
    , Date.today |> Task.perform ReceiveDate
    )


initialModel : Model
initialModel =
    { birthdate = Date.fromCalendarDate 1990 Jan 1
    , categories = categories
    , events = []
    , lifeExpectancy = 73
    , periods = []
    , retirementAge = 65
    , selectedDate = Nothing
    , today = Date.fromCalendarDate 2000 Jan 1
    , unit = Weeks
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEvent ->
            { model | events = addEvent model.events } |> save

        AddPeriod category ->
            { model | periods = addPeriod category model.periods } |> save

        Export ->
            ( model
            , File.Download.string
                "life-in-weeks.json"
                "application/json"
                (Json.Encode.encode 2 (Encoder.encode model))
            )

        JsonRequested ->
            ( model, File.Select.file [ "application/json" ] JsonSelected )

        JsonSelected file ->
            ( model, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded content ->
            case Json.Decode.decodeString Decoder.decoder content of
                Ok importedModel ->
                    { importedModel | today = model.today } |> save

                Err _ ->
                    ( model, Cmd.none )

        ReceiveDate date ->
            ( { model | today = date }, Cmd.none )

        RemoveEvent id ->
            { model
                | events =
                    List.filter (\event -> event.id /= id) model.events
            }
                |> save

        RemovePeriod id ->
            { model | periods = removePeriod id model.periods } |> save

        SelectDate date ->
            ( { model | selectedDate = date }, Cmd.none )

        SetBirthdate s ->
            { model
                | birthdate =
                    s
                        |> Date.fromIsoString
                        |> Result.withDefault model.birthdate
            }
                |> save

        SetLifeExpectancy s ->
            { model | lifeExpectancy = toIntWithDefault model.lifeExpectancy s }
                |> save

        SetRetirementAge s ->
            { model | retirementAge = toIntWithDefault model.retirementAge s }
                |> save

        SetUnit s ->
            { model
                | selectedDate = Nothing
                , unit = s |> DateRange.unitFromString |> Maybe.withDefault model.unit
            }
                |> save

        SortEvents ->
            { model | events = sortEvents model.events }
                |> save

        SortPeriods ->
            { model | periods = sortPeriods model.periods }
                |> save

        ToggleCategory s ->
            let
                toggleCategory : Category -> List Category
                toggleCategory category =
                    if List.member category model.categories then
                        List.filter (\c -> category /= c) model.categories

                    else
                        category :: model.categories

                newCategories =
                    s
                        |> categoryFromString
                        |> Maybe.map toggleCategory
                        |> Maybe.withDefault model.categories
            in
            { model | categories = newCategories } |> save

        UpdateEvent id field value ->
            { model | events = updateEvents id field value model.events }
                |> save

        UpdatePeriod id field value ->
            { model | periods = updatePeriods id field value model.periods }
                |> save


save : Model -> ( Model, Cmd Msg )
save model =
    ( model, sendModelToPort model )


sendModelToPort : Model -> Cmd msg
sendModelToPort model =
    model
        |> Encoder.encode
        |> Json.Encode.encode 0
        |> Ports.storeModel


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

        newPeriod : Period
        newPeriod =
            { id = maxId + 1
            , name = defaultPeriodName category
            , startDate = Date.fromCalendarDate 2000 Jan 1
            , endDate = Just <| Date.fromCalendarDate 2005 Jan 1
            , category = category
            , color = Colors.categoryColor category
            }
    in
    newPeriod
        :: List.reverse periods
        |> List.reverse
        |> updateColors category


removePeriod : Int -> List Period -> List Period
removePeriod id periods =
    let
        category =
            periods
                |> List.filter (\period -> period.id == id)
                |> List.head
                |> Maybe.map .category
                |> Maybe.withDefault Activity
    in
    periods
        |> List.filter (\period -> period.id /= id)
        |> updateColors category


updateColors : Category -> List Period -> List Period
updateColors category periods =
    let
        foldFunc : Period -> ( Color, List Period ) -> ( Color, List Period )
        foldFunc period ( color, accPeriods ) =
            if period.category == category then
                ( lighten 0.06 color, { period | color = color } :: accPeriods )

            else
                ( color, period :: accPeriods )

        ( _, updatedPeriods ) =
            List.foldl foldFunc
                ( Colors.categoryColor category, [] )
                periods
    in
    List.reverse updatedPeriods


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


sortPeriods : List Period -> List Period
sortPeriods periods =
    List.sortWith (\p1 p2 -> Date.compare p1.startDate p2.startDate) periods


sortEvents : List Event -> List Event
sortEvents events =
    List.sortWith (\e1 e2 -> Date.compare e1.date e2.date) events


toIntWithDefault : Int -> String -> Int
toIntWithDefault default s =
    s
        |> String.toInt
        |> Maybe.withDefault default


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
