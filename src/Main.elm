module Main exposing (main)

import Browser
import Color exposing (Color)
import Color.Manipulate exposing (lighten)
import Colors
import Date exposing (Interval(..), Unit(..))
import DateRange
import Debouncer.Messages as Debouncer exposing (provideInput)
import Decoder
import Encoder
import File
import File.Download
import File.Select
import Html.Styled exposing (toUnstyled)
import Html.Styled.Lazy exposing (lazy)
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
        , Settings
        , categories
        , categoryFromString
        , initialDebounce
        )
import View exposing (view)


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = lazy view >> toUnstyled
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
    { categories = categories
    , debounce = initialDebounce
    , selectedDate = Nothing
    , today = Date.fromCalendarDate 2000 Jan 1
    , unit = Weeks
    , settings = initialSettings
    , form = initialSettings
    }


initialSettings : Settings
initialSettings =
    { birthdate = Date.fromCalendarDate 1990 Jan 1
    , events = []
    , lifeExpectancy = 73
    , periods = []
    , retirementAge = 65
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEvent ->
            pushDebounceMsg
                { model | form = addEvent model.form }

        AddPeriod category ->
            pushDebounceMsg
                { model | form = addPeriod category model.form }

        DebounceMsg subMsg ->
            Debouncer.update update updateDebouncer subMsg model

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

        Refresh ->
            { model | settings = model.form } |> save

        RemoveEvent id ->
            pushDebounceMsg
                { model | form = removeEvent id model.form }

        RemovePeriod id ->
            pushDebounceMsg
                { model | form = removePeriod id model.form }

        SelectDate date ->
            ( { model | selectedDate = date }, Cmd.none )

        SetBirthdate s ->
            pushDebounceMsg
                { model | form = setBirthdate s model.form }

        SetLifeExpectancy s ->
            pushDebounceMsg
                { model | form = setLifeExpectancy s model.form }

        SetRetirementAge s ->
            pushDebounceMsg
                { model | form = setRetirementAge s model.form }

        SetUnit s ->
            { model
                | selectedDate = Nothing
                , unit =
                    s
                        |> DateRange.unitFromString
                        |> Maybe.withDefault model.unit
            }
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
            pushDebounceMsg
                { model | form = updateEvents id field value model.form }

        UpdatePeriod id field value ->
            pushDebounceMsg
                { model | form = updatePeriods id field value model.form }


pushDebounceMsg : Model -> ( Model, Cmd Msg )
pushDebounceMsg updatedModel =
    let
        debounceMsg =
            Refresh
                |> provideInput
                |> DebounceMsg

        ( newModel, debounceCmd ) =
            update debounceMsg updatedModel
    in
    ( newModel, debounceCmd )


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = DebounceMsg
    , getDebouncer = .debounce
    , setDebouncer =
        \debouncer model ->
            { model | debounce = debouncer }
    }


save : Model -> ( Model, Cmd Msg )
save model =
    ( model, sendModelToPort model )


sendModelToPort : Model -> Cmd msg
sendModelToPort model =
    model
        |> Encoder.encode
        |> Json.Encode.encode 0
        |> Ports.storeModel


setBirthdate : String -> Settings -> Settings
setBirthdate s settings =
    { settings
        | birthdate =
            s
                |> Date.fromIsoString
                |> Result.withDefault settings.birthdate
    }


setLifeExpectancy : String -> Settings -> Settings
setLifeExpectancy s settings =
    { settings | lifeExpectancy = toIntWithDefault settings.lifeExpectancy s }


setRetirementAge : String -> Settings -> Settings
setRetirementAge s settings =
    { settings | retirementAge = toIntWithDefault settings.retirementAge s }


addEvent : Settings -> Settings
addEvent settings =
    let
        insertEvent : List Event -> List Event
        insertEvent events =
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
    in
    { settings | events = insertEvent settings.events }


removeEvent : Int -> Settings -> Settings
removeEvent id settings =
    let
        events =
            List.filter (\event -> event.id /= id) settings.events
    in
    { settings | events = events }


addPeriod : Category -> Settings -> Settings
addPeriod category settings =
    let
        maxId =
            settings.periods
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

        newPeriods =
            (newPeriod :: List.reverse settings.periods)
                |> List.reverse
                |> updateColors category
    in
    { settings | periods = newPeriods }


removePeriod : Int -> Settings -> Settings
removePeriod id settings =
    let
        category =
            settings.periods
                |> List.filter (\period -> period.id == id)
                |> List.head
                |> Maybe.map .category
                |> Maybe.withDefault Activity

        periods =
            settings.periods
                |> List.filter (\period -> period.id /= id)
                |> updateColors category
    in
    { settings | periods = periods }


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


updateEvents : Int -> EventField -> String -> Settings -> Settings
updateEvents id field value settings =
    let
        events =
            List.map
                (\event ->
                    if event.id == id then
                        updateEvent field value event

                    else
                        event
                )
                settings.events
    in
    { settings | events = events }


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


updatePeriods : Int -> PeriodField -> String -> Settings -> Settings
updatePeriods id field value settings =
    let
        periods =
            List.map
                (\period ->
                    if period.id == id then
                        updatePeriod field value period

                    else
                        period
                )
                settings.periods
    in
    { settings | periods = periods }


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
