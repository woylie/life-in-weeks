module Types exposing
    ( Category(..)
    , ColorMap
    , Dates
    , Event
    , EventField(..)
    , FieldOpts
    , Form
    , Model
    , Msg(..)
    , Period
    , PeriodColor
    , PeriodField(..)
    , Phase(..)
    , Settings
    , State(..)
    , categories
    , categoryFromString
    , categoryToString
    , initialDebounce
    , settingsToForm
    )

import Color exposing (Color)
import Date exposing (Date, Unit)
import Debouncer.Messages as Debouncer
    exposing
        ( Debouncer
        , fromSeconds
        , settleWhenQuietFor
        , toDebouncer
        )
import File exposing (File)


type alias Model =
    { categories : List Category
    , debounce : Debouncer Msg
    , selectedDate : Maybe Date
    , today : Date
    , unit : Unit
    , settings : Settings
    , form : Form
    }


type alias Settings =
    { birthdate : Date
    , events : List Event
    , lifeExpectancy : Int
    , periods : List Period
    , retirementAge : Int
    }


type alias Form =
    { birthdate : Date
    , events : List Event
    , lifeExpectancy : String
    , periods : List Period
    , retirementAge : String
    }


settingsToForm : Settings -> Form
settingsToForm settings =
    { birthdate = settings.birthdate
    , events = settings.events
    , lifeExpectancy = String.fromInt settings.lifeExpectancy
    , periods = settings.periods
    , retirementAge = String.fromInt settings.retirementAge
    }


type alias Dates =
    { death : Date
    , retirement : Date
    }


type alias Period =
    { id : Int
    , name : String
    , startDate : Date
    , endDate : Maybe Date
    , category : Category
    , color : Color
    }


type alias PeriodColor =
    { color : Color
    , endDate : Maybe Date
    , startDate : Date
    }


type PeriodField
    = PeriodName
    | PeriodStartDate
    | PeriodEndDate


type alias Event =
    { id : Int
    , name : String
    , date : Date
    }


type EventField
    = EventName
    | EventDate


type Category
    = Education
    | Activity
    | Membership
    | Other
    | Relationship
    | Residence
    | Work


categoryToString : Category -> String
categoryToString category =
    case category of
        Activity ->
            "Activity"

        Education ->
            "Education"

        Membership ->
            "Membership"

        Other ->
            "Other"

        Relationship ->
            "Relationship"

        Residence ->
            "Residence"

        Work ->
            "Work"


categoryFromString : String -> Maybe Category
categoryFromString s =
    case s of
        "Activity" ->
            Just Activity

        "Education" ->
            Just Education

        "Membership" ->
            Just Membership

        "Other" ->
            Just Other

        "Relationship" ->
            Just Relationship

        "Residence" ->
            Just Residence

        "Work" ->
            Just Work

        _ ->
            Nothing


categories : List Category
categories =
    [ Activity
    , Education
    , Membership
    , Other
    , Relationship
    , Residence
    , Work
    ]


type Msg
    = AddEvent
    | AddPeriod Category
    | DebounceMsg (Debouncer.Msg Msg)
    | Export
    | ReceiveDate Date
    | RemoveEvent Int
    | RemovePeriod Int
    | JsonLoaded String
    | JsonRequested
    | JsonSelected File
    | Refresh
    | SelectDate (Maybe Date)
    | SetBirthdate String
    | SetLifeExpectancy String
    | SetRetirementAge String
    | SetUnit String
    | ToggleCategory String
    | UpdateEvent Int EventField String
    | UpdatePeriod Int PeriodField String


type State
    = Past
    | Present
    | Future
    | Selected


type Phase
    = Default
    | Retirement
    | PastLifeExpectancy
    | Phase Period


type alias FieldOpts msg =
    { min : Maybe Int
    , max : Maybe Int
    , onBlur : Maybe msg
    , required : Bool
    }


type alias ColorMap =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


initialDebounce : Debouncer Msg
initialDebounce =
    Debouncer.manual
        |> settleWhenQuietFor (Just <| fromSeconds 1)
        |> toDebouncer
