module Types exposing
    ( Category(..)
    , ColorMap
    , Dates
    , Event
    , EventField(..)
    , FieldOpts
    , Model
    , Msg(..)
    , Period
    , PeriodField(..)
    , Phase(..)
    , State(..)
    , categoryFromString
    , categoryToString
    )

import Color exposing (Color)
import Date exposing (Date, Unit)
import File exposing (File)


type alias Model =
    { birthdate : Date
    , events : List Event
    , lifeExpectancy : Int
    , periods : List Period
    , retirementAge : Int
    , selectedDate : Maybe Date
    , today : Date
    , unit : Unit
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


type Msg
    = AddEvent
    | AddPeriod Category
    | Export
    | ReceiveDate Date
    | RemoveEvent Int
    | RemovePeriod Int
    | JsonLoaded String
    | JsonRequested
    | JsonSelected File
    | SelectDate (Maybe Date)
    | SetBirthdate String
    | SetLifeExpectancy String
    | SetRetirementAge String
    | SetUnit String
    | SortEvents
    | SortPeriods
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
