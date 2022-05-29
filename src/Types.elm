module Types exposing
    ( Category(..)
    , Dates
    , FieldOpts
    , Model
    , Msg(..)
    , Period
    , PeriodField(..)
    , Phase(..)
    , State(..)
    , categoryToString
    )

import Color exposing (Color)
import Date exposing (Date, Unit)


type alias Model =
    { birthdate : Date
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
    = Name
    | StartDate
    | EndDate


type Category
    = Education
    | Hobby
    | Membership
    | Other
    | Relationship
    | Residence
    | Work


categoryToString : Category -> String
categoryToString category =
    case category of
        Education ->
            "Education"

        Hobby ->
            "Hobby"

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


type Msg
    = AddPeriod Category
    | ReceiveDate Date
    | RemovePeriod Int
    | SelectDate (Maybe Date)
    | SetBirthdate String
    | SetLifeExpectancy String
    | SetRetirementAge String
    | SetUnit String
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


type alias FieldOpts =
    { min : Maybe Int
    , max : Maybe Int
    , required : Bool
    }
