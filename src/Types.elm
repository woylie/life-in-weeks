module Types exposing
    ( Category(..)
    , Dates
    , Model
    , Msg(..)
    , Period
    , PeriodField(..)
    , Phase(..)
    , State(..)
    )

import Color exposing (Color)
import Date exposing (Date, Unit)


type alias Model =
    { birthdate : Date
    , lifeExpectancy : Int
    , periods : List Period
    , retirementAge : Int
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


type Msg
    = AddPeriod Category
    | ReceiveDate Date
    | RemovePeriod Int
    | SetBirthdate String
    | SetLifeExpectancy String
    | SetRetirementAge String
    | SetUnit String
    | UpdatePeriod Int PeriodField String


type State
    = Past
    | Present
    | Future


type Phase
    = Default
    | Retirement
    | PastLifeExpectancy
    | Phase Period
