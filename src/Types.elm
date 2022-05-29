module Types exposing (Dates, Model, Msg(..), Phase(..), State(..))

import Date exposing (Date, Unit)


type alias Model =
    { birthdate : Date
    , lifeExpectancy : Int
    , retirementAge : Int
    , today : Date
    , unit : Unit
    }


type alias Dates =
    { death : Date
    , retirement : Date
    }


type Msg
    = ReceiveDate Date
    | SetBirthdate String
    | SetLifeExpectancy String
    | SetRetirementAge String
    | SetUnit String


type State
    = Past
    | Present
    | Future


type Phase
    = Default
    | Retirement
