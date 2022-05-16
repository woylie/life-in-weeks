module Types exposing (Model, Msg(..), State(..))

import Date exposing (Date, Unit)


type alias Model =
    { birthdate : Date
    , lifeExpectancy : Int
    , today : Date
    , unit : Unit
    }


type Msg
    = ReceiveDate Date
    | SetBirthdate String
    | SetLifeExpectancy String
    | SetUnit String


type State
    = Past
    | Present
    | Future
