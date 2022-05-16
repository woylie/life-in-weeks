module Types exposing (Model, Msg(..))

import Date exposing (Date, Unit)


type alias Model =
    { birthdate : Date
    , lifeExpectancy : Int
    , unit : Unit
    }


type Msg
    = NoOp
    | SetLifeExpectancy String
    | SetUnit String
