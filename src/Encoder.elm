module Encoder exposing (encode)

import Color exposing (Color)
import Date exposing (Date)
import DateRange
import Json.Encode exposing (Value, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Types
    exposing
        ( Category
        , Event
        , Model
        , Period
        , Settings
        , categoryToString
        )


encode : Model -> Value
encode model =
    object
        [ ( "categories", list category model.categories )
        , ( "unit", string (DateRange.unitToString model.unit) )
        , ( "settings", settings model.settings )
        ]


settings : Settings -> Value
settings s =
    object
        [ ( "birthdate", date s.birthdate )
        , ( "events", list event s.events )
        , ( "lifeExpectancy", int s.lifeExpectancy )
        , ( "periods", list period s.periods )
        , ( "retirementAge", int s.retirementAge )
        ]


event : Event -> Value
event e =
    object
        [ ( "id", int e.id )
        , ( "name", string e.name )
        , ( "date", date e.date )
        ]


period : Period -> Value
period p =
    object
        [ ( "id", int p.id )
        , ( "name", string p.name )
        , ( "startDate", date p.startDate )
        , ( "endDate", maybe date p.endDate )
        , ( "category", category p.category )
        , ( "color", color p.color )
        ]


category : Category -> Value
category c =
    string (categoryToString c)


color : Color -> Value
color c =
    let
        colorMap =
            Color.toRgba c
    in
    object
        [ ( "red", float colorMap.red )
        , ( "green", float colorMap.green )
        , ( "blue", float colorMap.blue )
        , ( "alpha", float colorMap.alpha )
        ]


date : Date -> Value
date d =
    string (Date.toIsoString d)
