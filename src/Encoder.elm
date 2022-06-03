module Encoder exposing (encode)

import Color exposing (Color)
import Date exposing (Date)
import DateRange
import Json.Encode exposing (Value, bool, dict, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Types
    exposing
        ( Category
        , Event
        , Model
        , Period
        , categoryToString
        )


encode : Model -> Value
encode model =
    object
        [ ( "birthdate", date model.birthdate )
        , ( "categories", dict identity bool model.categories )
        , ( "events", list event model.events )
        , ( "lifeExpectancy", int model.lifeExpectancy )
        , ( "periods", list period model.periods )
        , ( "retirementAge", int model.retirementAge )
        , ( "unit", string (DateRange.unitToString model.unit) )
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
