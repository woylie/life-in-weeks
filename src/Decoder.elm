module Decoder exposing (decoder)

import Color exposing (Color)
import Date exposing (Date, Interval(..), Unit(..))
import DateRange
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , float
        , int
        , list
        , map
        , string
        )
import Json.Decode.Extra exposing (fromMaybe, fromResult)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Time exposing (Month(..))
import Types
    exposing
        ( Category
        , ColorMap
        , Event
        , Model
        , Period
        , Settings
        , categoryFromString
        )


decoder : Decoder Model
decoder =
    Decode.succeed Model
        |> required "categories" (list category)
        |> hardcoded Nothing
        |> hardcoded (Date.fromCalendarDate 2000 Jan 1)
        |> required "unit" unit
        |> required "settings" settings


settings : Decoder Settings
settings =
    Decode.succeed Settings
        |> required "birthdate" date
        |> required "events" (list event)
        |> required "lifeExpectancy" int
        |> required "periods" (list period)
        |> required "retirementAge" int


date : Decoder Date
date =
    string |> andThen (Date.fromIsoString >> fromResult)


event : Decoder Event
event =
    Decode.succeed Event
        |> required "id" int
        |> required "name" string
        |> required "date" date


period : Decoder Period
period =
    Decode.succeed Period
        |> required "id" int
        |> required "name" string
        |> required "startDate" date
        |> optional "endDate" (map Just date) Nothing
        |> required "category" category
        |> required "color" color


category : Decoder Category
category =
    string |> andThen (categoryFromString >> fromMaybe "invalid category")


color : Decoder Color
color =
    colorMap |> map Color.fromRgba


colorMap : Decoder ColorMap
colorMap =
    Decode.succeed ColorMap
        |> required "red" float
        |> required "green" float
        |> required "blue" float
        |> required "alpha" float


unit : Decoder Unit
unit =
    string |> andThen (DateRange.unitFromString >> fromMaybe "invalid unit")
