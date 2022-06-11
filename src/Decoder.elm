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
        , initialDebounce
        )


decoder : Decoder Model
decoder =
    Decode.succeed Model
        |> required "categories" (list category)
        |> hardcoded initialDebounce
        |> hardcoded Nothing
        |> hardcoded (Date.fromCalendarDate 2000 Jan 1)
        |> required "unit" unit
        |> required "settings" settings
        |> required "settings" settings


settings : Decoder Settings
settings =
    Decode.succeed Settings
        |> required "birthdate" date
        |> required "events" (list event |> map sortEvents)
        |> required "lifeExpectancy" int
        |> required "periods" (list period |> map sortPeriods)
        |> required "retirementAge" int


sortPeriods : List Period -> List Period
sortPeriods periods =
    List.sortWith (\p1 p2 -> Date.compare p1.startDate p2.startDate) periods


sortEvents : List Event -> List Event
sortEvents events =
    List.sortWith (\e1 e2 -> Date.compare e1.date e2.date) events


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
