module Models.SuspendedToggleEvent exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Models.YmdDate exposing (YmdDate, decodeYmdDate)


type alias SuspendedToggleEvent =
    { id : String, habitId : String, toggleDate : YmdDate, suspended : Bool }


decodeSuspendedToggleEvent : Decode.Decoder SuspendedToggleEvent
decodeSuspendedToggleEvent =
    decode SuspendedToggleEvent
        |> required "_id" Decode.string
        |> required "habit_id" Decode.string
        |> required "date" decodeYmdDate
        |> required "suspended" Decode.bool
