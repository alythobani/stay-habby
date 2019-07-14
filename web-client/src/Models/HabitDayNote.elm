module Models.HabitDayNote exposing (HabitDayNote, decodeHabitDayNote, graphQLOutputString)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Models.YmdDate exposing (YmdDate, decodeYmdDate)


type alias HabitDayNote =
    { id : String, userId : String, habitId : String, date : YmdDate, note : String }


decodeHabitDayNote : Decode.Decoder HabitDayNote
decodeHabitDayNote =
    Decode.succeed HabitDayNote
        |> required "_id" Decode.string
        |> required "user_id" Decode.string
        |> required "habit_id" Decode.string
        |> required "date" decodeYmdDate
        |> required "note" Decode.string


graphQLOutputString : String
graphQLOutputString =
    """{
      _id,
      user_id,
      note,
      date {
        year,
        month,
        day
      },
      habit_id
    }"""
