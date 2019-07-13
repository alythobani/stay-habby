module Models.User exposing
    ( User
    , decodeUser
    , graphQLOutputString
    )

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)


type alias User =
    { id : String
    , username : String
    , displayName : String
    , emailAddress : Maybe String
    }


decodeUser : Decode.Decoder User
decodeUser =
    Decode.succeed User
        |> required "_id" Decode.string
        |> required "username" Decode.string
        |> required "display_name" Decode.string
        |> optional "email_address" (Decode.maybe Decode.string) Nothing


graphQLOutputString : String
graphQLOutputString =
    """{
      _id
      username
      display_name
      email_address
    }"""
