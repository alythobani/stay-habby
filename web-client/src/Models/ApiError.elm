module Models.ApiError exposing (ApiError(..), GraphqlErrorInfo, GraphqlErrorLocation, decodeGraphqlErrorInfo, decodeGraphqlErrorLocation, decoder, toString)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


{-| An error from the api represented as a union of possible errors.

The most common error will be `GraphqlError` as this represents all the errors we can get from graphql.

`InternalError` represents an error from the backend that we didn't expect, the most generic error.

-}
type ApiError
    = UnexpectedPayload
    | RawTimeout
    | RawNetworkError
    | GraphqlError (List GraphqlErrorInfo)
    | InternalError


{-| An error in the format generated by graphql.
-}
type alias GraphqlErrorInfo =
    { message : String
    , locations : List GraphqlErrorLocation
    , queryPath : List String
    }


{-| @refer `GraphqlErrorInfo`.
-}
type alias GraphqlErrorLocation =
    { line : Int, column : Int }


toString : ApiError -> String
toString apiError =
    case apiError of
        UnexpectedPayload ->
            "Unexpected Payload"

        RawTimeout ->
            "Raw Timeout"

        RawNetworkError ->
            "Raw Network Error"

        GraphqlError graphqlErrorInfoList ->
            graphqlErrorInfoList |> List.map .message |> String.join ", "

        InternalError ->
            "Internal Error"


decoder : Decode.Decoder ApiError
decoder =
    let
        errorDecoder =
            Decode.at [ "errors" ] <| Decode.list <| decodeGraphqlErrorInfo
    in
    Decode.map GraphqlError errorDecoder


decodeGraphqlErrorInfo : Decode.Decoder GraphqlErrorInfo
decodeGraphqlErrorInfo =
    Decode.succeed GraphqlErrorInfo
        |> required "message" Decode.string
        |> required "locations" (Decode.list decodeGraphqlErrorLocation)
        |> required "query-path" (Decode.list Decode.string)


decodeGraphqlErrorLocation : Decode.Decoder GraphqlErrorLocation
decodeGraphqlErrorLocation =
    Decode.succeed GraphqlErrorLocation
        |> required "line" Decode.int
        |> required "column" Decode.int
