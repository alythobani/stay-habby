module DefaultServices.Util exposing
    ( addToListIfNotPresent
    , encodeBool
    , encodeInt
    , encodeListOfStrings
    , encodeMaybe
    , encodeString
    , firstInstanceInArray
    , firstInstanceInList
    , hiddenDiv
    , notEmpty
    , onKeydown
    , onKeydownPreventDefault
    , onKeydownStopPropagation
    , onKeyupStopPropagation
    , removeFromListIfPresent
    , replaceOrAdd
    , templater
    )

import Array
import DefaultServices.Keyboard as Keyboard
import Dict
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (keyCode, on, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import Json.Encode as Encode


{-| Creates an empty div with class `display-none`.
-}
hiddenDiv : Html msg
hiddenDiv =
    div [ class "display-none" ] []


{-| Returns `Just` a non-empty string or `Nothing` if the string is empty.
-}
notEmpty : String -> Maybe String
notEmpty string =
    if String.isEmpty string then
        Nothing

    else
        Just string


{-| TODO DOC.
-}
templater : Dict.Dict String String -> String -> String
templater dict templateString =
    Dict.foldr
        (\key value template ->
            String.replace ("{{" ++ key ++ "}}") value template
        )
        templateString
        dict


{-| Encode a String as JSON that can be inputed into a graphQL query / mutation.
-}
encodeString : String -> String
encodeString str =
    Encode.encode 0 (Encode.string str)


{-| Encode an Int as JSON that can be inputed into a graphQL query / mutation.
-}
encodeInt : Int -> String
encodeInt int =
    Encode.encode 0 (Encode.int int)


{-| Encode a `Maybe x` as JSON that can be inputed into a graphQL query / mutation.
Encode `Nothing` as `null` and `Just a` as `encodeX a`.
-}
encodeMaybe : Maybe x -> (x -> String) -> String
encodeMaybe maybeX encodeX =
    case maybeX of
        Just x ->
            encodeX x

        Nothing ->
            "null"


{-| Encode a List of Strings as JSON that can be inputed into a graphQL query / mutation.
-}
encodeListOfStrings : List String -> String
encodeListOfStrings stringList =
    Encode.encode 0 (Encode.list Encode.string stringList)


{-| Encode a Bool as JSON that can be inputed into a graphQL query / mutation.
-}
encodeBool : Bool -> String
encodeBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


{-| Add `x` to the end of `list`, if not already present in `list`.
-}
addToListIfNotPresent : List x -> x -> List x
addToListIfNotPresent list x =
    if List.member x list then
        list

    else
        List.append list [ x ]


{-| Remove any instances of `x` from `list`.
-}
removeFromListIfPresent : List a -> a -> List a
removeFromListIfPresent list x =
    if List.member x list then
        List.filter (\a -> a /= x) list

    else
        list


{-| Replace item(s) in list or add item to list if no replacements took place.

TODO Make more efficient

-}
replaceOrAdd : List a -> (a -> Bool) -> a -> List a
replaceOrAdd list pred replaceWith =
    list
        |> List.map
            (\a ->
                if pred a then
                    replaceWith

                else
                    a
            )
        |> (\newList ->
                if newList == list then
                    replaceWith :: list

                else
                    newList
           )


{-| Event handler for handling `keyDown` events.
-}
onKeydown : (Keyboard.Key -> Maybe msg) -> Attribute msg
onKeydown keyToMsg =
    let
        decodeMsgFromCode : Keyboard.Key -> Decode.Decoder msg
        decodeMsgFromCode code =
            code
                |> keyToMsg
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail "")
    in
    on
        "keydown"
        (Decode.andThen decodeMsgFromCode Keyboard.decodeKey)


{-| Event handler for `keyDown` events that also `preventDefault`.

WARNING: It'll only prevent default if your function returns a message not `Nothing`.

-}
onKeydownPreventDefault : (Keyboard.Key -> Maybe msg) -> Attribute msg
onKeydownPreventDefault keyToMsg =
    let
        decodeMsgBoolFromCode : Keyboard.Key -> Decode.Decoder ( msg, Bool )
        decodeMsgBoolFromCode code =
            code
                |> keyToMsg
                |> Maybe.map (\msg -> ( msg, True ))
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail "")
    in
    preventDefaultOn
        "keydown"
        (Decode.andThen decodeMsgBoolFromCode Keyboard.decodeKey)


{-| Event handler for `keyDown` events that also `stopPropagation`.

WARNING: It'll only stop propagation if your function returns a message not `Nothing`.

-}
onKeydownStopPropagation : (Keyboard.Key -> Maybe msg) -> Attribute msg
onKeydownStopPropagation keyToMsg =
    let
        decodeMsgBoolFromCode : Keyboard.Key -> Decode.Decoder ( msg, Bool )
        decodeMsgBoolFromCode code =
            code
                |> keyToMsg
                |> Maybe.map (\msg -> ( msg, True ))
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail "")
    in
    stopPropagationOn
        "keydown"
        (Decode.andThen decodeMsgBoolFromCode Keyboard.decodeKey)


{-| Event handler for `keyUp` events that also `stopPropagation`.

WARNING: It'll only stop propagation if your function returns a message not `Nothing`.

-}
onKeyupStopPropagation : (Keyboard.Key -> Maybe msg) -> Attribute msg
onKeyupStopPropagation keyToMsg =
    let
        decodeMsgBoolFromCode : Keyboard.Key -> Decode.Decoder ( msg, Bool )
        decodeMsgBoolFromCode code =
            code
                |> keyToMsg
                |> Maybe.map (\msg -> ( msg, True ))
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail "")
    in
    stopPropagationOn
        "keyup"
        (Decode.andThen decodeMsgBoolFromCode Keyboard.decodeKey)


{-| Returns `Just` the index and value of the first element in `array` that passes `filterFunction`.
Returns `Nothing` if no element passes `filterFunction`.
-}
firstInstanceInArray : Array.Array a -> (a -> Bool) -> Maybe ( Int, a )
firstInstanceInArray array filterFunction =
    let
        indexedList =
            Array.toIndexedList array
    in
    indexedList |> List.filter (\( index, elem ) -> filterFunction elem) |> List.head


{-| Returns `Just` the index and value of the first element in `list` that passes `filterFunction`.
Returns `Nothing` if no element passes `filterFunction`.
-}
firstInstanceInList : List a -> (a -> Bool) -> Maybe ( Int, a )
firstInstanceInList list filterFunction =
    let
        indexedList =
            list |> Array.fromList |> Array.toIndexedList
    in
    indexedList |> List.filter (\( index, elem ) -> filterFunction elem) |> List.head
