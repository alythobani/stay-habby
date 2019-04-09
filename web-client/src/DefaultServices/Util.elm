module DefaultServices.Util exposing (encodeBool, firstIndexInList, helper, hiddenDiv, notEmpty, onKeydown, onKeydownPreventDefault, onKeydownWithOptions, replaceOrAdd, templater)

import Dict
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (Options, defaultOptions, keyCode, onWithOptions)
import Json.Decode as Decode
import String.Extra


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
            String.Extra.replace ("{{" ++ key ++ "}}") value template
        )
        templateString
        dict


encodeBool : Bool -> String
encodeBool bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


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
onKeydownWithOptions : Options -> (KK.Key -> Maybe msg) -> Attribute msg
onKeydownWithOptions options keyToMsg =
    let
        decodeMsgFromKeyCode code =
            KK.fromCode code
                |> keyToMsg
                ||> Decode.succeed
                ?> Decode.fail ""
    in
    onWithOptions
        "keydown"
        options
        (Decode.andThen decodeMsgFromKeyCode keyCode)


{-| Default event handler for `keyDown` events.
-}
onKeydown : (KK.Key -> Maybe msg) -> Attribute msg
onKeydown =
    onKeydownWithOptions defaultOptions


{-| Event handler for `keyDown` events that also `preventDefault`.

WARNING: It'll only prevent default if your function returns a message not `Nothing`.

-}
onKeydownPreventDefault : (KK.Key -> Maybe msg) -> Attribute msg
onKeydownPreventDefault =
    onKeydownWithOptions
        { preventDefault = True
        , stopPropagation = False
        }


{-| Helper function for `firstIndexInList`
-}
helper : List a -> a -> Int -> Maybe Int
helper list elem offset =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == elem then
                Just offset

            else
                helper xs elem (offset + 1)


{-| Returns `Just` the index of the first occurrence of `element` in `list`.
Returns `Nothing` if `element` is not found in `list`.
-}
firstIndexInList : List a -> a -> Maybe Int
firstIndexInList list element =
    helper list element 0
