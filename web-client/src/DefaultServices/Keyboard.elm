module DefaultServices.Keyboard exposing (Key(..), codeBook, codeDict, decodeKey, fromCode)

{- This module got lots of inspiration (and copied code) from ohanhi's Keyboard.Extra module,
   which is, as of the moment I'm writing this, deprecated for Elm 0.19.
   Their code is here https://github.com/ohanhi/keyboard-extra/blob/3.0.4/src/Keyboard/Extra.elm
-}

import Dict
import Json.Decode as Decode


{-| Keys on the keyboard. Direct association with the "code" of a KeyboardEvent.
See <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code> for more information.
-}
type Key
    = Other
    | Escape
    | Backquote
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | Digit0
    | Minus
    | Equal
    | Backspace
    | Delete -- Triggered on Mac by Fn + Backspace together
    | Tab
    | CapsLock -- Note: only seems to be triggered if caps lock is toggled ON by the keystroke
    | ShiftLeft
    | ControlLeft
    | AltLeft
    | MetaLeft
    | Space
    | MetaRight
    | AltRight
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | ShiftRight
    | Enter
    | Backslash
    | Slash
    | Period
    | Comma
    | Semicolon
    | Quote
    | BracketLeft
    | BracketRight
      -- Letters
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ


{-| Convert a key code into a `Key`.
fromCode "ArrowLeft" == ArrowLeft
-}
fromCode : String -> Key
fromCode code =
    codeDict
        |> Dict.get code
        |> Maybe.withDefault Other


{-| A `Json.Decoder` for grabbing `event.code` and turning it into a `Key`
import Json.Decode as Json
onKey : (Key -> msg) -> Attribute msg
onKey tagger =
on "keydown" (Json.map tagger decodeKey)
-}
decodeKey : Decode.Decoder Key
decodeKey =
    Decode.map fromCode (Decode.field "code" Decode.string)


codeDict : Dict.Dict String Key
codeDict =
    Dict.fromList codeBook


codeBook : List ( String, Key )
codeBook =
    [ ( "Escape", Escape )
    , ( "Backquote", Backquote )
    , ( "Digit1", Digit1 )
    , ( "Digit2", Digit2 )
    , ( "Digit3", Digit3 )
    , ( "Digit4", Digit4 )
    , ( "Digit5", Digit5 )
    , ( "Digit6", Digit6 )
    , ( "Digit7", Digit7 )
    , ( "Digit8", Digit8 )
    , ( "Digit9", Digit9 )
    , ( "Digit0", Digit0 )
    , ( "Minus", Minus )
    , ( "Equal", Equal )
    , ( "Backspace", Backspace )
    , ( "Delete", Delete )
    , ( "Tab", Tab )
    , ( "CapsLock", CapsLock )
    , ( "ShiftLeft", ShiftLeft )
    , ( "ControlLeft", ControlLeft )
    , ( "AltLeft", AltLeft )
    , ( "MetaLeft", MetaLeft )
    , ( "Space", Space )
    , ( "MetaRight", MetaRight )
    , ( "AltRight", AltRight )
    , ( "ArrowLeft", ArrowLeft )
    , ( "ArrowRight", ArrowRight )
    , ( "ArrowUp", ArrowUp )
    , ( "ArrowDown", ArrowDown )
    , ( "ShiftRight", ShiftRight )
    , ( "Enter", Enter )
    , ( "Backslash", Backslash )
    , ( "Slash", Slash )
    , ( "Period", Period )
    , ( "Comma", Comma )
    , ( "Semicolon", Semicolon )
    , ( "Quote", Quote )
    , ( "BracketLeft", BracketLeft )
    , ( "BracketRight", BracketRight )
    , ( "KeyA", KeyA )
    , ( "KeyB", KeyB )
    , ( "KeyC", KeyC )
    , ( "KeyD", KeyD )
    , ( "KeyE", KeyE )
    , ( "KeyF", KeyF )
    , ( "KeyG", KeyG )
    , ( "KeyH", KeyH )
    , ( "KeyI", KeyI )
    , ( "KeyJ", KeyJ )
    , ( "KeyK", KeyK )
    , ( "KeyL", KeyL )
    , ( "KeyM", KeyM )
    , ( "KeyN", KeyN )
    , ( "KeyO", KeyO )
    , ( "KeyP", KeyP )
    , ( "KeyQ", KeyQ )
    , ( "KeyR", KeyR )
    , ( "KeyS", KeyS )
    , ( "KeyT", KeyT )
    , ( "KeyU", KeyU )
    , ( "KeyV", KeyV )
    , ( "KeyW", KeyW )
    , ( "KeyX", KeyX )
    , ( "KeyY", KeyY )
    , ( "KeyZ", KeyZ )
    ]
