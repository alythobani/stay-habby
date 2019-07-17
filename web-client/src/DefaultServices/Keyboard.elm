module DefaultServices.Keyboard exposing
    ( Key(..)
    , KeyCode
    , Model
    , Msg(..)
    , codeBook
    , codeDict
    , decodeKey
    , decodeKeyCode
    , fromCode
    , init
    , prettyPrintKey
    , subscriptions
    , update
    )

{- This module got lots of inspiration (and copied code) from ohanhi's Keyboard.Extra module,
   which is, as of the moment I'm writing this, deprecated for Elm 0.19.
   Their code is here https://github.com/ohanhi/keyboard-extra/blob/3.0.4/src/Keyboard/Extra.elm
-}

import Browser.Events as Events
import Dict
import Json.Decode as Decode
import Set


type alias KeyCode =
    String


type alias Model =
    Set.Set KeyCode


{-| Use this to initialize the component.
-}
init : Model
init =
    Set.empty


{-| You need to call this to have the component update.
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        Down code ->
            Set.insert code model

        Up code ->
            Set.remove code model

        OnVisibilityChange _ ->
            -- Whether the user is exiting the screen or entering it, reset our list of their pressed keys
            init


{-| The message type `DefaultServices.Keyboard` uses.
-}
type Msg
    = Down KeyCode
    | Up KeyCode
    | OnVisibilityChange Events.Visibility


{-| You will need to add this to your program's subscriptions.
-}
subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Events.onKeyDown (Decode.map Down decodeKeyCode)
        , Events.onKeyUp (Decode.map Up decodeKeyCode)
        , Events.onVisibilityChange OnVisibilityChange
        ]


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
    | ControlRight
    | AltLeft
    | MetaLeft
    | OSLeft
    | Space
    | MetaRight
    | OSRight
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


prettyPrintKey : Key -> String
prettyPrintKey key =
    case key of
        Other ->
            "Other"

        Escape ->
            "Escape"

        Backquote ->
            "`"

        Digit1 ->
            "1"

        Digit2 ->
            "2"

        Digit3 ->
            "3"

        Digit4 ->
            "4"

        Digit5 ->
            "5"

        Digit6 ->
            "6"

        Digit7 ->
            "7"

        Digit8 ->
            "8"

        Digit9 ->
            "9"

        Digit0 ->
            "0"

        Minus ->
            "-"

        Equal ->
            "="

        Backspace ->
            "Backspace"

        Delete ->
            "Delete"

        Tab ->
            "Tab"

        CapsLock ->
            "CapsLock"

        ShiftLeft ->
            "Shift (Left)"

        ControlLeft ->
            "Ctrl (Left)"

        ControlRight ->
            "Ctrl (Right)"

        AltLeft ->
            "Alt (Left)"

        MetaLeft ->
            "Cmd (Left)"

        OSLeft ->
            "OS (Left)"

        Space ->
            "Space"

        MetaRight ->
            "Cmd (Right)"

        OSRight ->
            "OS (Right)"

        AltRight ->
            "Alt (Right)"

        ArrowLeft ->
            "Left Arrow"

        ArrowRight ->
            "Right Arrow"

        ArrowUp ->
            "Up Arrow"

        ArrowDown ->
            "Down Arrow"

        ShiftRight ->
            "Shift (Right)"

        Enter ->
            "Enter"

        Backslash ->
            "\\"

        Slash ->
            "/"

        Period ->
            "."

        Comma ->
            ","

        Semicolon ->
            ";"

        Quote ->
            "'"

        BracketLeft ->
            "["

        BracketRight ->
            "]"

        KeyA ->
            "A"

        KeyB ->
            "B"

        KeyC ->
            "C"

        KeyD ->
            "D"

        KeyE ->
            "E"

        KeyF ->
            "F"

        KeyG ->
            "G"

        KeyH ->
            "H"

        KeyI ->
            "I"

        KeyJ ->
            "J"

        KeyK ->
            "K"

        KeyL ->
            "L"

        KeyM ->
            "M"

        KeyN ->
            "N"

        KeyO ->
            "O"

        KeyP ->
            "P"

        KeyQ ->
            "Q"

        KeyR ->
            "R"

        KeyS ->
            "S"

        KeyT ->
            "T"

        KeyU ->
            "U"

        KeyV ->
            "V"

        KeyW ->
            "W"

        KeyX ->
            "X"

        KeyY ->
            "Y"

        KeyZ ->
            "Z"


{-| Convert a key code into a `Key`.
fromCode "ArrowLeft" == ArrowLeft
-}
fromCode : KeyCode -> Key
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
    Decode.map fromCode decodeKeyCode


{-| A `Json.Decoder` for grabbing `event.code` and turning it into a `KeyCode`
import Json.Decode as Json
onKey : (KeyCode -> msg) -> Attribute msg
onKey tagger =
on "keydown" (Json.map tagger decodeKeyCode)
-}
decodeKeyCode : Decode.Decoder KeyCode
decodeKeyCode =
    Decode.field "code" Decode.string


codeDict : Dict.Dict KeyCode Key
codeDict =
    Dict.fromList codeBook


codeBook : List ( KeyCode, Key )
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
    , ( "ControlRight", ControlRight )
    , ( "AltLeft", AltLeft )
    , ( "MetaLeft", MetaLeft )
    , ( "OSLeft", OSLeft )
    , ( "Space", Space )
    , ( "MetaRight", MetaRight )
    , ( "OSRight", OSRight )
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
