module Subscriptions exposing (subscriptions)

import DefaultServices.Keyboard as Keyboard
import Model exposing (Model)
import Msg exposing (Msg(..))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (60 * 1000) TickMinute
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]
