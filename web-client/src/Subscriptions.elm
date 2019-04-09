module Subscriptions exposing (subscriptions)

import Keyboard.Extra as KK
import Model exposing (Model)
import Msg exposing (Msg(..))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.minute TickMinute
        , Sub.map KeyboardExtraMsg KK.subscriptions
        ]
