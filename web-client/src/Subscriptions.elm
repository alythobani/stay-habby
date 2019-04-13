module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (30 * 1000) TickMinute
        , Sub.map KeyboardExtraMsg KK.subscriptions
        ]
