module Models.Graph exposing (NumberOfDaysToShow(..))

{-| Module related to graph dialogs
-}


type NumberOfDaysToShow
    = AllTime
    | LastXDays Int
