module Mish.Config where

import           Mish.HexagonalGrid

data Config =
  Config { radius                :: Radius
         , roomAttempts          :: Int
         , minRoomRadius         :: Int
         , maxRoomRadius         :: Int
         , doubleConnectorChance :: Float
         }
  deriving Show

defaultConfig :: Config
defaultConfig =
  Config { radius                = 32
         , roomAttempts          = 1000
         , minRoomRadius         = 2
         , maxRoomRadius         = 4
         , doubleConnectorChance = 0.25
         }

invalidConfig :: Config -> Maybe String
invalidConfig c
  | not roomAttemptsCheck          = Just "Room attempts are no good (see if you have a sane value)."
  | not largerThanMinimumSize      = Just "Bounds too small."
  | not roomRadiusCheck            = Just "Room radii are no good."
  | not doubleConnectorChanceCheck = Just "0.0 <= doubleConnectorChance <= 1.0"
  | otherwise                      = Nothing
  where roomAttemptsCheck = roomAttempts c > 20 &&
                            roomAttempts c < 5000

        largerThanMinimumSize = radius c > 5

        roomRadiusCheck = minRoomRadius c <= maxRoomRadius c &&
                          minRoomRadius c >= 1               &&
                          minRoomRadius c <= (radius c - 2)  &&
                          maxRoomRadius c <= (radius c - 2)

        doubleConnectorChanceCheck = doubleConnectorChance c >= 0.0 &&
                                     doubleConnectorChance c <= 1.0
