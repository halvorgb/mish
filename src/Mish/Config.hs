module Mish.Config where

import           Mish.HexagonalGrid

data Config = Config { radius              :: Radius
                     , roomAttempts        :: Int
                     , minRoomRadius       :: Int
                     , maxRoomRadius       :: Int
                     }
              deriving Show

defaultConfig :: Config
defaultConfig =
    Config { radius              = 8
           , roomAttempts        = 1000
           , minRoomRadius       = 2
           , maxRoomRadius       = 4
           }

invalidConfig :: Config -> Maybe String
invalidConfig c
  | not roomAttemptsCheck     = Just "Room attempts are no good (see if you have a sane value)."
  | not largerThanMinimumSize = Just "Bounds too small."
  | not roomRadiusCheck       = Just "Room radii are no good."
  | otherwise                 = Nothing
  where roomAttemptsCheck = roomAttempts c > 20 &&
                            roomAttempts c < 5000

        largerThanMinimumSize = radius c > 5

        roomRadiusCheck   = minRoomRadius c <= maxRoomRadius c &&
                            minRoomRadius c >= 1               &&
                            minRoomRadius c <= (radius c - 2)    &&
                            maxRoomRadius c <= (radius c - 2)
