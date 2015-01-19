module MissionGenerator.Config where

import           MissionGenerator.Types

data Config = Config { dimensions          :: Dimensions
                     , roomAttempts        :: Int
                     , doubleConnectChance :: Float
                     , includeSquareRooms  :: Bool
                     , includeRectRooms    :: Bool
                     , includeRoundRooms   :: Bool
                     , includeCircleRooms  :: Bool
                     , includeLRooms       :: Bool
                     , includeHRooms       :: Bool
                     , includeVoidRooms    :: Bool
                     , minRoomRadius       :: Int
                     , maxRoomRadius       :: Int
                     , placeEnemies        :: Bool
                     , placeLights         :: Bool
                     }
              deriving Show

defaultConfig :: Config
defaultConfig =
    Config { dimensions          = (64,64)
           , roomAttempts        = 1000
           , doubleConnectChance = 0.1
           , includeSquareRooms  = True
           , includeRectRooms    = True
           , includeRoundRooms   = True
           , includeCircleRooms  = True
           , includeLRooms       = True
           , includeHRooms       = True
           , includeVoidRooms    = True
           , minRoomRadius       = 2
           , maxRoomRadius       = 4
           , placeEnemies        = False
           , placeLights         = False
           }


invalidConfig :: Config -> Maybe String
invalidConfig c
  | not roomAttemptsCheck     = Just "Room attempts are no good (see if you have a sane value)."
  | not largerThanMinimumSize = Just "Bounds too small."
  | not includesSomeRoomType  = Just "Does not include any room type (void doesnt count)."
  | not roomRadiusCheck       = Just "Room radii are no good."
  | not doubleConnectCheck    = Just "Double connect chance needs to be <= 1 and >= 0."
  | otherwise                 = Nothing
  where roomAttemptsCheck = roomAttempts c > 20 &&
                            roomAttempts c < 5000

        (rows, columns)       = dimensions c
        largerThanMinimumSize = rows > 4 &&
                                columns > 4


        includesSomeRoomType = includeSquareRooms c ||
                               includeRectRooms c   ||
                               includeRoundRooms c  ||
                               includeCircleRooms c ||
                               includeLRooms c      ||
                               includeHRooms c

        smallestDimension = min rows columns
        roomRadiusCheck   = minRoomRadius c <= maxRoomRadius c         &&
                            minRoomRadius c >= 2                       &&
                            minRoomRadius c <= (smallestDimension - 2) &&
                            maxRoomRadius c <= (smallestDimension - 2)

        doubleConnect      = doubleConnectChance c
        doubleConnectCheck = doubleConnect >= 0.0 &&
                             doubleConnect <= 1.0
