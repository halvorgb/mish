module MissionGenerator.MissionGenerator(generateMission, prettyMission) where

import qualified Data.Array                     as A
import qualified Data.Array.MArray              as MA
import qualified Data.Array.ST                  as MA
import           MissionGenerator.Config
import           MissionGenerator.MazeGenerator
import           MissionGenerator.RoomGenerator
import           MissionGenerator.ConnectorGenerator
import           MissionGenerator.Types
import           System.Random

-- Generates a rows*columns mission based on a seed.
-- The mission array is 0-indexed
generateMission ::Config -> StdGen -> Mission
generateMission config seed =
  case invalidConfig config of
    Just err -> error err
    Nothing ->
      MA.runSTArray $ -- In the following block, the array is mutable, returns an immutable array.
      do arr <- MA.newArray ((0,0), (columns-1, rows-1)) $ Cell Wall Nothing Nothing

         seed'   <- generateRandomRooms arr seed config $ roomAttempts config
         seed''  <- generateMaze arr seed' config
         seed''' <- connectRooms arr seed'' config
         return arr
  where (columns, rows) = dimensions config







prettyMission :: Mission -> String
prettyMission mission = unlines $ prettyMission' $ A.assocs mission
    where prettyMission' :: [(Position, Cell)] -> [String]
          prettyMission' [] = []
          prettyMission' m = concatMap (show . snd) rowCells:prettyMission' m'
              where row = snd $ fst $ head m
                    rowCells = filter ((==row) . snd . fst) m
                    m'       = filter ((/=row) . snd . fst) m
