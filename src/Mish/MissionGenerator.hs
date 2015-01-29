module Mish.MissionGenerator(generateMission) where

import qualified Data.Map as M
import           Mish.Config
import           Mish.ConnectorGenerator
import           Mish.HexagonalGrid
import           Mish.MazeGenerator
import           Mish.RoomGenerator
import           System.Random

-- Generates a mission based on a seed and a config
generateMission ::Config -> StdGen -> HexagonalMission
generateMission config seed =
  case invalidConfig config of
    Just err -> error err
    Nothing -> let m = M.fromList $ zip (withinRange (0,0) $ radius config) $ repeat Wall
                   (m', seed')   = generateRandomRooms m seed config $ roomAttempts config
                   (m'', seed'') = generateMazes m' seed' config
                   (m''', seed''') = connectRooms m'' seed'' config
               in newMission m''' $ radius config
