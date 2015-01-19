module MissionGenerator.ConnectorGenerator(connectRooms) where

import           Control.Monad
import           Control.Monad.ST
import           Debug.Trace
-- import qualified Data.List               as L
import qualified Data.Set                as S

--import qualified Control.Monad.Trans.State as S
import qualified Data.Array.ST           as MA
-- import           Debug.Trace
import           MissionGenerator.Config
import           MissionGenerator.Types
import           MissionGenerator.Util
import           System.Random

type Region = S.Set Position

-- Having already made random rooms and filled the array with mazes, connect rooms through use of said mazes.
connectRooms :: MA.STArray s Position Cell -> StdGen -> Config -> ST s StdGen
connectRooms arr seed config =
  do b <- MA.getBounds arr
     assocs <- MA.getAssocs arr
     connectorIndices <- connectorCandidates arr assocs b

     let floor_tiles :: S.Set Position
         floor_tiles = S.fromList $ map fst $ filter (\(_, c) -> isFloor c) assocs
         regions = createRegions floor_tiles

     -- yolo
     --mapM_ (\p -> MA.writeArray arr p $ Cell Door Nothing Nothing) connectorIndices
     --mapM_ (\p -> MA.writeArray arr p $ Cell Water Nothing Nothing) $ S.toList $ head regions
     case regions of
       (h:t) -> connectRegions arr seed config h (S.fromList t) (S.fromList connectorIndices)
       _     -> return seed
     --return seed

-- all candidates for connectors, that is any wall tile which has 2 or more neighbouring floor tiles
connectorCandidates :: MA.STArray s Position Cell -> [(Position, Cell)] -> (Position, Position) -> ST s [Position]
connectorCandidates arr assocs b =
  do candidates <- filterM (connectorCandidate arr) assocs
     return $ map fst candidates

  where connectorCandidate :: MA.STArray s Position Cell -> (Position, Cell) -> ST s Bool
        connectorCandidate arr' (p, (Cell Wall _ _)) =
          do [up, down, right, left] <- mapM (\n ->
                                              case n of
                                                Nothing -> return Nothing
                                                Just v  -> do r <- MA.readArray arr' v
                                                              return $ Just r
                                             ) ns
             return $
               check up down ||
               check right left


          where ns = map (\n -> if inBounds b n
                                then Just n
                                else Nothing) $ neighbours p


                check :: Maybe Cell -> Maybe Cell -> Bool
                check Nothing _ = False
                check _ Nothing = False
                check (Just x) (Just y) = x == y &&
                                          isFloor x

        connectorCandidate _ _ = return False

-- take an element from the set, add all of its neighbours into a Region.
createRegions :: S.Set Position -> [Region]
createRegions s
  | S.null s  = []
  | otherwise = region:createRegions s'
  where any_pos  = head $ S.toList s
        any_five = S.fromList $ fiveWay any_pos
        region   = createRegion any_five S.empty

        -- delete every element from region found from the set of floor tiles
        s'       = s S.\\ region

        createRegion :: S.Set Position -> Region -> Region
        createRegion frontier visited
          | S.null frontier = visited
          | otherwise       = createRegion frontier' visited'
          where in_region = S.filter (\n -> S.notMember n visited &&
                                            S.member n s) frontier
                visited'  = S.union visited in_region
                frontier' = S.fromList $ concatMap fiveWay $ S.toList in_region



-- connect regions until only 1 remains
connectRegions :: MA.STArray s Position Cell -> StdGen -> Config -> Region -> S.Set Region -> S.Set Position -> ST s StdGen
connectRegions arr seed config region regions connectors
  | trace ("len region: " ++ show (S.size region)) False = undefined
  | trace ("connector: " ++ show connector) False = undefined
  | trace ("len regions: " ++ show (S.size regions)) False = undefined
  | trace ("len regions_connected: " ++ show (S.size regions_connected)) False = undefined
  | S.null regions ||
    S.size regions == 1 = return seed
  | S.null connectors   = error "Something has went terribly wrong, impossible to connect regions."
  | otherwise =
    do MA.writeArray arr connector $ Cell Door Nothing Nothing
       connectRegions arr seed'' config region' regions' connectors'

  where region_connectors  = S.toList $ S.filter (bordersRegion region) connectors
        (connector, seed') = choice region_connectors seed


        regions_connected  = S.filter (\r -> bordersRegion r connector) regions
        (chosen, seed'')   = choice (S.toList regions_connected) seed'
        region'            = S.union region chosen
        regions'           = S.delete chosen regions

        connectors'        = S.filter (\c -> not (bordersRegion region c &&
                                                  bordersRegion chosen c)) connectors

        -- (dc, seed''')      = randomR (0, 1) seed''
        -- doubleConnect      = dc <= doubleConnectChance config

        bordersRegion :: Region -> Position -> Bool
        bordersRegion r c = any (\n -> S.member n r) $ neighbours c
