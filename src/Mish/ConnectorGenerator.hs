module Mish.ConnectorGenerator(connectRooms) where

import qualified Data.Set                as S
import qualified Data.Map           as M
import           Mish.Config
import           Mish.Util
import           Mish.HexagonalGrid
import           System.Random
type Region = S.Set AxialCoordinate

-- Having already made random rooms and filled the array with mazes, connect rooms through use of said mazes.
connectRooms :: InternalMap -> StdGen -> Config -> (InternalMap, StdGen)
connectRooms m seed config =
  case regions of
    []    -> (m, seed)
    (h:t) -> connectRegions m seed config h (S.fromList t) $ S.fromList connectorIndices
  where ascs = M.assocs m
        connectorIndices = connectorCandidates m ascs
        floorTiles = S.fromList $ map fst $ filter ((Floor==) . snd) ascs
        regions = createRegions floorTiles

-- all candidates for connectors, that is any wall tile which has 2 or more neighbouring floor tiles
connectorCandidates :: InternalMap -> [(AxialCoordinate, Tile)] -> [AxialCoordinate]
connectorCandidates m ascs = map fst candidates
  where candidates = filter connectorCandidate ascs

        connectorCandidate :: (AxialCoordinate, Tile) -> Bool
        connectorCandidate (p, t) =
          (t == Wall) &&
          (2 <= length (filter (\n -> maybe False (==Floor) $ M.lookup n m) $ neighbours p))

-- take an element from the set, add all of its neighbours into a Region.
createRegions :: S.Set AxialCoordinate -> [Region]
createRegions s
  | S.null s  = []
  | otherwise = region:createRegions s'
  where any_pos  = head $ S.toList s
        any_ns = S.fromList $ any_pos:neighbours any_pos
        region   = createRegion any_ns S.empty

        -- delete every element from region found from the set of floor tiles
        s'       = s S.\\ region

        createRegion :: S.Set AxialCoordinate -> Region -> Region
        createRegion frontier visited
          | S.null frontier = visited
          | otherwise       = createRegion frontier' visited'
          where in_region = S.filter (\n -> S.notMember n visited &&
                                            S.member n s) frontier
                visited'  = S.union visited in_region
                frontier' = S.fromList $ concatMap (\p -> p:neighbours p) $ S.toList in_region



-- connect regions until only 1 remains
connectRegions :: InternalMap -> StdGen -> Config -> Region -> S.Set Region -> S.Set AxialCoordinate -> (InternalMap, StdGen)
connectRegions m seed config region regions connectors
  | S.null regions ||
    S.size regions == 1 ||
    S.null region_connectors = (m, seed)
  | S.null connectors        = error "Something has went terribly wrong, impossible to connect regions."
  | otherwise                = connectRegions m' seed'' config region' regions' connectors'

  where region_connectors  = S.filter (bordersRegion region) connectors
        (connector, seed') = choice (S.toList region_connectors) seed


        regions_connected  = S.filter (`bordersRegion` connector) regions
        (chosen, seed'')   = if S.null regions_connected
                             then (region, seed')
                             else choice (S.toList regions_connected) seed'

        region'            = S.union region chosen
        regions'           = S.delete chosen regions

        connectors'        = if S.null regions_connected
                             then S.delete connector connectors
                             else S.filter (\c -> not (bordersRegion region c &&
                                                       bordersRegion chosen c)) connectors


        m' = if not $ S.null regions_connected
             then M.insert connector Door m
             else m

        bordersRegion :: Region -> AxialCoordinate -> Bool
        bordersRegion r c = any (`S.member` r) $ neighbours c
