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
    (h:t) -> connectRegions m seed config h (S.fromList t) (S.fromList connectorIndices) False
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
          any (\(n1, n2) -> S.member n1 floorNeighbours &&
                             S.member n2 floorNeighbours
              ) (neighbourpairs p)
          where floorNeighbours = S.fromList $ filter (\n -> maybe False (==Floor) $ M.lookup n m) $ neighbours p

        -- the acceptable pairs of neighbours for a connectorCandidate
        neighbourpairs :: AxialCoordinate -> [(AxialCoordinate, AxialCoordinate)]
        neighbourpairs c = map (\(n1, n2) -> (n1 |+| c, n2 |+| c)) matrix
          where matrix = [ ((-1, 0), (0,  1)) -- TL, B
                         , ((-1, 0), (1,  0)) -- TL, BR
                         , ((-1, 0), (1, -1)) -- TL, TR
                         , ((-1, 1), (1,  0)) -- BL, BR
                         , ((-1, 1), (1, -1)) -- BL, TR
                         , ((-1, 1), (0, -1)) -- BL, T
                         , (( 0, 1), (1, -1)) -- B , TR
                         , (( 0, 1), (0, -1)) -- B , T
                         , (( 1, 0), (0, -1)) -- BR, T
                         ]

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
connectRegions :: InternalMap -> StdGen -> Config -> Region -> S.Set Region -> S.Set AxialCoordinate -> Bool -> (InternalMap, StdGen)
connectRegions m seed config region regions connectors doubled
  | S.null regions ||
    S.size regions == 1 ||
    S.null region_connectors = (m, seed)
  | S.null connectors        = error "Something has went terribly wrong, impossible to connect regions."
  | otherwise                = connectRegions m' seed''' config region' regions' connectors' doubleConnect

  where region_connectors  = S.filter (bordersRegion region) connectors
        (connector, seed') = choice (S.toList region_connectors) seed

        dcChance           = doubleConnectorChance config
        (dc, seed'')       = randomR (0.0, 1.0) seed'
        doubleConnect      = not doubled &&
                             S.size region_connectors > 1 &&
                             dc < dcChance

        regions_connected  = S.filter (`bordersRegion` connector) regions
        (chosen, seed''')  = if S.null regions_connected
                             then (region, seed'')
                             else choice (S.toList regions_connected) seed''

        region'            = S.union region chosen

        regions'           = if doubleConnect
                             then regions
                             else S.delete chosen regions

        connectors'        = if S.null regions_connected
                             then S.delete connector connectors
                             else S.filter (\c -> not (bordersRegion region c &&
                                                       bordersRegion chosen c)) connectors



        m' = if not $ S.null regions_connected
             then M.insert connector Floor m
             else m

        bordersRegion :: Region -> AxialCoordinate -> Bool
        bordersRegion r c = any (`S.member` r) $ neighbours c
