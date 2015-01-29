module Mish.MazeGenerator(generateMazes) where


import qualified Data.List                      as L
import qualified Data.Map                       as M
import           Mish.Config
import           Mish.HexagonalGrid
import           Mish.Util
import           System.Random


-- Generate mazes wherever there's room in an InternalMap
generateMazes :: InternalMap -> StdGen -> Config -> (InternalMap, StdGen)
generateMazes m seed _ = L.foldl' subMaze (m, seed) $ M.keys m

-- try to create a sub maze starting at AxialCoordinate p.
subMaze :: (InternalMap, StdGen) -> AxialCoordinate -> (InternalMap, StdGen)
subMaze ms@(m, _) p
  | isOpenToStart m p = buildMaze ms p
  | otherwise = ms


-- We have a (legal) starting position, build a maze from this position until we cannot build any more.
buildMaze :: (InternalMap, StdGen) -> AxialCoordinate -> (InternalMap, StdGen)
buildMaze (m, seed) p
  | null moves = (m',  seed)
  | otherwise  = buildMaze (m', seed') p'

  where m'          = M.insert p Floor m
        moves       = legalMoves m p
        (p', seed') = choice moves seed

-- Check if position p is OK to start
isOpenToStart :: InternalMap -> AxialCoordinate -> Bool
isOpenToStart m p = allWall m $ p:neighbours p


allWall :: InternalMap -> [AxialCoordinate] -> Bool
allWall m = all (\p -> maybe False (==Wall) $ M.lookup p m)

-- Find all (0-6) legal moves for a maze to take from a position p
legalMoves :: InternalMap -> AxialCoordinate -> [AxialCoordinate]
legalMoves m p = filter legal ns
  where ns = neighbours p
        legal :: AxialCoordinate -> Bool
        legal n = allWall m nns
          where nns = L.delete p $ n:neighbours n
