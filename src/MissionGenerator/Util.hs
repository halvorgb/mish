module MissionGenerator.Util where

import MissionGenerator.Types
import System.Random

isWall :: Cell -> Bool
isWall (Cell Wall _ _)   = True
isWall _                 = False

isFloor :: Cell -> Bool
isFloor (Cell Floor _ _) = True
isFloor _                = False

(|+|) :: Position -> Position -> Position
(ax, ay) |+| (bx, by) = (ax+bx, ay+by)
(|-|) :: Position -> Position -> Position
(ax, ay) |-| (bx, by) = (ax-bx, ay-by)
(|+) :: Position -> Int -> Position
(ax, ay) |+ b = (ax+b, ay+b)

-- n
--npn
-- n
fiveWay :: Position -> [Position]
fiveWay p = p:neighbours p

-- n
--n n
-- n
neighbours :: Position -> [Position]
neighbours p = [ p |+| (0,1)
               , p |-| (0,1)
               , p |+| (1,0)
               , p |-| (1,0) ]


inBounds :: (Position, Position) -> Position -> Bool
inBounds ((minx, miny), (maxx, maxy)) (x,y) =
  x >= minx && x <= maxx &&
  y >= miny && y <= maxy

choice :: [a] -> StdGen -> (a, StdGen)
choice l g = (l !! index, g')
  where (index, g') = randomR (0, (length l) - 1) g
