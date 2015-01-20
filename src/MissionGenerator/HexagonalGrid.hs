module MissionGenerator.HexagonalGrid(AxialCoordinate, (|+|), Dimensions, Tile, HexagonalMission, newMission, hexBounds, neighbours) where

-- An implementation of http://www.redblobgames.com/grids/hexagons/
-- Using axial coordinates

import qualified Data.Array as A
import Debug.Trace

-- (q,r)
type AxialCoordinate = (Int, Int)

-- (x,z,y)
type CubeCoordinate  = (Int, Int, Int)
type Dimensions = (AxialCoordinate, AxialCoordinate)

data Tile = Floor
          | Wall
          | Door
          | Water
            deriving Eq

instance Show Tile where
  show Floor = " "
  show Wall  = "#"
  show Door  = "D"
  show Water = "~"



newtype HexagonalMission =
  HexagonalMission { internalMission :: InternalMission }

data InternalMission =
  InternalMission { internalArray :: A.Array AxialCoordinate Tile
                  , internalDimensions :: Dimensions
                  }




hexBounds :: HexagonalMission -> Dimensions
hexBounds = internalDimensions . internalMission

newMission :: A.Array AxialCoordinate Tile -> Dimensions -> HexagonalMission
newMission arr ds = HexagonalMission $
                    InternalMission { internalArray = arr
                                    , internalDimensions = ds
                                    }


(|+|) :: AxialCoordinate -> AxialCoordinate -> AxialCoordinate
(q,r) |+| (a,b) = (q+a, r+b)

(|++|) :: CubeCoordinate -> CubeCoordinate -> CubeCoordinate
(x,z,y) |++| (a,b,c) = (x+a, z+b, y+c)


-- list all immediate neighbour coordinates.
neighbours :: AxialCoordinate -> [AxialCoordinate]
neighbours c = map (|+| c) matrix
  where matrix = [ ( 1,  0)
                 , ( 1, -1)
                 , ( 0, -1)
                 , (-1,  0)
                 , (-1,  1)
                 , ( 0,  1)
                 ]

diagonalNeighbours :: AxialCoordinate -> [AxialCoordinate]
diagonalNeighbours c = map (cubeToAxial . (|++| cubeCoords)) matrix
  where cubeCoords = axialToCube c
        matrix     = [ ( 2, -1, -1)
                     , ( 1,  1, -2)
                     , (-1,  2, -1)
                     , (-2,  1,  1)
                     , (-1, -1,  2)
                     , ( 1, -2,  1)
                     ]

-- Distance between two points.
distance :: AxialCoordinate -> AxialCoordinate -> Int
distance (q1, r1) (q2,r2) = (abs(q1 - q2) + abs(r1 - r2) + abs(q1 + r1 - q2 - r2))
                            `div`
                            2

-- TODO
line :: AxialCoordinate -> AxialCoordinate -> [AxialCoordinate]
line a b = []
  where n = distance a b


-- All coordinates within n distance of the point.
inRange :: AxialCoordinate -> Int -> [AxialCoordinate]
inRange (q,r) n = [(x+q, z+r) | x <- [(-n)..n]
                              , z <- [max (-n) (-x-n)..min n (-x+n)]
                              ]
-- The interesection of two ranges
inIntersectionRange :: AxialCoordinate -> Int -> AxialCoordinate -> Int -> [AxialCoordinate]
inIntersectionRange (q1, r1) n1 (q2, r2) n2

  | otherwise = [(q,r) | q <- [q_min..q_max]
                       , r <- [r_min q..r_max q]
                       ]
  where q1_min = q1 - n1
        q2_min = q2 - n2
        q_min  = max q1_min q2_min

        q1_max = q1 + n1
        q2_max = q2 + n2
        q_max = min q1_max q2_max

        r1_min q = max (r1-n1) (r1-q-n1)
        r2_min q = max (r2-n2) (r2-q-n2)
        r_min  q = max (r1_min q) (r2_min q)

        r1_max q = min (r1+n1) (r1-q+n1)
        r2_max q = min (r2+n2) (r2-q+n2)
        r_max  q = min (r1_max q) (r2_max q)


axialToCube :: AxialCoordinate -> CubeCoordinate
axialToCube (q,r) = (x,z,y)
  where x = q
        z = r
        y = -x-z

cubeToAxial :: CubeCoordinate -> AxialCoordinate
cubeToAxial (x,z,_) = (q,r)
  where q = x
        r = z
