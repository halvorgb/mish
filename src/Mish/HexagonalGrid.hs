module Mish.HexagonalGrid( AxialCoordinate
                                     , CubeCoordinate
                                     , (|+|)
                                     , Radius
                                     , Tile(..)
                                     , HexagonalMission
                                     , InternalMap
                                     , newMission
                                     , inBounds
                                     , withinRange
                                     , ring
                                     , neighbours
                                     , prettyPrint
                                     , axialToCube
                                     , cubeToAxial
                                     ) where

-- An implementation algorithms from http://www.redblobgames.com/grids/hexagons/
-- + general utility functions to handle hex grid missions
-- Using axial coordinates

import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Set      as S
import           System.Random

-- (q,r)
type AxialCoordinate = (Int, Int)
-- (x,z,y)
type CubeCoordinate  = (Int, Int, Int)
type Radius          = Int
type InternalMap     = M.Map AxialCoordinate Tile

data Tile = Floor
          | Wall
          | Door
          | Water
          deriving (Bounded, Enum, Eq)

instance Random Tile where
    random g = case randomR (fromEnum (minBound :: Tile), fromEnum (maxBound :: Tile)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

instance Show Tile where
  show Floor = " "
  show Wall  = "#"
  show Door  = "D"
  show Water = "~"



-- assumptions: (0,0) = centre
newtype HexagonalMission =
  HexagonalMission { internalMission :: InternalMission }

data InternalMission =
  InternalMission { internalMap    :: InternalMap
                  , internalRadius :: Radius
                  }

-- The only way to create a HexagonalMission outside this module is through this function.
-- controls that the radius is correct (and that the indices form a perfect hexagon)
newMission :: M.Map AxialCoordinate Tile -> Radius -> HexagonalMission
newMission m r
  | keySet /= keySetCorrect = error "The map isn't a perfect hexagon."
  | otherwise =
    HexagonalMission
    InternalMission { internalMap = m
                    , internalRadius = r
                    }
  where keySet = S.fromList $ M.keys m
        keySetCorrect = S.fromList $ withinRange (0,0) r

getInternalMap :: HexagonalMission -> InternalMap
getInternalMap = iMap . iMiss


(|+|) :: AxialCoordinate -> AxialCoordinate -> AxialCoordinate
(q1,r1) |+| (q2, r2) = (q1+q2, r1+r2)

(|++|) :: CubeCoordinate -> CubeCoordinate -> CubeCoordinate
(x1,y1,z1) |++| (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

-- Control that the given coordinates are within the bounds of the mission.
inBounds :: HexagonalMission -> AxialCoordinate -> Bool
inBounds hm (x,y) =
  x >= -r &&
  x <= r &&
  y >= max (-r) (-x-r) &&
  y <= min r (-x+r)
  where r = iRad $ iMiss hm

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

-- list all diagonal neighbour coordinates.
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

-- Distance between two coordinates.
distance :: AxialCoordinate -> AxialCoordinate -> Int
distance (q1, r1) (q2,r2) = (abs(q1 - q2) + abs(r1 - r2) + abs(q1 + r1 - q2 - r2))
                            `div`
                            2

-- TODO
line :: AxialCoordinate -> AxialCoordinate -> [AxialCoordinate]
line a b = []
  where n = distance a b


-- All coordinates within n distance of the point.
withinRange :: AxialCoordinate -> Int -> [AxialCoordinate]
withinRange (q,r) n = [(x+q, y+r) | x <- [(-n)..n]
                                  , y <- [max (-n) (-x-n)..min n (-x+n)]
                                  ]

-- All coordinates at radius r around a point.
ring :: AxialCoordinate -> Int -> [AxialCoordinate]
ring ax r = map cubeToAxial $ rotate bottomLeft (cycle corners) directions
  where (x,y,z) = axialToCube ax

        rotate :: CubeCoordinate -> [CubeCoordinate] -> [CubeCoordinate] -> [CubeCoordinate]
        rotate _ _         [] = []
        rotate p cs@(c:cr) ds@(d:dr)
          | n == c    = n:rotate n cr dr
          | otherwise = n:rotate n cs ds
          where n = p |++| d

        bottomLeft =   (x-r, y  , z+r) -- bl
        corners    = [ (x  , y-r, z+r) -- br
                     , (x+r, y-r, z  ) -- r
                     , (x+r, y  , z-r) -- tr
                     , (x  , y+r, z-r) -- tl
                     , (x-r, y+r, z  ) -- l
                     , bottomLeft
                     ]
        directions = [ ( 1, -1,  0) -- bl -> b
                     , ( 1,  0, -1) -- b  -> br
                     , ( 0,  1, -1) -- br -> tr
                     , (-1,  1,  0) -- tr -> t
                     , (-1,  0,  1) -- t -> tl
                     , ( 0, -1,  1) -- tl -> bl
                     ]

-- The interesection of two ranges with separate lengths.
withinIntersectionRange :: AxialCoordinate -> Int -> AxialCoordinate -> Int -> [AxialCoordinate]
withinIntersectionRange a1 n1 a2 n2 =
  [(x, y) | x <- [x_min..x_max]
          , y <- [max y_min (-x-z_max)..min y_max (-x-z_min)]
          ]
  where (x1, y1, z1) = axialToCube a1
        (x2, y2, z2) = axialToCube a2

        x_min = max (x1 - n1) (x2 - n2)
        x_max = min (x1 + n1) (x2 + n2)

        y_min = max (y1 - n1) (y2 - n2)
        y_max = min (y1 + n1) (y2 + n2)

        z_min = max (z1 - n1) (z2 - n2)
        z_max = min (z1 + n1) (z2 + n2)


-- prints a hexagonalMission
prettyPrint :: HexagonalMission -> IO ()
prettyPrint hm = foreachRow hm f
  where f :: (Int, [(AxialCoordinate, Tile)]) -> IO ()
        f (row, l) =
          putStrLn $ createSpace row ++ L.intersperse ' ' (concatMap (show . snd) l)

        createSpace :: Int -> String
        createSpace row = replicate (max 0 $ abs row) ' '

-- Helper function for prettyPrint
foreachRow :: HexagonalMission -> ((Int, [(AxialCoordinate, Tile)]) -> IO ()) -> IO ()
foreachRow hm f = mapM_ f row_list
  where sorted_by_rows = M.toAscList $ iMap $ iMiss hm

        row_list     = createRowList sorted_by_rows []

        createRowList :: [(AxialCoordinate, Tile)] -> [(Int, [(AxialCoordinate, Tile)])] -> [(Int, [(AxialCoordinate, Tile)])]
        createRowList [] m = reverse m
        createRowList l  m = createRowList l' m'
          where r        = ff $ head l
                (cs, l') = span ((r==) . ff) l
                m'       = (r,cs):m
                ff       = fst . fst


-- Generate a test mission.
testMission :: Radius -> HexagonalMission
testMission r = newMission m r
  where m :: InternalMap
        m = M.fromList $ map pseudoRandomTile $ withinRange (0,0) r

        pseudoRandomTile :: AxialCoordinate -> (AxialCoordinate, Tile)
        pseudoRandomTile c@(x,y) = (c, fst $ random $ mkStdGen (abs x + abs y * 100))

-- ---------------
-- Util functions:
-- ---------------
axialToCube :: AxialCoordinate -> CubeCoordinate
axialToCube (q,r) = (x,y,z)
  where x = q
        y = r
        z = -x-y

cubeToAxial :: CubeCoordinate -> AxialCoordinate
cubeToAxial (x,y,_) = (q,r)
  where q = x
        r = y


iMiss :: HexagonalMission -> InternalMission
iMiss = internalMission

iMap :: InternalMission -> InternalMap
iMap = internalMap

iRad :: InternalMission -> Radius
iRad = internalRadius
