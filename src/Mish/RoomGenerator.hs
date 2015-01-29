module Mish.RoomGenerator(generateRandomRooms) where



import qualified Data.List                      as L
import qualified Data.Map                       as M
import           Mish.Config
import           Mish.HexagonalGrid
import           System.Random

data Room = HexRoom
          deriving (Show, Bounded, Enum, Eq)

instance Random Room where
    random g = case randomR (fromEnum (minBound :: Room), fromEnum (maxBound :: Room)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

data VoidRoom = VoidRoom Room
                deriving (Show, Eq)

-- attempt to generate a room of a specific type at a center
-- generate the largest area possible.
generateRoom :: InternalMap -> Room -> AxialCoordinate -> Radius -> InternalMap
generateRoom m room c r = m'
  where (wallPositions, openPositions) = coordinatesFromRoom room c r
        allPoses :: [AxialCoordinate]
        allPoses = wallPositions ++ openPositions

        cells = map (m M.!) allPoses

        m' = if not (null allPoses) &&
                all (==Wall) cells
             then L.foldl' (\tm op -> M.insert op Floor tm) m openPositions
             else m

-- Given a center and a roomtype, generate all the coordinates that fall within the room.
coordinatesFromRoom :: Room -> AxialCoordinate -> Radius -> ([AxialCoordinate], [AxialCoordinate])
coordinatesFromRoom HexRoom c r = (ring c r, withinRange c $ r-1)


generateRandomRooms :: InternalMap -> StdGen -> Config -> Int -> (InternalMap, StdGen)
generateRandomRooms m seed _      0        = (m, seed)
generateRandomRooms m seed config attempts = generateRandomRooms m' s''' config attempts'
  where m' = if minRR <= maxRR
             then generateRoom m room centre rr
             else m

        attempts' = attempts - 1

        -- randomize centre:
        centre :: AxialCoordinate
        centre = (row, col)
        (col, s)  = randomR (-r, r) seed
        (row, s') = randomR (max (-r) (-col-r), min r (-col+r)) s

        minRR = minRoomRadius config
        maxRR = min minDistToEdge $ maxRoomRadius config
        (rr, s'') = randomR (minRR, maxRR) s'
        -- randomize room-type.
        room :: Room
        (room, s''') = random s''

        cube = axialToCube centre

        r = radius config


        minDistToEdge = minimum $ distToEdge cube r
        -- randomize shape.
        -- find the max bounds the shape can take
        -- randomize bounds

        -- Find a way to not pass s''''' in the above...


distToEdge :: CubeCoordinate -> Radius -> [Int]
distToEdge  (x, y, z) r = [ r - x -- distance to right edge
                          , r + x -- distance to left edge

                          , r - y -- distance to bottom left edge
                          , r + y -- distance to top right edge

                          , r - z -- distance to top left edge
                          , r + z -- distance to bottom right edge
                          ]
