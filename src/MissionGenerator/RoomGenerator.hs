module MissionGenerator.RoomGenerator(generateRandomRooms) where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.List               as L
--import qualified Control.Monad.Trans.State as S
import qualified Data.Array.MArray       as MA
import qualified Data.Array.ST           as MA
-- import           Debug.Trace
import           MissionGenerator.Config
import           MissionGenerator.Types
import           MissionGenerator.Util
import           System.Random

data Room = SquareRoom
          | RectRoomWide
          | RectRoomHigh
          | RoundRoom
          | CircleRoom
          | LRoom
          | HRoom
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
generateRoom :: MA.STArray s Position Cell -> Room -> Position -> Int -> ST s ()
generateRoom arr room center radius
  | otherwise =
  do cells <- mapM (\c -> MA.readArray arr c) allPoses
     when ((not $ null allPoses) &&
           (all isWall cells)) $
       mapM_(\pos -> MA.writeArray arr pos $ Cell Floor Nothing Nothing) openPositions
     return ()

  where (wallPositions, openPositions) = coordinatesFromRoom room center radius
        allPoses :: [Position]
        allPoses = wallPositions ++ openPositions

-- Given a center and a roomtype, generate all the coordinates that fall within the room.
coordinatesFromRoom :: Room -> Position -> Int -> ([Position], [Position])
coordinatesFromRoom SquareRoom c r = (wps, ops)
  where ops = squareByRadius (r-1) c
        wps = (squareByRadius r    c) L.\\ ops

coordinatesFromRoom RectRoomWide c r = (wps, ops)
  where h_r      = r `div` 2
        fullRect = rectByWHRadius (r, h_r) c

        r'       = r-1
        h_r'     = h_r-1
        ops      = rectByWHRadius (r', h_r') c

        wps      = fullRect L.\\ ops

coordinatesFromRoom RectRoomHigh c r = (wps, ops)
  where w_r      = r `div` 2
        fullRect = rectByWHRadius (w_r, r) c

        r'       = r-1
        w_r'     = w_r-1
        ops      = rectByWHRadius (w_r', r') c

        wps      = fullRect L.\\ ops




coordinatesFromRoom RoundRoom center radius = (wps, ops)
  where fullCircle = circleByRadius radius center
        ops        = circleByRadius (radius - 1) center
        wps        = fullCircle L.\\ ops

coordinatesFromRoom CircleRoom center radius = (wps, ops)
  where fullCircle = circleByRadius radius center
        kernel     = circleByRadius 1 center
        ops        = (circleByRadius (radius - 1) center) L.\\ kernel
        wps        = fullCircle L.\\ ops

coordinatesFromRoom LRoom c@(cCol, cRow) r = (wps, ops)
  where half_r = r `div` 2
        negativeUpperLeft = [(x,y) | x <- [cCol..cCol+r]
                                   , y <- [cRow-r..cRow+half_r]]
        ops = (squareByRadius (r-1) c) L.\\ negativeUpperLeft
        wps = (squareByRadius r     c) L.\\ ops

coordinatesFromRoom HRoom c@(cCol, cRow) r = (wps, ops)
  where gap_w = r `div` 5
        gap_h = r `div` 2

        negativeUpper = [(x,y) | x <- [cCol-gap_w..cCol+gap_w]
                               , y <- [cRow-r..cRow-gap_h]]
        negativeLower = [(x,y) | x <- [cCol-gap_w..cCol+gap_w]
                               , y <- [cRow+gap_h..cRow+r]]
        negative = negativeUpper++negativeLower

        ops = (squareByRadius (r-1) c) L.\\ negative
        wps = (squareByRadius r     c) L.\\ ops


-- (x-a)^2 + (y-b)^2 <= r^2
circleByRadius :: Int -> Position -> [Position]
circleByRadius r (a, b) = [(x,y) | x <- [a-r..a+r]
                                 , y <- [b-r..b+r]
                                 , (x-a)*(x-a) + (y-b)*(y-b) <= r*r]


squareByRadius :: Int -> Position -> [Position]
squareByRadius r p = rectByWHRadius (r,r) p

rectByWHRadius :: Dimensions -> Position -> [Position]
rectByWHRadius (rw, rh) (a,b) = [(x,y) | x <- [a-rw..a+rw]
                                       , y <- [b-rh..b+rh]]


generateRandomRooms :: MA.STArray s Position Cell -> StdGen -> Config -> Int -> ST s StdGen
generateRandomRooms _   seed _      0        = return seed
generateRandomRooms arr seed config attempts =
  do when (distToEdge > radius) $ generateRoom arr room (column, row) radius
     generateRandomRooms arr s''' config attempts'
  where (columns, rows) = dimensions config
        -- randomize center.
        (column, s) = randomR (0, columns-1) seed
        (row, s')   = randomR (0, rows-1) s
        (radius, s'') = randomR (minRoomRadius config, min distToEdge $ maxRoomRadius config) s'
        (room, s''') = random s''
        distToEdge  = minimum $ [ column           -- distance to left edge
                                , row              -- distance to top edge
                                , columns-column-1 -- distance to right edge
                                , rows-row-1       -- distance to bottom edge
                                ]


        attempts' = attempts-1

        -- randomize shape.
        -- find the max bounds the shape can take
        -- randomize bounds

        -- Find a way to not pass s''''' in the above...
