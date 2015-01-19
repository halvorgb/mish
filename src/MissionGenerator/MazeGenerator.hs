module MissionGenerator.MazeGenerator(generateMaze) where


import           Control.Monad
import           Control.Monad.ST

import qualified Data.Array.MArray       as MA
import qualified Data.Array.ST           as MA
import qualified Data.List               as L
-- import           Data.Maybe as Mb
import           MissionGenerator.Config
import           MissionGenerator.Types
import           MissionGenerator.Util
import           System.Random



generateMaze :: MA.STArray s Position Cell -> StdGen -> Config -> ST s StdGen
generateMaze arr seed _ =
  do is <- indicesFromArr arr
     foldM (subMaze arr) seed is


indicesFromArr :: MA.STArray s Position Cell -> ST s [Position]
indicesFromArr arr =
  do ((minx, miny), (maxx, maxy)) <- MA.getBounds arr
     return $ [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]


-- try to create a sub maze starting at position p.
subMaze :: MA.STArray s Position Cell -> StdGen -> Position -> ST s StdGen
subMaze arr seed p =
  do o <- isOpenToStart arr p
     if o
       then buildMaze arr seed p
       else return seed


-- We have a (legal) starting position, build a maze from this position until we cannot build any more.
buildMaze :: MA.STArray s Position Cell -> StdGen -> Position -> ST s StdGen
buildMaze arr seed p =
  do MA.writeArray arr p $ Cell Floor Nothing Nothing
     moves <- legalMoves arr p

     if null moves
       then return seed
       else do let (chosenMove, seed') = choice moves seed
               buildMaze arr seed' chosenMove

-- Check if position p is OK to start (not open, all edges are not opened)
isOpenToStart :: MA.STArray s Position Cell -> Position -> ST s Bool
isOpenToStart arr p = allWall arr $ fiveWay p

-- Find all (0-4) legal moves for a maze to take from a position p
legalMoves :: MA.STArray s Position Cell -> Position -> ST s [Position]
legalMoves arr p =
  do legals <- filterM (allWall arr) possibleMoves
     let centers :: [Position]
         centers = map head legals -- fiveWay always puts the center at head.
     return centers

  where ns = neighbours p
        possibleMoves :: [[Position]]
        possibleMoves = map (L.delete p . fiveWay) ns

-- checks whether all tiles are wall.
allWall :: MA.STArray s Position Cell -> [Position] -> ST s Bool
allWall arr ps =
  do b <- MA.getBounds arr
     if (all (inBounds b) ps)
       then do cells <- mapM (\p -> MA.readArray arr p) ps
               return $ all isWall cells
       else return False
