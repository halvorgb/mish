module MissionGenerator.Util where

import System.Random

choice :: [a] -> StdGen -> (a, StdGen)
choice [] _ = error "invalid list for choice"
choice l  g = (l !! index, g')
  where (index, g') = randomR (0, (length l) - 1) g
