module Main where

import           Mish.Config
import           Mish.HexagonalGrid
import           Mish.MissionGenerator
import           System.Random

main :: IO ()
main =
  do seed <- newStdGen
     prettyPrint $ generateMission defaultConfig seed
