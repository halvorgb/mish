module Main where

import           MissionGenerator.Config
import           MissionGenerator.HexagonalGrid
import           MissionGenerator.MissionGenerator
import           System.Random

main :: IO ()
main =
  do seed <- newStdGen
     prettyPrint $ generateMission defaultConfig seed
