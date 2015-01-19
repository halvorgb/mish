module Main where

import           MissionGenerator.Config
import           MissionGenerator.MissionGenerator
-- import           MissionGenerator.Types
import System.Random

main :: IO ()
main =
  do seed <- newStdGen
     putStrLn $ prettyMission $ generateMission defaultConfig seed
