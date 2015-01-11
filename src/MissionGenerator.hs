module MissionGenerator(generateMission) where

import qualified Data.Array as A
import qualified Data.Array.ST as MA

type Dimensions = (Int, Int)
type Position   = (Int, Int)
type Seed       = Int
type Mission    = A.Array Position Cell

data Cell = Cell Tile (Maybe Enemy) (Maybe Light)
          deriving Eq

data Tile = Floor
          | Wall
          | Door
          | Water
            deriving Eq

data Enemy = Enemy Position Strength
           deriving Eq

data Strength = Weak
              | Normal
              | Strong
              | Boss
                deriving Eq

data Light = Position
           deriving Eq

generateMission :: Dimensions -> Seed -> Mission
generateMission (width, height) seed = A.array ((0,0), (width-1, height-1)) $ zip indices $ repeat $ Cell Floor Nothing Nothing
    where indices = [(row,col) | col <- [0..width-1], row <- [0..height-1]]
