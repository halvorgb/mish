module MissionGenerator.Types where

import qualified Data.Array as A

type Dimensions = (Int, Int)
type Position   = (Int, Int)
type Seed       = Int
type Mission    = A.Array Position Cell

data Cell = Cell Tile (Maybe Enemy) (Maybe Light)
          deriving Eq

instance Show Cell where
  show (Cell Floor _ _) = " "
  show (Cell Wall _ _)  = "#"
  show (Cell Door _ _)  = "D"
  show (Cell Water _ _) = "~"
  show (Cell Void _ _)  = "*"


data Tile = Floor
          | Wall
          | Door
          | Water
          | Void
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
