module Game.Types where

import qualified Data.Vector as V

data Stone = Black | White | Empty
  deriving (Eq, Show)

data Move = Place Int Int | Pass | Resign
  deriving (Eq, Show)

type Board = V.Vector Stone

data GameState = GameState
  { board :: Board
  , boardSize :: Int
  , currentPlayer :: Stone
  , moveNumber :: Int       -- New field for tracking move number
  , capturedBlack :: Int
  , capturedWhite :: Int
  } deriving (Show)