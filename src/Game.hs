module Game where

import Data.List.Split (chunksOf)
import Data.Function ((&))
import Lens.Micro

data Cell
  = Occ Int
  | Empty
  deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data Game = Game 
  { board :: Board
  , focusPos :: (Int, Int)
  , tictoc :: Int -- TODO: timer
  } deriving (Show)

mkGame :: [Int] -> Int -> Game
mkGame ib c = Game 
  { 
    board = chunksOf 9 $ mkCell <$> ib
  , focusPos = (4, 4)
  , tictoc = c
  }
  where
    mkCell 0 = Empty
    mkCell i = Occ i

data Direction
  = North
  | South
  | East
  | West

moveCursor :: Direction -> Game -> Game
moveCursor d g = 
  move d (focusPos g) 
  & (\p -> g { focusPos = p })
  where
    move :: Direction -> (Int, Int) -> (Int, Int)
    move North (x, y) = (x, check $ y - 1)
    move South (x, y) = (x, check $ y + 1)
    move East  (x, y) = (check $ x + 1, y)
    move West  (x, y) = (check $ x - 1, y)
    check x | x < 0     = 0
            | x > 8     = 8
            | otherwise = x
