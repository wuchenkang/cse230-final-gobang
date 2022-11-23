module Game where

import Data.List.Split (chunksOf)

data Cell
  = Occ Int
  | Empty
  deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data Game = Game {
  board :: Board,
  tictoc :: Int -- TODO: timer
} deriving (Show)

mkGame :: [Int] -> Int -> Game
mkGame ib c = Game { 
  board = chunksOf 9 $ mkCell <$> ib, 
  tictoc = c
  }
  where
    mkCell 0 = Empty
    mkCell i = Occ i
