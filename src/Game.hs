module Game where

import Data.List.Split (chunksOf)
import Data.Function ((&))
import Lens.Micro
import Brick
import Brick.BChan
import Control.Concurrent 
import Control.Concurrent.STM

data Cell
  = Occ Int
  | Empty
  deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data GobangEvent = 
  Countdown | Placement (Int, Int)

data TimerStatus = ON | OFF

data Game = Game 
  { board :: Board
  , focusPos :: (Int, Int)
  , player :: Int
  , tictoc :: Int -- TODO: timer
  , timeLimit :: Int
  , timerStatus :: TVar TimerStatus
  } 

mkGame :: [Int] -> Int -> Int -> TVar TimerStatus -> Game
mkGame ib p t s = Game 
  { 
    board = chunksOf 9 $ mkCell <$> ib
  , focusPos = (4, 4)
  , player = p
  , tictoc = t
  , timeLimit = t
  , timerStatus = s
  }
  where
    mkCell 0 = Empty
    mkCell i = Occ i

data CursorDirection
  = North
  | South
  | East
  | West

moveCursor :: CursorDirection -> Game -> Game
moveCursor d g = 
  move d (focusPos g) 
  & (\p -> g { focusPos = p })
  where
    move :: CursorDirection -> (Int, Int) -> (Int, Int)
    move North (x, y) = (x, check $ y - 1)
    move South (x, y) = (x, check $ y + 1)
    move East  (x, y) = (check $ x + 1, y)
    move West  (x, y) = (check $ x - 1, y)
    check x | x < 0     = 0
            | x > 8     = 8
            | otherwise = x

switchPlayer :: Game -> Game
switchPlayer game = game { player = 1 - p }
  where p = player game

placePiece :: Game -> Game
placePiece game = game { board = board game & ix y . ix x .~ p }
  where
    (x, y) = focusPos game
    p = Occ $ player game

timerUpdate :: Int -> Game -> Game
timerUpdate c game = game { tictoc = c }

turnOnTimer :: Game -> IO ()
turnOnTimer game = do
  atomically $ writeTVar (timerStatus game) ON

turnOffTimer :: Game -> IO ()
turnOffTimer game = do
  atomically $ writeTVar (timerStatus game) OFF


tictocThread :: TVar TimerStatus -> BChan GobangEvent -> IO ()
tictocThread status chan = do
  s <- atomically $ readTVar status
  case s of
    ON -> do 
      threadDelay 1000000
      writeBChan chan Countdown
    _ -> do
      threadDelay 1000000